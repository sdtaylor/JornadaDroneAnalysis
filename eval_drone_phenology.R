library(tidyverse)

source('analysis_config.R')
source('ndvi_simulation_tools.R')

roi_cover = read_csv('data/random_roi_percent_cover.csv') 

nort_ndvi = read_csv(random_roi_ndvi_file) %>%
  filter(site_id %in% c('NORT')) %>% 
  filter(scale %in% c(8)) %>%
  filter(date >= '2019-01-01', date<='2020-01-31') %>%
  mutate(doy = lubridate::yday(date)) %>%
  left_join(roi_cover, by=c('roi_id','site_id'))

# drop any roi dates where there are >10% NA pixels.
# this happens to ROI's near the edge of the site when
# some flight dates have a smaller than normal boundery
nort_ndvi = nort_ndvi %>%
  filter(percent_na<0.1)

# remove any ROI's without the full year, these are near
# the edge of the site and overlapp the bounderies on some dates
# currently 9 dates at p9 and 7 at nort for 2019, if more years/sites are
# added this will have to be adjusted. 
nort_ndvi = nort_ndvi %>%
  filter(!is.na(ndvi)) %>%
  group_by(roi_id) %>%
  filter(n() >=7 ) %>%
  ungroup()
  
nort_ndvi %>%
ggplot(aes(x=date, y=ndvi)) +
  geom_line(aes(color=grass, group=as.factor(roi_id))) +
  #geom_point() +
  geom_smooth(se=F, color='red') + 
  scale_x_date(breaks = unique(nort_ndvi$date)) + 
  scale_color_viridis_c() + 
  facet_wrap(site_id~scale, ncol=1) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust=1))

all_phenology = tibble()
for(this_roi_id in unique(nort_ndvi$roi_id)){
  roi_ndvi = nort_ndvi %>%
    filter(roi_id == this_roi_id) %>%
    arrange(doy)
  
  # skip any with missing NDVI due to being out of bounds in some of the imagery
  if(any(is.na(roi_ndvi$ndvi))) next
  
  # Buffer the ndvi in the spring and fall using the last known values 
  first_ndvi = roi_ndvi$ndvi[1]
  last_ndvi  = roi_ndvi$ndvi[nrow(roi_ndvi)]
  
  buffer_ndvi = tribble(
    ~doy, ~ndvi,
    -60,  first_ndvi,
    -30,  first_ndvi,
    1,    first_ndvi,
    390,  last_ndvi,
    420,  last_ndvi,
    450,  last_ndvi
  )
  
  roi_ndvi = roi_ndvi %>%
    bind_rows(buffer_ndvi)
  
  # get phenology
  phenology = extract_phenology(select(roi_ndvi, doy, vi = ndvi))
  extract_phenology(select(roi_ndvi, doy, vi = ndvi), percent_threshold=c(0.1), to_return='plot')
  
  phenology$roi_id = this_roi_id
 
  all_phenology = all_phenology %>%
    bind_rows(phenology)
}

all_phenology = all_phenology %>%
  left_join(roi_cover, by='roi_id')

all_phenology %>%
  filter(threshold==0.1) %>%
  #filter(grass<0.1) %>%
  #select(-grass) %>% 
  #pivot_longer(c('soil','total_veg'), names_to='plant',values_to='cover') %>% 
  #pivot_longer(c('total_veg','soil','mesquite'), names_to='plant',values_to='cover') %>% 
  mutate(meets_threshold = qa==0) %>%
  #mutate(cover_bin = ceiling(cover*10*2)/2/10) %>% # round to the nearest 0.05
  mutate(mesquite_cover_bin = round(mesquite,2)) %>%
  group_by(site_id,mesquite_cover_bin) %>%
  summarise(percent_meeting_threshold = mean(meets_threshold), n=n()) %>%
  ungroup() %>%
ggplot(aes(x=mesquite_cover_bin, y=percent_meeting_threshold)) + 
  #geom_point(size=5) +
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  #facet_wrap(~site_id) + 
  theme_bw() +
  theme(legend.position = 'right',
        axis.text = element_text(color='black', size=15),
        axis.title = element_text(size=18)) + 
  labs(x='Mesquite Fractional Cover', y='Percent of Drone Pixels with\nannual NDVI amplitude > 0.1')

#----------------------------------
# mean average error (MAE) of estimates
true_phenology = data.frame(sos=104, peak=257, eos=344) %>%
  pivot_longer(everything(), names_to='metric', values_to='true_doy')


all_phenology %>%
  filter(threshold==0.1) %>%
  select(peak, sos, eos, roi_id, soil, grass, mesquite) %>%
  mutate(sos = ifelse(is.infinite(sos), 1, sos),
         eos = ifelse(is.infinite(eos),365, eos)) %>% 
  pivot_longer(c(peak, sos, eos), names_to='metric', values_to='doy') %>%
  left_join(true_phenology, by='metric') %>%
  mutate(mesquite_cover_bin = round(mesquite,2)) %>%
  group_by(mesquite_cover_bin, metric) %>%
  summarise(mae = mean(abs(doy - true_doy))) %>%
  ggplot(aes(x=mesquite_cover_bin, y=mae, color=metric)) + 
  geom_line(size=3) +
  scale_color_brewer(palette = 'Dark2') +
  scale_y_continuous(breaks=seq(0,100,20)) + 
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  coord_cartesian(ylim=c(0,100))  +
  theme_bw(20) + 
  theme(legend.position = c(0.8,0.45),
        legend.background = element_rect(color='black'),
        panel.grid.major = element_line(color='grey50', size=0.4),
        panel.grid.minor = element_line(color='grey90', size=0.5),
        axis.text = element_text(color='black')) +
  labs(x='Mesquite Percent Cover', y='Mean Absolute Error\nof Estimates', color='Metric')


#----------------------------------
# 4 example Nort NDVI curves
mesquite_cover_to_highlight = c(0.2,0.4, 0.6, 0.8)

example_rois = nort_ndvi %>%
  mutate(mesquite_cover = round(mesquite,2)) %>% 
  filter(mesquite_cover %in% mesquite_cover_to_highlight) %>%
  select(doy, vi=ndvi, roi_id, mesquite_cover) 

example_roi_phenology = example_rois %>%
  group_by(roi_id, mesquite_cover) %>%
  summarise(extract_phenology(., percent_threshold = c(0.1))) %>%
  ungroup() %>%
  group_by(mesquite_cover) %>%
  summarise(SOS = mean(sos),
            EOS = mean(eos),
            Peak = mean(peak)) %>%
  ungroup() %>%
  pivot_longer(c(SOS,EOS,Peak), names_to='metric',values_to='doy')

n_distinct(example_rois$roi_id)

ggplot(example_rois, aes(x=doy, y=vi, group=roi_id, color=as.factor(mesquite_cover))) + 
  geom_smooth(method='loess', se=F) +
  geom_point(aes(fill=as.factor(mesquite_cover)),color='black',stroke=1,size=2, shape=21, show.legend = F) +
  scale_color_viridis_d(end=0.9, option='E') + 
  scale_fill_viridis_d(end=0.9, option='E') + 
  ylim(0.05,0.5) +
  theme_bw(25) +
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(color='black'),
        legend.title = element_text(size=18),
        axis.text = element_text(color='black')) +
  labs(x='Day of Year', y='NDVI', color='ROI Mesquite\nCover') +
  guides(color=guide_legend(reverse = T, override.aes = list(size=4)))


