library(tidyverse)

source('analysis_config.R')
source('./simulation_analysis/vi_simulation_tools.R')

roi_cover = read_csv(random_roi_percent_cover_file) 

nort_ndvi = read_csv(random_roi_ndvi_file) %>%
  filter(site_id %in% c('NORT')) %>% 
  filter(date >= '2019-01-01', date<='2020-01-31') %>%
  mutate(doy = lubridate::yday(date)) %>%
  rename(pixel_size_check = pixel_size) %>%
  left_join(roi_cover, by=c('roi_id','site_id'))


# drop any roi dates where there are >5% NA pixels.
# this happens to ROI's near the edge of the site when
# some flight dates have a smaller than normal boundery
nort_ndvi = nort_ndvi %>%
  filter(percent_na<0.05)

# remove any ROI's without the full year, these are near
# the edge of the site and overlapp the bounderies on some dates
# currently 9 dates at p9 and 7 at nort for 2019, if more years/sites are
# added this will have to be adjusted. 
nort_ndvi = nort_ndvi %>%
  filter(!is.na(ndvi)) %>%
  group_by(roi_id) %>%
  filter(n() >=7 ) %>%
  ungroup()

# some quick sanity checks
if(any(is.na(nort_ndvi$pixel_size))) stop('pixel sizes not lining up in nort_ndvi')
if(!all(nort_ndvi$pixel_size == nort_ndvi$pixel_size_check)) stop('pixel sizes not lining up in nort_ndvi')

# An exploratory figure
# nort_ndvi %>%
# ggplot(aes(x=date, y=ndvi_sd)) +
#   geom_line(aes(group=as.factor(roi_id))) +
#   #geom_point() +
#   geom_smooth(se=F, color='red') +
#   scale_x_date(breaks = unique(nort_ndvi$date)) +
#   scale_color_viridis_c() +
#   facet_wrap(site_id~pixel_size, ncol=1) +
#   theme(legend.position = 'none',
#         axis.text.x = element_text(angle=45, hjust=1))

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
  #extract_phenology(select(roi_ndvi, doy, vi = ndvi), percent_threshold=c(0.1), to_return='plot')
  
  phenology$roi_id = this_roi_id
 
  all_phenology = all_phenology %>%
    bind_rows(phenology)
}

pixel_sizes = nort_ndvi %>%
  distinct(roi_id, pixel_size)

all_phenology2 = all_phenology %>%
  #left_join(pixel_sizes, by='roi_id') %>%
  left_join(roi_cover, by='roi_id')

all_phenology2 %>%
  filter(threshold %in% c(0.1)) %>%
  filter(pixel_size %in% c(2,4,8,16)) %>%
  mutate(meets_threshold = qa==0) %>%
  #mutate(cover_bin = ceiling(cover*10*2)/2/10) %>% # round to the nearest 0.05
  mutate(mesquite_cover_bin = round(mesquite,2)) %>%
  group_by(site_id,pixel_size,mesquite_cover_bin) %>%
  summarise(percent_meeting_threshold = mean(meets_threshold), n=n()) %>%
  ungroup() %>%
ggplot(aes(x=mesquite_cover_bin, y=percent_meeting_threshold, color=as.factor(pixel_size))) + 
  #geom_point(size=5) +
  geom_line(size=1) +
  scale_color_manual(values = c('#000000','#e69f00','#56b4e9','#d55e00')) + 
  scale_x_continuous(breaks=seq(0,1,0.2), labels = function(x){paste0(x*100,'%')}) +
  scale_y_continuous(breaks=seq(0,1,0.2), labels = function(x){paste0(x*100,'%')}) +
  theme_bw() +
  theme(legend.position = c(0.8,0.5),
        legend.background = element_rect(color='black'),
        legend.key.width = unit(20,'mm'),
        legend.title = element_text(size=18),
        legend.text = element_text(size=15),
        axis.text = element_text(color='black', size=15),
        axis.title = element_text(size=18)) + 
  labs(x='Mesquite Fractional Cover', 
       y='Percent of Drone Pixels with\nannual NDVI amplitude > 0.1',
       color = 'Pixel Size (m)') +
  guides(color = guide_legend(reverse=T, override.aes = list(size=3)))

#----------------------------------
# mean average error (MAE) of estimates. from calculate_NORT_observed_phenology.R
true_phenology = tribble(
  ~sos, ~peak, ~eos, ~threshold, ~method,
  108,  241,   314,  NA,         'max_change_rate',
  109,  241,   328,  0.25,       'percent_max_threshold',
  102,  241,   345,  0.10,       'percent_max_threshold') %>%
  pivot_longer(c(-threshold,-method), names_to='metric', values_to='true_doy')

method_levels = c('percent_max_threshold','max_change_rate')
method_labels = c('10% of relative max','Maximum rate of change')

all_phenology2 %>%
  filter(threshold %in% c(0.1, NA)) %>%
  filter(pixel_size %in% c(2,4,8,16)) %>%
  select(peak, sos, eos, roi_id, soil, mesquite, pixel_size, threshold, method) %>%
  mutate(sos = ifelse(is.infinite(sos), 1, sos),
         eos = ifelse(is.infinite(eos),365, eos)) %>% 
  pivot_longer(c(peak, sos, eos), names_to='metric', values_to='doy') %>% 
  left_join(true_phenology, by=c('method','metric','threshold')) %>%
  mutate(mesquite_cover_bin = round(mesquite,2)) %>%
  group_by(mesquite_cover_bin, pixel_size, threshold, method, metric) %>%
  summarise(mae = mean(abs(doy - true_doy))) %>%
  ungroup() %>% 
  mutate(metric = factor(metric, levels=c('sos','peak','eos'), labels=c('SOS','Peak','EOS'), ordered = T)) %>%
  mutate(method = factor(method, levels = method_levels, labels=method_labels)) %>%
  ggplot(aes(x=mesquite_cover_bin, y=mae, color=as.factor(pixel_size))) + 
  geom_line(size=1) +
  scale_color_manual(values = c('#000000','#e69f00','#56b4e9','#d55e00')) + 
  scale_y_continuous(breaks=seq(0,100,20)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), labels = function(x){paste0(x*100,'%')}) +
  coord_cartesian(ylim=c(0,100))  +
  facet_grid(metric~method) + 
  theme_bw() + 
  theme(legend.position = c(0.35,0.22),
        legend.background = element_rect(color='black'),
        legend.key.width = unit(20,'mm'),
        legend.title = element_text(size=18),
        legend.text = element_text(size=15),
        #panel.grid.major = element_line(color='grey50', size=0.4),
        #panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size=16),
        strip.text.y = element_text(size=18,face='bold'),
        axis.text = element_text(color='black', size=15),
        axis.text.x = element_text(size=14),
        axis.title = element_text(size=18)) +
  labs(x='Mesquite Fractional Cover', y='Mean Absolute Error of Estimates', color='Pixel Size (m)') +
  guides(color = guide_legend(reverse=T, override.aes = list(size=3)))


#----------------------------------
# Supplemental Figure showing examples of different UAV  pixl annual curves.
mesquite_cover_to_highlight = c(0.2,0.4, 0.6, 0.8)

example_rois = nort_ndvi %>%
  filter(pixel_size %in% c(2,4,8,16)) %>%
  mutate(mesquite_cover = round(mesquite,2)) %>% 
  filter(mesquite_cover %in% mesquite_cover_to_highlight) %>%
  select(doy, vi=ndvi, roi_id, mesquite_cover, pixel_size) %>%
  mutate(mesquite_cover = paste0(mesquite_cover*100,'%'))

# example_roi_phenology = example_rois %>%
#   group_by(roi_id, mesquite_cover) %>%
#   summarise(extract_phenology(., percent_threshold = c(0.1))) %>%
#   ungroup() %>%
#   group_by(mesquite_cover) %>%
#   summarise(SOS = mean(sos),
#             EOS = mean(eos),
#             Peak = mean(peak)) %>%
#   ungroup() %>%
#   pivot_longer(c(SOS,EOS,Peak), names_to='metric',values_to='doy')

# The number of pixels in this figure
n_distinct(example_rois$roi_id)

# The maximum amplitue seen, which should approximate the canopy VI_veg amplitude.
nort_ndvi %>% 
  group_by(roi_id, mesquite, pixel_size) %>% 
  summarise(ndvi_amplitude = max(ndvi) - min(ndvi)) %>%
  ungroup() %>%
  arrange(-ndvi_amplitude)

colors = c('#d8b365','#8c510a','#80cdc1','#003c30')

example_rois %>%
  mutate(pixel_size_label = paste0('Pixel Size: ',pixel_size,'m')) %>% 
  mutate(pixel_size_label = fct_reorder(pixel_size_label, pixel_size)) %>%
ggplot(aes(x=doy, y=vi, color=as.factor(mesquite_cover), group=roi_id)) + 
  geom_smooth(method='loess', se=F) +
  geom_point(aes(fill=as.factor(mesquite_cover)),color='black',stroke=1,size=2, shape=21, show.legend = F) +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  facet_wrap(~pixel_size_label) +
  theme_bw(25) +
  theme(legend.position = 'right',
        legend.background = element_rect(color='black'),
        legend.title = element_text(size=18),
        axis.text = element_text(color='black')) +
  labs(x='Day of Year', y='NDVI', color='Mesquite Fractional\nCover') +
  guides(color=guide_legend(reverse = T, override.aes = list(size=4)))
