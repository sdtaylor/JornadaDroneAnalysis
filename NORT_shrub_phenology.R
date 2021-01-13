library(tidyverse)

source('analysis_config.R')
source('ndvi_simulation_tools.R')

roi_cover = read_csv('data/random_roi_percent_cover.csv') 

nort_ndvi = read_csv('data/random_roi_ndvi.csv') %>%
  filter(site_id %in% c('P9')) %>%
  filter(scale %in% c(8)) %>%
  filter(date >= '2019-01-01', date<='2020-03-31') %>%
  mutate(doy = lubridate::yday(date)) %>%
  left_join(roi_cover, by=c('roi_id','site_id'))

nort_ndvi %>%
  group_by(roi_id, scale) %>%
  summarise(amplitude = max(ndvi) - min(ndvi)) %>%
  ungroup() %>%
  ggplot(aes(amplitude)) +
  geom_histogram(bins = 20) +
  facet_wrap(~scale, ncol=1)

ggplot(nort_ndvi, aes(x=date, y=ndvi, color=soil, group=as.factor(roi_id))) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = unique(nort_ndvi$date)) + 
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
    1,    first_ndvi,
    390,  last_ndvi,
    420,  last_ndvi,
    450,  last_ndvi
  )
  
  roi_ndvi = roi_ndvi %>%
    bind_rows(buffer_ndvi)
  
  # get phenology
  phenology = extract_phenology(select(roi_ndvi, doy, vi = ndvi))
  # extract_phenology(select(roi_ndvi, doy, vi = ndvi), to_return='plot')
  
  phenology$roi_id = this_roi_id
 
  all_phenology = all_phenology %>%
    bind_rows(phenology)
}

all_phenology = all_phenology %>%
  left_join(roi_cover, by='roi_id')

all_phenology %>%
  filter(threshold==0.1) %>%
  mutate(meets_threshold = qa==0) %>%
  mutate(mesquite_cover_bin = ceiling(mesquite*10*2)/2/10) %>% # round to the nearest 0.05
  group_by(mesquite_cover_bin) %>%
  summarise(percent_meeting_threshold = mean(meets_threshold)) %>%
  ungroup() %>%
ggplot(aes(x=mesquite_cover_bin, y=percent_meeting_threshold)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6)) + 
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=12),
        axis.title = element_text(size=14)) + 
  labs(x='Mesquite Percent Cover', y='Percent of 8m pixels where \namplitude > 0.1')
