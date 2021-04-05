library(tidyverse)

#how much each cover class contributes to pixels level NDVI

start_date = '2016-01-01'
end_date   = '2020-12-31'

# only ibp and p9 in 2019 for now.
# the 2 dates filtered out have some band issues
roi_band_data = read_csv('data/roi_ortho.csv') %>%
  select(-band_i) %>%
  filter(site_id %in% c('GIBPE','P9','NORT')) %>%
  filter(date>=start_date,date<=end_date) %>%
  filter(!((site_id=='P9' & date=='2019-10-07')|(site_id=='GIBPE' & date=='2019-10-21'))) %>%
  pivot_wider(names_from = 'band', values_from = 'value')

roi_band_data$ndvi = with(roi_band_data, (nir-red)/(nir+red))

roi_band_data = roi_band_data %>%
  filter(roi_type=='primary') %>%
  group_by(date, scale, plant, site_id) %>%
  summarise(ndvi_sd = sd(ndvi,na.rm=T),
            ndvi    = mean(ndvi, na.rm=T),
            pixel_count = n()) %>%
  ungroup()  %>%
  mutate(ndvi_high = ndvi + (ndvi_sd*1.96),
         ndvi_low  = ndvi - (ndvi_sd*1.96))


thirty_meter_ndvi = roi_band_data %>%
  filter(scale==30, plant=='none')

landsat_ndvi = read_csv('data/landsat_ndvi.csv') %>%
  select(date, site_id, landsat_pixel_id, ndvi) %>%
  filter(date>=start_date,date<=end_date) %>%
  group_by(date, site_id) %>%
  summarise(ndvi_sd = sd(ndvi),
            ndvi    = mean(ndvi),
            pixel_count = n()) %>%
  ungroup() %>%
  mutate(ndvi_high = ndvi + (ndvi_sd*1.96),
         ndvi_low  = ndvi - (ndvi_sd*1.96))

# 2018 site level cover values for shrub (mesquite), grass (graminoid), and bare soil.
# Did not actually add up to 1.0 so I'm just assuming the grass is dominant in IPB and soil 
# dominant in P9
fractional_cover = tribble(
  ~site_id, ~plant,      ~fcover,
  'GIBPE',  'mesquite',   0.08,
  'GIBPE',  'grass',      0.46,  # potentially just 0.48
  'GIBPE',  'soil',       0.46,
  'P9',     'mesquite',   0.12,
  'P9',     'grass',      0.18,
  'P9',     'soil',       0.70,  # potentially just 0.39
  'NORT',   'soil',       0.9,  # eyeballed the 2 nort ones
  'NORT',   'mesquite',   0.10,  
)

#                grass,    mesquite,  soil
color_values = c('#009E73','#0072B2','#E69F00')

roi_band_data2 = roi_band_data %>%
  filter(plant %in% c('mesquite','grass','soil')) %>%
  left_join(fractional_cover, by=c('site_id','plant')) %>%
  mutate(scaled_ndvi  = ndvi * fcover) %>%
  group_by(date, site_id) %>%
  mutate(total_ndvi = sum(scaled_ndvi)) %>%
  ungroup() %>%
  mutate(percent_ndvi = scaled_ndvi/total_ndvi)

ggplot(roi_band_data2, aes(x=date, y=scaled_ndvi)) + 
  geom_col(aes(fill=plant), width=8) +
  scale_fill_manual(values=color_values) + 
  geom_point(data=thirty_meter_ndvi, aes(y=ndvi),size=1) + 
  geom_line(data=thirty_meter_ndvi, aes(y=ndvi)) + 
  geom_errorbar(data=thirty_meter_ndvi, aes(y=ndvi,ymax = ndvi_high, ymin = ndvi_low)) + 
  #geom_point(data=landsat_ndvi, aes(y=ndvi),size=1) + 
  #geom_line(data=landsat_ndvi, aes(y=ndvi), color='red') + 
  facet_wrap(~site_id, ncol=1) +
  theme_bw(20)


ggplot(roi_band_data2, aes(x=date,y=ndvi, color=plant)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~site_id, ncol=1)


  # ----------------------------------------
# Percentage contribution to NDVI
roi_band_data2 %>%
  group_by(date, site_id) %>%
  mutate(total_ndvi = sum(scaled_ndvi)) %>%
  ungroup() %>%
  mutate(percent_ndvi = scaled_ndvi/total_ndvi)


ggplot(roi_band_data2, aes(x=plant, y=ndvi)) + 
  geom_boxplot(fill='transparent') +
  geom_jitter()
