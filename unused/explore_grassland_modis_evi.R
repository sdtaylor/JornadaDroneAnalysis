library(tidyverse)

# This is the 8-day 250 MODIS NDVI/EVI dataset at various grassy locations

other_site_modis = read_csv('data/other_locations_modis_ndvi.csv') %>%
  filter(SummaryQA %in% c(0,1)) %>%
  mutate(NDVI = NDVI * 0.0001,
         EVI  = EVI  * 0.0001) %>%
  mutate(year = lubridate::year(date),
         doy  = lubridate::yday(date))


ggplot(other_site_modis, aes(x=doy, y=NDVI)) +
  geom_line(aes( color=as.factor(year))) + 
  geom_point(aes( color=as.factor(year))) +
  #geom_smooth(method='gam',color='black', size=1.5) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(1,365,15)) + 
  facet_wrap(~site_id,ncol=1) +
  theme(legend.position = 'none')

# average amplitude
other_site_modis %>%
  group_by(site_id, year) %>%
  summarise(amplitude_ndvi = max(NDVI) - min(NDVI),
            amplitude_evi  = max(EVI)  - min(EVI)) %>%
  ungroup() %>%
  pivot_longer(c(amplitude_ndvi,amplitude_evi), names_to='metric', values_to='vi') %>%
  ggplot(aes(x=site_id, y=vi)) +
  geom_jitter(height=0, width=0.15) + 
  facet_wrap(~metric, ncol=2)

# NDVI Variation in winter    
other_site_modis %>% 
  group_by(site_id, doy) %>%
  summarise(ndvi_mean = mean(NDVI),
            ndvi_sd   = sd(NDVI),
            n=n()) %>%
  filter(doy<100) %>%
  View()


jornada_modis_phenology = read_csv('data/modis_phenology.csv') %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(greenup_detected = !is.na(Greenup_1)) %>%
  select(site_id, year, greenup_detected)

jornada_modis = read_csv('data/modis_ndvi.csv') %>%
  filter(SummaryQA %in% c(0,1)) %>%
  mutate(NDVI = NDVI * 0.0001,
         EVI  = EVI  * 0.0001) %>%
  mutate(year = lubridate::year(date),
         doy  = lubridate::yday(date))

jornada_modis = jornada_modis %>%
  left_join(jornada_modis_phenology, by=c('site_id','year'))

ggplot(jornada_modis, aes(x=doy, y=EVI)) +
  geom_line(aes( color=greenup_detected, group=year)) + 
  #geom_point(aes( color=as.factor(year))) +
  #geom_smooth(method='gam',color='black', size=1.5) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(1,365,15)) + 
  facet_wrap(~site_id,ncol=1) +
  theme(legend.position = 'none')



