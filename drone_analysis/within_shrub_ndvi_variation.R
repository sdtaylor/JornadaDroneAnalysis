library(tidyverse)

# This creates figure S2 showing the within shrub NDVI variation in the drone/UAV imagery.
# Carefull running it as it takes a lot of memory in the upsample step.

nort_shrubs = sf::st_read('./drone_analysis/data/gis/NORT_shrubs.geojson') %>%
  rename(shrub_id = polygon_id)  

nort_ndvi_raster = raster::raster('drone_analysis/data/imagery/NORT_Jul5_2019_index_ndvi.tif')
nort_ndvi_raster = raster::aggregate(nort_ndvi_raster, fact=5) # upsample to ~10cm pixels


cover_values = exactextractr::exact_extract(nort_ndvi_raster, nort_shrubs, fun=NULL)


x = map2_df(cover_values, nort_shrubs$shrub_id, function(x,p){tibble(ndvi=x$value, shrub_id=p)})

set.seed(6)
random_shrubs = sample(nort_shrubs$shrub_id, 20)

mean_shrub_ndvi = x %>%
  filter(shrub_id %in% random_shrubs) %>%
  group_by(shrub_id) %>%
  summarise(mean_ndvi = mean(ndvi, na.rm=T),
            median_ndvi = median(ndvi, na.rm=T)) %>%
  ungroup()

figS2_shrub_ndvi_variation = x %>%
  filter(shrub_id %in% c(random_shrubs)) %>%
  ggplot(aes(x=ndvi)) + 
  #geom_density(fill='transparent', size=2) + 
  geom_histogram(binwidth = 0.02) + 
  geom_vline(data = mean_shrub_ndvi, aes(xintercept=mean_ndvi), size=1.5, color='#d55e00') + 
  facet_wrap(~shrub_id, labeller = label_both) + 
  theme_bw(14) + 
  theme(axis.text = element_text(color='black'),
        strip.text = element_text(size=14),
        strip.background = element_blank()) +
  labs(x='NDVI', y='Count of 10cm pixels')

ggsave('manuscript/figS2_shrub_ndvi_variation.png', plot=figS2_shrub_ndvi_variation, height=20, width=20, units='cm', dpi=150)
