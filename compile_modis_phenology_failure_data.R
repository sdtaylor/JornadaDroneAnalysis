library(tidyverse)
library(sf)
library(raster)


# constraint to L1 ecoregions N.A. Deserts, Med California, S. Semi-arid highlands, & Temperate Sierras
l3_ecoregions = sf::st_read('data/l3_ecoregions/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>%
  filter(NA_L1CODE %in% c('10','11','12','13'))


mcd12q2_percent_failed = raster::raster('~/data/jornada_drone_imagery/mcd12q2_percent_missing_values_500m.tif')
rap_cover              = raster::brick('~/data/jornada_drone_imagery/RAP_average_cover_2001-2019.tif')

# generate random points to query

random_point_count = 10000
set.seed(1)

random_points = sf::st_sample(l3_ecoregions, size=random_point_count)
random_points = sf::st_sf(geometry = random_points)
random_points$point_id = 1:random_point_count

# assign ecoregions
rp_ecoregions = st_intersection(l3_ecoregions, random_points) 
random_points$L3_ecoregion = rp_ecoregions$NA_L3NAME
random_points$L2_ecoregion = rp_ecoregions$NA_L2NAME

# get phenology and cover values
random_points$mcd12_percent_failed = raster::extract(mcd12q2_percent_failed, random_points)

rp_rap = raster::extract(rap_cover, random_points) %>%
  as_tibble()
random_points = random_points %>%
  bind_cols(rp_rap)

# convert to a csv and save
random_points %>%
  st_transform(crs=4326) %>%
  cbind(., st_coordinates(.)) %>%
  rename(lon = X, lat=Y) %>%
  st_set_geometry(NULL) %>% 
  write_csv('data/mcd12q2_phenology_failure_data.csv')
