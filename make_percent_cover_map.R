library(tidyverse)
library(stars)
library(sf)
library(patchwork)


ecoregions = st_read('./data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>%
  filter(NA_L1CODE == 10) %>%
  filter(NA_L3NAME != 'Chihuahuan Deserts') %>%     # This is a small chunk of the CH desert in AZ. 
  mutate(NA_L3NAME = recode(NA_L3NAME, 'Arizona/New Mexico Plateau'= 'Arizona-NM Plateau',
                            'Sonoran Basin and Range' = 'Sonoran Desert')) %>%
  group_by(NA_L3NAME) %>%
  summarize() %>%
  ungroup() %>%
  st_union(by_feature = T) %>%
  st_buffer(0.01)

# only north american deserts shapefile for us in qgis
st_write(ecoregions, './data/gis/desert_ecoregions/NA_Desert_ecoregions.shp')

# Create some cropped rasters for use in QGIS
mean_cover_raster = read_stars('./data/rap_cover/RAP_average_cover_2000-2019.tif')
std_cover_raster  = read_stars('./data/rap_cover/RAP_std_cover_2000-2019.tif')

ecoregions = st_transform(ecoregions, crs=st_crs(mean_cover_raster))

mean_cover_raster = st_crop(mean_cover_raster, ecoregions)
std_cover_raster = st_crop(std_cover_raster, ecoregions)

write_stars(mean_cover_raster,'./data/rap_cover/RAP_average_cover_2000-2019_cropped.tif')
write_stars(std_cover_raster, './data/rap_cover/RAP_std_cover_2000-2019_cropped.tif')



for(this_ecoregion in unique(as.character(ecoregions$NA_L3NAME))){
  e = ecoregions %>%
    filter(NA_L3NAME==this_ecoregion)
  
  # Instead of making histograms for every pixel within each ecoregion
  # just sample a large number. 
  random_points = st_sample(e, size=50000)
  
  #---------------
  mean_cover = st_extract(mean_cover_raster, random_points) %>%
    st_set_geometry(NULL)
  colnames(mean_cover) <- 'value'

  mean_cover_fig = ggplot(mean_cover, aes(value)) +
    geom_density(size=3, color='#009e73') +
    scale_x_continuous(breaks=c(0,20,40,60,80,100), labels = function(x){paste0(x,'%')}, limits=c(0,100)) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color='black',size=12),
          axis.title  = element_text(size=18),
          plot.title = element_text(size=21)) +
    labs(x='Fractional Cover',y='Density', title=this_ecoregion)

  ggsave(paste0('./manuscript/map_figure/',this_ecoregion,'_mean_cover.png'), mean_cover_fig, width=10, height=8, units='cm')
  
  #---------------
  std_cover = st_extract(std_cover_raster, random_points) %>%
    st_set_geometry(NULL)
  colnames(std_cover) <- 'value'

  mean_cover_fig = ggplot(std_cover, aes(value)) +
    geom_density(size=3, color='#d55e00') +
    scale_x_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color='black',size=12),
          axis.title.y  = element_text(size=18),
          axis.title.x  = element_text(size=14),
          plot.title = element_text(size=21)) +
    labs(x='Annual Variation in Fractional Cover',y='Density', title=this_ecoregion)

  ggsave(paste0('./manuscript/map_figure/',this_ecoregion,'_std_cover.png'), mean_cover_fig, width=10, height=8, units='cm')
  
}