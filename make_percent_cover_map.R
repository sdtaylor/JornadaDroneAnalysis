library(tidyverse)
library(stars)
library(sf)
library(patchwork)



mean_cover_raster = read_stars('./data/rap_cover/RAP_average_cover_2000-2019.tif')
std_cover_raster  = read_stars('./data/rap_cover/RAP_std_cover_2000-2019.tif')

ecoregions = st_read('./data/gis/NA_Desert_ecoregions.geojson')

ecoregions = st_transform(ecoregions, crs=st_crs(mean_cover_raster))


legend_guide = guide_colorsteps(even.steps = F,
                                label.theme = element_text(size=12),
                                barwidth = unit(80,'mm'),
                                #title = 'Average Fractional Cover 2000-2019',
                                title.position='top',
                                #title.theme = element_text(size=18),
                                title.theme = element_blank(),
                                direction='horizontal')

map_theme = theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(color='black', fill='white',size=1),
        plot.title = element_text(size=18),
        legend.background = element_rect(color='black',fill='white'),
        legend.position = c(0.25,0.15))

#--------------------------------
mean_color_palette = c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')

mean_map = ggplot() + 
  geom_stars(data=mean_cover_raster,downsample=0) +
  geom_sf(data=ecoregions, fill='transparent',color='black') +
  scale_fill_stepsn(colors = mean_color_palette, 
                    breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                    limits = c(0,100),
                    na.value='white',
                    guide =  legend_guide)+ 
  coord_sf(xlim=c(-125,-106),ylim=c(31,42)) +
  labs(title = 'A. Average Fractional Cover 2000-2019') +
  map_theme
  

#--------------------------------
std_color_palette = c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')

std_map = ggplot() + 
  geom_stars(data=std_cover_raster,downsample=0, na.rm=T) +
  geom_sf(data=ecoregions, fill='transparent',color='black') +
  scale_fill_stepsn(colors = std_color_palette, 
                    breaks = c(0,5,10,15,20),
                    labels = c('0','5','10','15','20+'),
                    limits = c(0,25),
                    na.value='white',
                    guide = legend_guide) + 
  coord_sf(xlim=c(-125,-106),ylim=c(31,42)) +
  labs(title = 'B. Annual Variation in Fractional Cover') +
  map_theme 

#-------------------------------
both = mean_map + std_map

ggsave('./manuscript/avg_cover_figure.png',both,width=45,height=18,units='cm', dpi=150)
