library(tidyverse)

source('ndvi_simulation_tools.R')


plant_cover = tribble(
  ~plant, ~percent_cover,
  'shrub', 0.6,
  'grass', 0,
  'soil',  0.4
)

plant_ndvi = data.frame(doy = 1:365,
                        soil = random_soil_ndvi(),
                        shrub = random_shrub_ndvi(),
                        grass = random_grass_ndvi()) %>%
  pivot_longer(-doy, names_to='plant', values_to='ndvi')

full_ndvi = plant_ndvi %>%
  left_join(plant_cover, by='plant') %>%
  mutate(scaled_ndvi = ndvi*percent_cover) %>%
  group_by(doy) %>%
  summarise(ndvi = sum(scaled_ndvi), n=n()) %>%
  ungroup()

#-----------------------------------------
potential_grass_cover = seq(0.05,1, 0.05)
bootstrap_iterations = 10

final_peak_info = data.frame()

for(grass_cover in potential_grass_cover){
  plant_cover = tribble(
    ~plant, ~percent_cover,
    'grass', grass_cover,
    'soil',  1 - grass_cover
  )
  for(bootstrap_i in 1:bootstrap_iterations){
    plant_ndvi = data.frame(doy = 1:365,
                            soil = random_soil_ndvi(),
                            grass = random_grass_ndvi()) %>%
      pivot_longer(-doy, names_to='plant', values_to='ndvi') %>%
      left_join(plant_cover, by='plant')
    
    final_ndvi = plant_ndvi %>%
      mutate(scaled_ndvi = ndvi*percent_cover) %>%
      group_by(doy) %>%
      summarise(ndvi = sum(scaled_ndvi), n=n()) %>%
      ungroup()
    
    peak_info = find_peaks(final_ndvi)
    peak_info$grass_cover = grass_cover
    peak_info$bootstrap_i = bootstrap_i
    
    final_peak_info = final_peak_info %>%
      bind_rows(peak_info)
  }
}

x=final_peak_info %>%
  group_by(grass_cover) %>%
  summarise(n_potential_peaks = n(),
            n_valid_peaks     = sum(peak_valid))

ggplot(x, aes(x=grass_cover,y=n_valid_peaks)) +
  geom_point()
