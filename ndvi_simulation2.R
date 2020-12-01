library(tidyverse)


shrub_peak_ndvi   = 0.6
shrub_winter_ndvi = 0.2
shrub_ndvi_sd     = 0.04

soil_ndvi = 0.1
soil_sd   = 0.01

n_bootstraps = 100

###############
# generate all combinations of soil/shrub cover
cover_step = 5
cover_values = tibble(shrub=numeric(),soil=numeric())
for(shrub_cover in seq(0,100,cover_step)){
  soil_cover = 100 - shrub_cover
  cover_values = cover_values %>%
    add_row(shrub=shrub_cover, soil=soil_cover)
}
cover_values$shrub = cover_values$shrub/100
cover_values$soil  = cover_values$soil/100

final_phenology_results = tibble()
##############
for(cover_i in 1:nrow(cover_values)){
  plant_cover = tribble(
    ~plant, ~percent_cover,
    'shrub', cover_values$shrub[cover_i],
    'soil',  cover_values$soil[cover_i]
  )
  
  for(i in 1:n_bootstraps){
    doy = seq(-60,450,8)
    
    df = tibble(doy = doy)
    # double logistic values to produce, with 0.1 threshold
    # sos: 82, eos 292, season length: 210
    df$shrub = elmore_double_sigmoid(doy, 
                                     m1 = 0.2,
                                     m2 = 0.5,
                                     m3 = 100,
                                     m4 = 8,
                                     m5 = 280, 
                                     m6 = 8,
                                     m7 = 0.001)
    # add random noise
    df$shrub = df$shrub + rnorm(length(doy), mean=0, sd=shrub_ndvi_sd)
    
    # soil ndvi is a constant with noise
    df$soil = rnorm(length(doy), mean = soil_ndvi, sd = soil_sd)
    
    scaled_ndvi = df %>%
      pivot_longer(-doy, names_to='plant', values_to='ndvi') %>%
      left_join(plant_cover, by='plant') %>%
      mutate(scaled_ndvi = ndvi * percent_cover) %>%
      group_by(doy) %>%
      summarise(vi = sum(scaled_ndvi)) %>%
      ungroup()
    
    extract_phenology(scaled_ndvi, to_return = 'plot')
    
    phenology = extract_phenology(scaled_ndvi)
    
    phenology$shrub_cover =  cover_values$shrub[cover_i]
    phenology$bootstrap = i
    
    final_phenology_results = final_phenology_results %>%
      bind_rows(phenology)
  }
}




ggplot(final_phenology_results, aes(x=as.factor(shrub_cover), y=sos)) + 
  geom_boxplot() +
  geom_hline(yintercept = 82)
  #geom_point(aes(color=as.factor(qa)))

final_phenology_results %>%
  pivot_longer(c(peak, sos, eos, season_length), names_to='metric', values_to='value') %>%
  ggplot(aes(x=as.factor(shrub_cover), y=value, color=qa)) + 
  geom_boxplot() + 
  facet_wrap(~metric)



