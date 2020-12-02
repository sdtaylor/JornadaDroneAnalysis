library(tidyverse)


plant_peak_ndvi   = 0.6
plant_winter_ndvi = 0.2
plant_ndvi_sd     = 0.04

soil_ndvi = 0.1
soil_sd   = 0.01

n_bootstraps = 100

###############
# generate all combinations of soil/plant cover
cover_step = 5
cover_values = tibble(plant=numeric(),soil=numeric())
for(plant_cover in seq(0,100,cover_step)){
  soil_cover = 100 - plant_cover
  cover_values = cover_values %>%
    add_row(plant=plant_cover, soil=soil_cover)
}

cover_values = filter(cover_values, plant!=0)
cover_values = add_row(cover_values, plant=1,soil=99)

cover_values$plant = cover_values$plant/100
cover_values$soil  = cover_values$soil/100

final_phenology_results = tibble()
##############
for(cover_i in 1:nrow(cover_values)){
  plant_cover = tribble(
    ~plant, ~percent_cover,
    'plant', cover_values$plant[cover_i],
    'soil',  cover_values$soil[cover_i]
  )
  
  for(i in 1:n_bootstraps){
    doy = seq(-60,450,8)
    
    df = tibble(doy = doy)
    # double logistic values to produce, with 0.1 threshold
    # sos: 82, eos 292, season length: 210
    df$plant = elmore_double_sigmoid(doy, 
                                     m1 = 0.2,
                                     m2 = 0.5,
                                     m3 = 100,
                                     m4 = 8,
                                     m5 = 280, 
                                     m6 = 8,
                                     m7 = 0.001)
    # add random noise
    #df$plant = df$plant + rnorm(length(doy), mean=0, sd=plant_ndvi_sd)
    
    # soil ndvi is a constant with noise
    #df$soil = rnorm(length(doy), mean = soil_ndvi, sd = soil_sd)
    df$soil = soil_ndvi
    
    scaled_ndvi = df %>%
      pivot_longer(-doy, names_to='plant', values_to='ndvi') %>%
      left_join(plant_cover, by='plant') %>%
      mutate(scaled_ndvi = ndvi * percent_cover) %>%
      group_by(doy) %>%
      summarise(vi = sum(scaled_ndvi)) %>%
      ungroup()
    
    # add random noise
    scaled_ndvi$vi = scaled_ndvi$vi + rnorm(nrow(scaled_ndvi), mean=0, sd=0.02)
    
    extract_phenology(scaled_ndvi, to_return = 'plot')
    
    phenology = extract_phenology(scaled_ndvi)
    
    phenology$plant_cover =  cover_values$plant[cover_i]
    phenology$bootstrap = i
    
    final_phenology_results = final_phenology_results %>%
      bind_rows(phenology)
  }
}


# just looking at whether it meets 0.1 amplitude rise
final_phenology_results %>%
  filter(threshold==0.1) %>% 
  group_by(plant_cover) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa)) %>%
  ungroup() %>%
  ggplot(aes(x=plant_cover, y=percent_meeting_amplitude)) +
  geom_point()


final_phenology_results %>%
  pivot_longer(c(peak, sos, eos, season_length), names_to='metric', values_to='value') %>%
  ggplot(aes(x=as.factor(plant_cover), y=value)) + 
  geom_jitter(alpha=0.2) + 
  geom_vline(xintercept = as.factor(0.3)) + 
  facet_wrap(metric~threshold, ncol=3)



