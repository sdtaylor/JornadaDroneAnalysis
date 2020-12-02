library(tidyverse)

source('ndvi_simulation_tools.R')

# winter ndvi is generally higher than pure soil 
plant_winter_ndvi = 0.2
soil_ndvi = 0.2

n_bootstraps = 200

error_rates = c(0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
# error_rates = c(0.02)
#amplitude_rates = c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.7, 0.8, 0.9, 1.0)
amplitude_rates = seq(0.25,1.0,0.05)

###############
# generate all combinations of soil/plant cover
cover_step = 2
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
  
  for(error in error_rates){
    for(amplitude in amplitude_rates){
      for(i in 1:n_bootstraps){
        doy = seq(-60,450,8)
        
        df = tibble(doy = doy)
        # double logistic values to produce, with 0.1 threshold
        # sos: 82, eos 292, season length: 210
        df$plant = elmore_double_sigmoid(doy, 
                                         m1 = plant_winter_ndvi,
                                         m2 = amplitude, # make this range from 0.3 - 1.0. But have to record absolute difference
                                         m3 = 100,
                                         m4 = 8,
                                         m5 = 280, 
                                         m6 = 8,
                                         m7 = 0.001)
  
        # final amplitude between winter and peak will be different than the m2
        # parameter in the double sigmoid
        final_amplitude = max(df$plant) - min(df$plant)
        
        # soil is a constant
        df$soil = soil_ndvi
        
        # combine the endmembers to the pixel level VI
        # using percent cover
        scaled_ndvi = df %>%
          pivot_longer(-doy, names_to='plant', values_to='ndvi') %>%
          left_join(plant_cover, by='plant') %>%
          mutate(scaled_ndvi = ndvi * percent_cover) %>%
          group_by(doy) %>%
          summarise(vi = sum(scaled_ndvi)) %>%
          ungroup()
        
        # add measurment error
        scaled_ndvi$vi = scaled_ndvi$vi + rnorm(nrow(scaled_ndvi), mean=0, sd= error)
        
        # debugging call to make a plot of everything
        #extract_phenology(scaled_ndvi, to_return = 'plot')
        
        phenology = extract_phenology(scaled_ndvi)
        
        phenology$plant_cover =  cover_values$plant[cover_i]
        phenology$amplitude   = final_amplitude
        phenology$error       = error
        phenology$bootstrap = i
        
        final_phenology_results = final_phenology_results %>%
          bind_rows(phenology)
  }
    }
  }
}


write_csv(final_phenology_results, 'data/vi_simulation_results.R')
