library(tidyverse)
library(doParallel)
source('./simulation_analysis/vi_simulation_tools.R')

# winter ndvi is generally higher than pure soil 
plant_winter_ndvi = 0.2
soil_ndvi = 0.2

n_bootstraps = 200

error_rates = c(0, 0.01, 0.04)
# error_rates = c(0.02)

# These are values for the m2 parameter in the elmore function and correspond to 
# final amplitudes of c(0.1,  0.2,  0.4,    0.8)
amplitude_rates =    c(0.18, 0.285,  0.488, 0.89)
###############
# generate all combinations of soil/plant cover
cover_step = 2
cover_values = tibble(plant=numeric(),soil=numeric())
for(plant_percent_cover in seq(0,100,cover_step)){
  soil_cover = 100 - plant_percent_cover
  cover_values = cover_values %>%
    add_row(plant=plant_percent_cover, soil=soil_cover)
}

cover_values = filter(cover_values, plant!=0)
cover_values = add_row(cover_values, plant=1,soil=99)

cover_values$plant = cover_values$plant/100
cover_values$soil  = cover_values$soil/100


##############
# setup parallel processing 
cl <- makeCluster(2)
registerDoParallel(cl)
##############

final_phenology_results = tibble()
##############
for(cover_i in 1:nrow(cover_values)){
  print(paste0('Cover step ',cover_i,' of ',nrow(cover_values)))
  
  plant_cover = tribble(
    ~plant, ~percent_cover,
    'plant', cover_values$plant[cover_i],
    'soil',  cover_values$soil[cover_i]
  )
  
  for(error in error_rates){
    for(amplitude in amplitude_rates){
      
      iteration_reslts = foreach(i=1:n_bootstraps, .combine = rbind,
                                 .packages = c('dplyr','tidyr')) %dopar% {
      #for(i in 1:n_bootstraps){
            # Dormant season VI values are "stretched" well beyond the annual bounds. This is so the
            # smoothing algorithm does not have any hard spikes at the begining and end.
            doy = seq(-100,500,8)
            
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
                                             m7 = 0.0006)
      
            # final amplitude between winter and peak will be different than the m2
            # parameter in the double sigmoid
            final_amplitude = max(df$plant) - min(df$plant)
            
            # The true VI peak from the pure endmember curve.
            true_peak = doy[which.max(df$plant)]
            
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
            
            # maximum value composition. MODIS 8-day NDVI takes the max value
            # # within any 8 day window as a noise reduction method.
            # If enabling this chagen `doy = seq(-60,450,8)`` above to `doy = seq(-60,450,1)``
            # scaled_ndvi$vi_max = zoo::rollmax(scaled_ndvi$vi, k=8, fill=NA)
            # 
            # scaled_ndvi = scaled_ndvi %>%
            #   filter(doy %in% seq(-60,450,8))
            
            # debugging call to make a plot of everything
            #extract_phenology(scaled_ndvi, to_return = 'plot')
            
            phenology = extract_phenology(scaled_ndvi)
            
            phenology$plant_cover =  cover_values$plant[cover_i]
            phenology$amplitude   = final_amplitude
            phenology$true_peak_doy = true_peak
            phenology$error       = error
            phenology$bootstrap = i
            
            return(phenology)
            #final_phenology_results = final_phenology_results %>%
            #  bind_rows(phenology)
          }
      
      final_phenology_results = final_phenology_results %>%
        bind_rows(iteration_reslts)
      
    }
  }
}


write_csv(final_phenology_results, './simulation_analysis/data/vi_simulation_results.csv')
