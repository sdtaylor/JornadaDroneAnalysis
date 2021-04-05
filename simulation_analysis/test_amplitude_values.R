library(tidyverse)
source('ndvi_simulation_tools.R')

#-------------------------------
# Here we test out different amplitude rates with the double sigmoid function.
# a pretty rainbow figure of all of them is produced at the end. 
#-------------------------------

# winter ndvi is generally higher than pure soil 
plant_winter_ndvi = 0.2
soil_ndvi = 0.2

amplitude_rates = seq(0.1,1.0,0.01)
amplitude_rates = c(0.18, 0.285, 0.387,0.488, 0.588, 0.69, 0.79, 0.89)

df = tibble(doy = seq(-60,450,8))

all_vis = data.frame()
for(amplitude in amplitude_rates){
  
  df$plant = elmore_double_sigmoid(df$doy, 
                                   m1 = plant_winter_ndvi,
                                   m2 = amplitude, # make this range from 0.3 - 1.0. But have to record absolute difference
                                   m3 = 100,
                                   m4 = 8,
                                   m5 = 280, 
                                   m6 = 8,
                                   m7 = 0.0006)
  df$amplitude = amplitude
  df$final_amplitude = max(df$plant) - min(df$plant)
  
  all_vis = all_vis %>%
    bind_rows(df)
}

all_vis %>%
  #filter(final_amplitude<0.11) %>%
ggplot( aes(x=doy, y=plant, color=as.factor(final_amplitude), group=final_amplitude)) + 
  geom_line() +
  scale_color_viridis_c()


