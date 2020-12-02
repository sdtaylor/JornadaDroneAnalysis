library(tidyverse)



# just looking at whether it meets 0.1 amplitude rise
final_phenology_results %>%
  filter(threshold==0.1) %>% 
  group_by(plant_cover) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa)) %>%
  ungroup() %>%
  ggplot(aes(x=plant_cover, y=percent_meeting_amplitude)) +
  geom_point()


final_phenology_results %>%
  filter(error==0.02) %>%
  pivot_longer(c(peak, sos, eos, season_length), names_to='metric', values_to='value') %>%
  ggplot(aes(x=as.factor(plant_cover), y=value)) + 
  geom_jitter(alpha=0.2) + 
  geom_vline(xintercept = as.factor(0.3)) + 
  facet_wrap(metric~threshold, ncol=3)

#library(ggtern)
#library(scatterpie)
final_phenology_results %>%
  group_by(threshold, plant_cover, amplitude, error) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa),
            sos_bias = mean(sos-82),
            n=n()) %>%
  ungroup() %>%
  filter(threshold==0.1, error == 0.02) %>% 
  ggplot(aes(x=amplitude, y = sos_bias, color=as.factor(plant_cover))) +
  geom_line() +
  scale_color_viridis_d()