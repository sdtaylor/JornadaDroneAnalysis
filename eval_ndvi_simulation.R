library(tidyverse)

vi_simulation_results = read_csv('data/vi_simulation_results.csv')

#-----
# axis of variation
# amplitude
# error
# plant cover
# threshold, 0.1, 0.25, 0.5
# bootstraps (50)

# just looking at whether it meets 0.1 amplitude rise
vi_simulation_results %>%
  filter(threshold==0.1) %>% 
  group_by(plant_cover) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa)) %>%
  ungroup() %>%
  ggplot(aes(x=plant_cover, y=percent_meeting_amplitude)) +
  geom_point()


vi_simulation_results %>%
  filter(error==0.02) %>%
  pivot_longer(c(peak, sos, eos, season_length), names_to='metric', values_to='value') %>%
  ggplot(aes(x=as.factor(plant_cover), y=value)) + 
  geom_jitter(alpha=0.2) + 
  geom_vline(xintercept = as.factor(0.3)) + 
  facet_wrap(metric~threshold, ncol=3)


#-----------------------------------
# percent of time the pixel scale amplitude is > 0.1
#-----------------------------------
percent_meeting_amplitude_labels = tribble(
  ~error, ~plant_cover, ~amplitude, ~percent_meeting_amplitude, 
  0.02,    0.1,           0.7,         0.25,
  0.02,    0.2,           0.48,         0.4,
  0.02,    0.4,           0.28,         0.5,
  0.02,    0.6,           0.2,         0.65,
  0.02,    0.8,           0.15,         0.88,
  
  0.04,    0.1,           0.7,         0.25,
  0.04,    0.2,           0.48,         0.4,
  0.04,    0.4,           0.28,        0.5,
  0.04,    0.6,           0.2,         0.65,
  0.04,    0.8,           0.15,         0.88
)
percent_meeting_amplitude_labels$percent_cover_label = paste0(percent_meeting_amplitude_labels$plant_cover*100,'%')
error_levels = c(0.02,0.04)
error_labels = c('Error Rate : 0.02', 'Error Rate : 0.04')
percent_meeting_amplitude_labels$error = factor(percent_meeting_amplitude_labels$error, levels = error_levels, labels=error_labels)

# Percent f time the 0.1 threshold i smet
primary_plant_cover = c(0.1, 0.2, 0.4, 0.6, 0.8)
vi_simulation_results %>%
  group_by(threshold, plant_cover, amplitude, error) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa),
            n=n()) %>%
  ungroup() %>%
  filter(threshold %in% c(0.1)) %>% 
  filter(error %in% c(0.02,0.04)) %>%
  mutate(error = factor(error, levels = error_levels, labels=error_labels)) %>%
  filter(plant_cover %in% primary_plant_cover) %>%
  ggplot(aes(x=amplitude, y = percent_meeting_amplitude, color=as.factor(plant_cover))) +
  geom_line(size=1) +
  geom_point(size=2) + 
  geom_label(data=percent_meeting_amplitude_labels, aes(label=percent_cover_label)) + 
  scale_color_viridis_d(end=0.9) +
  #scale_color_brewer(palette='Blues') + 
  facet_wrap(~error, labeller = label_value) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=8),
        strip.text = element_text(size=12),
        strip.background = element_blank()) +
  labs(x='Relative Amplitude of plant endmember',y='Percent of permutations where \npixel scale amplitude > 0.1')

#-----------------------------------
#-----------------------------------


primary_plant_cover = c(0.1, 0.2, 0.4, 0.6, 0.8)
vi_simulation_results %>%
  group_by(threshold, plant_cover, amplitude, error) %>%
  mutate(sos = ifelse(is.infinite(sos), 1, sos)) %>%
  mutate(sos_bias = sos-82) %>%
  filter(threshold %in% c(0.1)) %>% 
  filter(error %in% c(0.01,0.02,0.04,0.08)) %>%
  filter(plant_cover %in% primary_plant_cover) %>%
  ggplot(aes(x=amplitude, y = sos_bias, color=as.factor(plant_cover))) +
  geom_smooth(method='loess', se=F) + 
  #geom_line() +
  #geom_point() + 
  scale_color_viridis_d() +
  facet_wrap(~error)

