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


primary_plant_cover = c(0.1, 0.2, 0.4, 0.6, 0.8)


#-----------------------------------
# percent of time the pixel scale amplitude is > 0.1
#-----------------------------------
percent_meeting_amplitude_labels = tribble(
  ~error, ~amplitude, ~plant_cover, ~percent_meeting_amplitude, 
  0.02,    0.14,           0.7,         0.25,
  0.02,    0.22,           0.48,         0.4,
  0.02,    0.41,           0.28,         0.5,
  0.02,    0.61,           0.2,         0.65,
  0.02,    0.80,           0.15,         0.88,
  
  0.04,    0.14,           0.7,         0.25,
  0.04,    0.22,           0.48,         0.4,
  0.04,    0.41,           0.28,        0.5,
  0.04,    0.61,           0.2,         0.65,
  0.04,    0.80,           0.15,         0.88
)
percent_meeting_amplitude_labels$amplitude_label = paste0('a=',percent_meeting_amplitude_labels$amplitude)
error_levels = c(0.02,0.04)
error_labels = c('Error S.D. : 0.02', 'Error S.D. : 0.04')
percent_meeting_amplitude_labels$error = factor(percent_meeting_amplitude_labels$error, levels = error_levels, labels=error_labels)

primary_plant_amplitudes = c(0.14, 0.22, 0.41, 0.61, 0.80)

# Percent of time the 0.1 threshold is met
vi_simulation_results %>%
  mutate(plant_cover = ceiling(plant_cover*10*2)/2/10) %>% # round to the nearest 0.05
  group_by(threshold, plant_cover, amplitude, error) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa),
            n=n()) %>%
  ungroup() %>%
  filter(threshold %in% c(0.1)) %>% 
  filter(error %in% c(0.02,0.04)) %>%
  mutate(error = factor(error, levels = error_levels, labels=error_labels)) %>%
  filter(round(amplitude,2) %in% primary_plant_amplitudes) %>% 
  ggplot(aes(x=plant_cover, y = percent_meeting_amplitude, color=as.factor(amplitude))) +
  geom_line(size=1) +
  geom_point(size=2) + 
  geom_label(data=percent_meeting_amplitude_labels, aes(label=amplitude_label)) + 
  scale_x_continuous(breaks=c(0.05,0.25,0.5,0.75,1.0)) + 
  scale_color_viridis_d(end=0.8) +
  #scale_color_brewer(palette='Blues') + 
  facet_wrap(~error, labeller = label_value) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=12),
        axis.title = element_text(size=14),
        strip.text = element_text(size=16),
        strip.background = element_blank()) +
  labs(x='Fractional Vegetation Cover',y='Proportion of simulations where \namplitude > 0.1')

#-----------------------------------
#-----------------------------------

# The "true" sos/eos dates are obtained with 100% cover and 0 error
# note they will change slightly w/ amplitude
true_phenology_metrics = vi_simulation_results %>% 
  filter(error == 0, plant_cover==1) %>% 
  select(-bootstrap) %>% 
  distinct() %>%
  select(threshold, amplitude, sos_true = sos, eos_true = eos, peak_true = peak, true_peak_doy)

vi_simulation_results %>%
  left_join(true_phenology_metrics, by=c('threshold','amplitude')) %>% 
  group_by(threshold, plant_cover, amplitude, error) %>%
  mutate(sos = ifelse(is.infinite(sos), 1, sos),
         eos = ifelse(is.infinite(eos),365, eos)) %>%
  mutate(sos_bias = sos-sos_true,
         eos_bias = eos-eos_true,
         peak_bias = peak - peak_true) %>%
  filter(threshold %in% c(0.1,0.25)) %>% 
  filter(error %in% c(0.02,0.04)) %>%
  filter(plant_cover %in% primary_plant_cover) %>%
  ggplot(aes(x=amplitude, y = sos_bias, color=as.factor(plant_cover))) +
  geom_smooth(method='loess', se=F) + 
  #geom_line() +
  #geom_point() + 
  #ylim(-10,50) + 
  scale_color_viridis_d() +
  facet_grid(threshold~error, labeller = label_both) + 
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=8),
        strip.text = element_text(size=12),
        strip.background = element_blank()) +
  labs(x='Relative amplitude of plant endmember',y='Percent of permutations where \npixel scale amplitude > 0.1')

