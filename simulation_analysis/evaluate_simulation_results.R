library(tidyverse)

vi_simulation_results = read_csv('./simulation_analysis/data/vi_simulation_results.csv')

#-----
# axis of variation
# amplitude
# error
# plant cover
# threshold, 0.1, 0.25, 0.5
# bootstraps (50)


#-----------------------------------
# percent of time the pixel scale amplitude is > 0.1
#-----------------------------------
percent_meeting_amplitude_labels = tribble(
  ~error, ~amplitude, ~plant_cover, ~percent_meeting_amplitude, 
  0.01,    0.1,           0.80,         0.25,
  0.01,    0.2,           0.48,         0.4,
  0.01,    0.4,           0.28,         0.5,
  #0.01,    0.6,           0.2,         0.65,
  0.01,    0.8,           0.12,         0.88,
  
#  0.04,    0.1,           0.7,         0.25,
#  0.04,    0.2,           0.48,         0.4,
#  0.04,    0.4,           0.28,        0.5,
#  #0.04,    0.6,           0.2,         0.65,
#  0.04,    0.8,           0.15,         0.88
)
percent_meeting_amplitude_labels$amplitude_label = paste0('VI[veg] == ',percent_meeting_amplitude_labels$amplitude)
#error_levels = c(0.01,0.04)
#error_labels = c('Uncertainty S.D. : 0.01', 'Error S.D. : 0.04')
#percent_meeting_amplitude_labels$error = factor(percent_meeting_amplitude_labels$error, levels = error_levels, labels=error_labels)

amplitude_colors = viridisLite::viridis(4, end = 0.8)

# Percent of time the 0.1 threshold is met
percent_meeting_amplitude_data = vi_simulation_results %>%
  group_by(threshold, plant_cover, amplitude, error) %>%
  summarise(percent_meeting_amplitude = 1 - mean(qa),
            n=n()) %>%
  ungroup() %>%
  filter(threshold %in% c(0.25)) %>% 
  filter(error %in% c(0.01,0.04)) %>%
  #mutate(error = factor(error, levels = error_levels, labels=error_labels)) %>%
  mutate(amplitude = round(amplitude,2)) %>%
  filter(amplitude %in% c(0.1, 0.2, 0.4, 0.8))


ggplot(percent_meeting_amplitude_data, aes(x=plant_cover, y = percent_meeting_amplitude, color=as.factor(amplitude))) +
  geom_line(aes(linetype=as.factor(error)),size=2) +
  #geom_point(size=2) + 
  geom_label(data=percent_meeting_amplitude_labels, label='                    ',color='black', label.size=0.5, size=6) + 
  geom_text(data=percent_meeting_amplitude_labels, aes(label=amplitude_label), parse=T, size=6) + 
  scale_x_continuous(breaks=seq(0,1,0.2),  labels = function(x){paste0(x*100,'%')}) + 
  scale_color_manual(values = amplitude_colors) + 
  #scale_color_viridis_d() + 
  #scale_color_brewer(palette='Blues') + 
  #facet_wrap(~error, labeller = label_value) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(color='black'),
        panel.grid.major = element_line(color='grey70', size=0.7),
        panel.grid.minor = element_line(color='grey90', size=0.8),
        axis.text = element_text(color='black', size=14),
        axis.title = element_text(size=16),
        strip.background = element_blank()) +
  guides(color=guide_none(),
         linetype = guide_legend(keywidth=unit(40,'mm'), 
                                 label.position = 'top',
                                 title.theme = element_text(size=14),
                                 label.theme = element_text(size=12))) + 
  labs(x='Fractional Vegetation Cover',y=bquote(atop('Proportion of simulations where',VI[pixel]~' amplitude > 0.1')),
       linetype='Uncertainty (S.D.)')


# The difference between the two Fig 2 panels. aka the affect of doubling the error rate
# this is probably a supplement figure
percent_meeting_amplitude_data %>%
  mutate(error = fct_recode(error, 'error01' = 'Error S.D. : 0.01', 'error04' = 'Error S.D. : 0.04')) %>% # rename this factor to something that can be column names
  pivot_wider(names_from='error', values_from='percent_meeting_amplitude') %>% 
  group_by(plant_cover, amplitude) %>%
  summarise(difference = error01 - error04) %>% 
  ungroup() %>%
  ggplot(aes(x=plant_cover, y=difference, color=as.factor(round(amplitude,2)))) + 
  geom_line(size=1.5) +
  scale_color_viridis_d(end=0.8) +
  theme_bw() + 
  labs(y='Difference in Proportion of simulations meeting thresholds\n Error:0.01 - Error:0.04',
       x='Fractional Vegetation Cover', color=bquote(VI[veg]~'Amplitude'))

#-----------------------------------
# MAE of estimates figure in relation to frac. cover, error, and amplitude.
#-----------------------------------

# The "true" sos/eos dates are obtained with 100%  veg cover and 0 error
# note they will change slightly w/ amplitude
true_phenology_metrics = vi_simulation_results %>% 
  filter(error == 0, plant_cover==1) %>% 
  select(-bootstrap) %>% 
  distinct() %>%
  mutate(sos = ifelse(is.infinite(sos), 1, sos),
         eos = ifelse(is.infinite(eos),365, eos)) %>%
  select(threshold, amplitude, sos_true = sos, eos_true = eos, peak_true = peak)

# bias_figure_labels = tribble(
#   ~error, ~amplitude, ~plant_cover, ~percent_meeting_amplitude, 
#   0.01,    0.1,           0.7,         0.25,
#   0.01,    0.4,           0.28,         0.5,
#   0.01,    0.8,           0.15,         0.88,
#   
#   0.04,    0.1,           0.7,         0.25,
#   0.04,    0.4,           0.28,        0.5,
#   0.04,    0.8,           0.15,         0.88
# )

vi_simulation_results %>%
  left_join(true_phenology_metrics, by=c('threshold','amplitude')) %>% 
  mutate(sos = ifelse(is.infinite(sos), 1, sos),
         eos = ifelse(is.infinite(eos),365, eos)) %>%
  group_by(threshold, plant_cover, amplitude, error) %>%
  summarise(SOS = mean(abs(sos-sos_true)),
            EOS = mean(abs(eos-eos_true)),
            Peak = mean(abs(peak - peak_true)),
            n=n()) %>%
  ungroup() %>%
  filter(threshold %in% c(0.1, 0.25)) %>% 
  filter(error %in% c(0.01,0.04)) %>%
  filter(round(amplitude,2) %in% c(0.1,0.2,0.8)) %>% 
  pivot_longer(c(SOS, EOS, Peak), names_to='metric', values_to='metric_values') %>%
  #mutate(error = factor(error, levels = error_levels, labels=error_labels)) %>%
  mutate(threshold = paste0('Percent of max threshold: ',threshold*100,'%')) %>%
  mutate(metric = factor (metric, levels=c('SOS','Peak','EOS'), ordered = T)) %>% 
  ggplot(aes(x=plant_cover, y = metric_values, color=as.factor(amplitude))) +
  #geom_line(aes(alpha=as.factor(error)), linetype='dotted', size=3) +
  geom_line(aes(linetype=as.factor(error)), size=1.25) +
  #scale_alpha_manual(values=c(0,1)) + 
  #scale_color_viridis_d(end=0.8, labels=c('0.1','0.2','0.4','0.8')) +
  scale_color_manual(values = amplitude_colors[c(1,2,4)], labels=c('0.1','0.2','0.8')) + # 0.4 is dropped here for clarity
  #scale_linetype_manual(values=c('solid','solid')) + 
  #scale_size_manual(values = c(1.5,1.5)) + 
  scale_y_continuous(breaks = seq(0,160,20), expand=expansion(mult=0.02)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), expand=expansion(mult=0.05), labels = function(x){paste0(x*100,'%')}) +
  coord_cartesian(ylim=c(0,100)) + 
  facet_grid(metric~threshold) + 
  theme_bw(15) +
  theme(legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.background = element_rect(color='black'),
        legend.text = element_text(size=13),
        legend.title = element_text(size=16),
        panel.grid.major = element_line(color='grey50', size=0.4),
        panel.grid.minor = element_line(color='grey90', size=0.5),
        axis.text = element_text(color='black'),
        strip.text = element_text( size=16),
        strip.text.y = element_text(face='bold'),
        strip.background = element_blank()) +
  labs(x='Fractional Vegetation Cover' ,y='Mean Absolute Error (MAE) of Estimates',
       linetype='Uncertainty (S.D.)',
       color=bquote(VI[veg]~'Amplitude')) +
  guides(color=guide_legend(override.aes = list(size=3), 
                            direction = 'vertical',
                            keywidth = unit(15,'mm')),
         linetype =  guide_legend(keywidth=unit(60,'mm'),
                                  #eyheight = unit(10,'mm'),
                                  #direction = 'vertical',
                                  label.position = 'top',
                                  title.position = 'top',
                                  title.theme = element_text(size=14, hjust=0.5),
                                  label.theme = element_text(size=12)))

