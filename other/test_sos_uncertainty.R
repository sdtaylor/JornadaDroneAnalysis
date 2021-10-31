library(tidyverse)

source('./simulation_analysis/vi_simulation_tools.R')

#------------------------------------
# This produces the conceptual figure in the discussion which shows
# that uncertainty (and therefore potential error) in transitiion dates
# increases as seasonal VI amplitude decreases
#------------------------------------

doy_full = tibble(doy = c(1:365))
doy_8_day = tibble(doy = seq(-100,500,8))

soil_vi = 0.2
error_sd = 0.01

iterations = 100

set.seed(55)

# These are values for the m2 parameter in the elmore function and correspond to 
# final amplitudes of c(,   0.2,  0.4)
amplitude_rates =    c(0.22,0.35, 0.488)


estimated_curves = tibble()
estimated_phenology = tibble()

for(a in amplitude_rates){
  for(i in 1:iterations){
    doy_plant = doy_8_day
    # double logistic values to produce, with 0.1 threshold
    # sos: 82, eos 292, season length: 210
    doy_plant$vi = elmore_double_sigmoid(doy_plant$doy, 
                                          m1 = 0.2,
                                          m2 = a, # make this range from 0.3 - 1.0. But have to record absolute difference
                                          m3 = 100,
                                          m4 = 8,
                                          m5 = 280, 
                                          m6 = 8,
                                          m7 = 0.0006)
    # add vi uncertainty
    doy_plant$vi = doy_plant$vi + rnorm(nrow(doy_plant), mean=0, sd= error_sd)
    
    phenology = extract_phenology(doy_plant, percent_threshold = c(0.1))
    phenology$iteration = i
    phenology$amplitude = a
    
    estimated_phenology = estimated_phenology %>%
      bind_rows(phenology)
    
    doy_full$smoothed_vi = predict(smooth.spline(doy_plant$doy, doy_plant$vi), x=doy_full$doy)$y
    doy_full$iteration = i
    doy_full$amplitude = a
    
    
    estimated_curves = estimated_curves %>%
      bind_rows(doy_full)
    
  }
}

summarized_phenology = estimated_phenology %>%
  #filter(method=='max_change_rate') %>%
  group_by(amplitude, method) %>%
  summarise(sos_mean = median(sos),
            sos_sd   = sd(sos)) %>%
  ungroup() %>%
  mutate(error_bar_y = case_when(
    amplitude == 0.488 ~ 0.70,
    amplitude == 0.35  ~ 0.67,
    amplitude == 0.22  ~ 0.63
  )) 

amplitude_colors = c('#000000','#0072b2','#e69f00')
font_family = 'serif'

fig8_sos_uncertainty = estimated_curves %>%
  group_by(amplitude, doy) %>%
  summarise(vi_mean = mean(smoothed_vi),
            vi_sd   = sd(smoothed_vi)) %>%
  ungroup() %>%
  ggplot(aes(x=doy, y=vi_mean, color=as.factor(amplitude))) + 
  annotate('rect', xmin = 48, xmax = 124, ymin = 0.61, ymax=0.87, size=0.6, linetype='solid', color='black', fill='white', alpha=0) + 
  annotate('rect', xmin = 50, xmax = 90, ymin = 0.62, ymax=0.785, size=0.5, linetype='solid', color='black', fill='white', alpha=0) + 
  annotate('rect', xmin = 91, xmax = 122, ymin = 0.62, ymax=0.785, size=0.5, linetype='solid', color='black', fill='white', alpha=0) + 
  geom_ribbon(aes(ymin = vi_mean-vi_sd*1.96,
                  ymax = vi_mean+vi_sd*1.96,
                  fill = as.factor(amplitude)),
                  alpha=0.4, size=0) + 
  geom_line(size=1) +
  geom_errorbarh(data=summarized_phenology,
                 aes(y=error_bar_y, x=0,
                     xmin = sos_mean - sos_sd*1.96, 
                     xmax = sos_mean + sos_sd*1.96),
                 height=0, size=2) + 
  geom_point(data=summarized_phenology,
             aes(y=error_bar_y, x=sos_mean),
             size=3) + 
  scale_color_manual(values=amplitude_colors) + 
  scale_fill_manual(values=amplitude_colors) + 
  annotate('label', x=85, y=0.83, label='95% Confidence interval\nfor start of season', size=6, fill='white', label.size=0, family=font_family) + 
  annotate('label', x=70, y=0.75, label='10% Threshold\nmethod', size=5, fill='white', label.size=0, family=font_family) + 
  annotate('label', x=106.5, y=0.75, label='Change rate\nmethod', size=5, fill='white', label.size=0, family=font_family) + 
  coord_cartesian(xlim=c(0,150), ylim=c(0.15, 0.85)) +
  scale_x_continuous(breaks = c(1,50,100,150)) + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=22, family=font_family),
        axis.title = element_text(size=24, family=font_family)) + 
  labs(y='Vegetation index', x='Day of year')

ggsave('manuscript/figures/fig8_sos_uncertainty.pdf', fig8_sos_uncertainty, width=20, height=18, units='cm', dpi=200)
