library(tidyverse)

source('./simulation_analysis/vi_simulation_tools.R')

#------------------------------------
# This produces the conceptual figure in the discussion which shows
# that uncertainty (and therefore potential error) in transitiion dates
# increases as seasonal VI amplitude decreases
#------------------------------------

doy_full = tibble(doy = c(1:365))
doy_8_day = tibble(doy = seq(-60,450,8))

soil_vi = 0.2
error_sd = 0.04

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
  group_by(amplitude) %>%
  summarise(sos_mean = mean(sos),
            sos_sd   = sd(sos)) %>%
  ungroup() %>%
  mutate(error_bar_y = case_when(
    amplitude == 0.488 ~ 0.57,
    amplitude == 0.35  ~ 0.54,
    amplitude == 0.22  ~ 0.51
  ))
  


amplitude_colors = c('#000000','#0072b2','#e69f00')

estimated_curves %>%
  group_by(amplitude, doy) %>%
  summarise(vi_mean = mean(smoothed_vi),
            vi_sd   = sd(smoothed_vi)) %>%
  ungroup() %>%
  ggplot(aes(x=doy, y=vi_mean, color=as.factor(amplitude))) + 
  geom_ribbon(aes(ymin = vi_mean-vi_sd*1.97,
                  ymax = vi_mean+vi_sd*1.97,
                  fill = as.factor(amplitude)),
                  alpha=0.4, size=0) + 
  geom_line(size=2) +
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
  annotate('text', x=55, y=0.625, label='95% Confidence Interval\nfor Start of Season', size=5) + 
  annotate('rect', xmin = 15, xmax = 95, ymin = 0.49, ymax=0.66, size=0.5, linetype='dotted', color='black', fill='transparent') + 
  coord_cartesian(xlim=c(0,150)) +
  scale_x_continuous(breaks = c(1,50,100,150)) + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text = element_text(color='black', size=14),
        axis.title = element_text(size=16)) + 
  labs(y='Vegetation Index', x='Day of Year')

