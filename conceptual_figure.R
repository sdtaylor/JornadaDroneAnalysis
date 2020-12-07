library(tidyverse)

source('ndvi_simulation_tools.R')

doy_full = tibble(doy = c(1:365))
doy_8_day = tibble(doy = seq(-60,450,8))

soil_vi = 0.2
error_rate = 0.04

set.seed(50)

#-------------------------
# One for 60% cover
label_60 = 'B. 60% Plant Cover'
doy_plant_60 = doy_8_day
doy_plant_60$plant_cover = label_60
# double logistic values to produce, with 0.1 threshold
# sos: 82, eos 292, season length: 210
doy_plant_60$pure_endmember_curve = elmore_double_sigmoid(doy_plant_60$doy, 
                                  m1 = 0.2,
                                 m2 = 0.6, # make this range from 0.3 - 1.0. But have to record absolute difference
                                 m3 = 100,
                                 m4 = 8,
                                 m5 = 280, 
                                 m6 = 8,
                                 m7 = 0.001)



doy_plant_60$plant_scaled = (doy_plant_60$pure_endmember_curve * 0.6) + (soil_vi * 0.4)
doy_plant_60$plant_scaled_with_error = doy_plant_60$plant_scaled + rnorm(nrow(doy_plant_60), mean=0, sd= error_rate)
doy_plant_60$plant_cubic_spline = predict(smooth.spline(doy_plant_60$doy, doy_plant_60$plant_scaled_with_error))$y

phenology_60 = extract_phenology(select(doy_plant_60, doy, vi = plant_scaled_with_error))
phenology_60$plant_cover = label_60
#-------------------------
# One for 30% cover
label_30 = 'C. 30% Plant Cover'
doy_plant_30 = doy_8_day
doy_plant_30$plant_cover = label_30
# double logistic values to produce, with 0.1 threshold
# sos: 82, eos 292, season length: 210
doy_plant_30$pure_endmember_curve = elmore_double_sigmoid(doy_plant_30$doy, 
                                                          m1 = 0.2,
                                                          m2 = 0.6, # make this range from 0.3 - 1.0. But have to record absolute difference
                                                          m3 = 100,
                                                          m4 = 8,
                                                          m5 = 280, 
                                                          m6 = 8,
                                                          m7 = 0.001)



doy_plant_30$plant_scaled = (doy_plant_30$pure_endmember_curve * 0.3) + (soil_vi * 0.7)
doy_plant_30$plant_scaled_with_error = doy_plant_30$plant_scaled + rnorm(nrow(doy_plant_30), mean=0, sd= error_rate)
doy_plant_30$plant_cubic_spline = predict(smooth.spline(doy_plant_30$doy, doy_plant_30$plant_scaled_with_error))$y

phenology_30 = extract_phenology(select(doy_plant_30, doy, vi = plant_scaled_with_error))
phenology_30$plant_cover = label_30
#---------------------------


ggplot(doy_plant_60, aes(x=doy)) + 
  geom_line(aes(y=pure_endmember_curve), size=2) + 
  scale_x_continuous(limits = c(1,365), breaks=c(1,100,200,300)) + 
  theme_minimal() +
  labs(title = 'A. Plant Endmember VI Curve') + 
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.45),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.background = element_rect(color='black'),
        legend.key.width = unit(20,'mm'),
        axis.text = element_text(size=20,color='black'),
        axis.title = element_blank(),
        plot.title = element_text(size=25, color='black'),
        strip.text = element_text(size=50, color='black', hjust=0.2))


##########################################

both = doy_plant_30 %>%
  bind_rows(doy_plant_60) %>%
  pivot_longer(cols = c(plant_scaled, plant_cubic_spline), names_to='curve_type', values_to='curve_value') 

phenology_both = phenology_30 %>%
  bind_rows(phenology_60) %>%
  filter(threshold==0.1) %>%
  pivot_longer(c(sos,eos), names_to='metric',values_to='doy')

both$curve_type = factor(both$curve_type, levels=c('plant_scaled','plant_cubic_spline'), labels=c('Scaled Pixel VI','Cubic Spline'))

ggplot(both, aes(x=doy)) + 
  geom_line(aes(y=curve_value, linetype=curve_type, color=curve_type), size=2) + 
  scale_linetype_manual(values=c('solid','solid')) + 
  scale_color_manual(values=c('#009E73','black')) + 
  geom_point(aes(y=plant_scaled_with_error), color='black', size=2) +
  geom_segment(data=phenology_both, aes(x=doy, xend=doy, y=0.4, yend=0.3),size=1, arrow=arrow(length=unit(4,'mm'))) + 
  geom_text(data=phenology_both, aes(x=doy, y=0.42, label=toupper(metric)), size=5) + 
  scale_x_continuous(limits = c(1,365),  breaks=c(1,100,200,300)) + 
  facet_wrap(~plant_cover, ncol = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.48),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_rect(color='black'),
        legend.key.width = unit(20,'mm'),
        axis.text = element_text(size=20,color='black'),
        axis.title = element_blank(),
        strip.text = element_text(size=25, color='black', hjust=0.1, ))
 