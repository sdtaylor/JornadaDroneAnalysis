library(tidyverse)
library(patchwork)

# makes the 2 plots for figure 1.

source('simulation_analysis/vi_simulation_tools.R')

doy_full = tibble(doy = c(1:365))
doy_8_day = tibble(doy = seq(-60,450,8))

soil_vi = 0.2
error_rate = 0.04

set.seed(50)

#-------------------------
# One for 60% cover
label_60 = '(b) Pixel with 60% fractional vegetation cover'
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
label_30 = '(c) Pixel with 30% fractional vegetation cover'
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
font_family = 'serif'

fig1a_original_vi = ggplot(doy_plant_60, aes(x=doy)) + 
  geom_line(aes(y=pure_endmember_curve), size=1.5) + 
  scale_x_continuous(limits = c(1,365), breaks=c(1,100,200,300)) + 
  theme_bw() +
  labs(caption = '(a) Plant canopy VI curve') + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size=1, color='black'),
        legend.position = c(0.8,0.45),
        legend.title = element_blank(),
        legend.text = element_text(size=14, family=font_family),
        legend.background = element_rect(color='black'),
        legend.key.width = unit(20,'mm'),
        axis.text = element_text(size=20,color='black', family=font_family),
        axis.title = element_blank(),
        plot.caption = element_text(hjust=0.5,vjust=-1, size=22, family=font_family),
        plot.title = element_blank(),
        strip.text = element_text(size=50, color='black', hjust=0.2,family=font_family),
        strip.background = element_blank())


#---------------------------------------------

both = doy_plant_30 %>%
  bind_rows(doy_plant_60) %>%
  pivot_longer(cols = c(plant_scaled, plant_cubic_spline), names_to='curve_type', values_to='curve_value') 

phenology_both = phenology_30 %>%
  bind_rows(phenology_60) %>%
  filter(threshold==0.1) %>%
  pivot_longer(c(sos,eos), names_to='metric',values_to='doy')

both$curve_type = factor(both$curve_type, levels=c('plant_scaled','plant_cubic_spline'), labels=c('Scaled pixel VI','Cubic spline'))

fig1bc_scaled_vi = ggplot(both, aes(x=doy)) + 
  geom_line(aes(y=curve_value, linetype=curve_type, color=curve_type), size=1.5) + 
  scale_linetype_manual(values=c('solid','solid')) + 
  scale_color_manual(values=c('#009E73','grey20')) + 
  geom_point(aes(y=plant_scaled_with_error), color='black', size=2) +
  geom_segment(data=phenology_both, aes(x=doy, xend=doy, y=0.4, yend=0.3),size=0.7, arrow=arrow(length=unit(4,'mm'))) + 
  geom_text(data=phenology_both, aes(x=doy, y=0.42, label=toupper(metric)), size=7, family=font_family) + 
  scale_x_continuous(limits = c(1,365),  breaks=c(1,100,200,300)) + 
  facet_wrap(~plant_cover, ncol = 1, strip.position='bottom') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size=1, color='black'),
        legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.title = element_blank(),
        legend.text = element_text(size=20, family=font_family),
        legend.background = element_blank(),
        legend.key.width = unit(20,'mm'),
        axis.text = element_text(size=18,color='black', family=font_family),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=22, color='black', hjust=0, family=font_family),
        strip.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=1.7)))

# An otherwise blank subplot for the 2 arrows
arrow_plot = ggplot(aes=aes(x=c(0,1),y=c(0,1))) +
  annotate('segment', x=0, xend=0.75, y=0.55, yend=0.9, size=1.0, color='black', arrow=arrow(length=unit(4,'mm'),angle=25,type='closed')) +
  annotate('segment', x=0, xend=0.75, y=0.45, yend=0.1, size=1.0, color='black', arrow=arrow(length=unit(4,'mm'),angle=25,type='closed')) +
  ylim(0,1) +
  xlim(0,0.9) + 
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

#---------------------------------------------
# See https://patchwork.data-imaginist.com/articles/guides/layout.html#moving-beyond-the-grid-1
# for explanation of this.

fig1_layout = "
####BBB
AAACBBB
AAACBBB
####BBB
"

fig1_final = fig1a_original_vi + 
  fig1bc_scaled_vi + 
  arrow_plot + 
  plot_layout(design = fig1_layout)

ggsave('manuscript/figures/fig1_conceptual_vi_curve.pdf', plot=fig1_final, width=40, height=25, units='cm', dpi=150)

 
