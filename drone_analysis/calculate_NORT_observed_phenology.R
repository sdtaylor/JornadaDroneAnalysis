library(tidyverse)

#-------------------------------------------
# This data is part of the unreleased PHENOMET database.
# Values for the percent percent green canopy for mesquite at 
# the NORT site are provided in the repo for the relavant year.
# obtained with the following commented code.
#-------------------------------------------
library(phenometR)

# all_pros_glan = get_species_phenophase('PRGL')
# 
# percent_cover_values = tribble(
#   ~PHENOPHASE, ~STATUS, ~PERCENT_COVER,
#   'DS_202',     0,       0,
#   'DS_202',     1,       2.5,
#   'DS_202',     2,       14.5,
#   'DS_202',     3,       37,
#   'DS_202',     4,       57,
#   'DS_202',     5,       84,
#   'DS_202',     6,       97.5
# )
# 
# 
# nort_2019 = all_pros_glan %>%
#   filter(PHENOPHASE %in% c('DS_202')) %>%
#   filter(SITE_CODE %in% c('NO')) %>%
#   filter(YEAR==2019) %>%
#   left_join(percent_cover_values, by=c('PHENOPHASE','STATUS')) %>%
#   rename_with(tolower)
# 
# write_csv(nort_2019, 'data/nort_2019_ground_phenology.csv')
#-------------------------------------------


# here percent cover is the phenophase "Percentage of leaves green for deciduous shrubs"
nort_2019 = read_csv('data/nort_2019_ground_phenology.csv')  



#------
# extract phenology
#------

threshold = 10 # equal 10% green lvs since the scale here is 0-100%
full_year = data.frame(doy = 1:365)
full_year$smoothed_percent_green_lvs = predict(smooth.spline(x=nort_2019$doy,y=nort_2019$percent_cover), x=full_year$doy)$y

# make sure the smoothed line doesn't drop below 0%
full_year$smoothed_percent_green_lvs = pmax(0, full_year$smoothed_percent_green_lvs)

peak = full_year$doy[which.max(full_year$smoothed_percent_green_lvs)]
# onset is the day where the curve crosses the threshold the final time before increasing to the peak
sos = max(full_year$doy[full_year$doy < peak & full_year$smoothed_percent_green_lvs <= threshold])
# end is the day where the  curve crosses the threshold the first time while decreasing from the peak.
eos = min(full_year$doy[full_year$doy > peak & full_year$smoothed_percent_green_lvs <= threshold])


nort_true_phenology = data.frame(sos, peak, eos) %>%
  pivot_longer(everything(), names_to='metric', values_to='doy')

#----------
# nice figure to summarize
#----------

ggplot(nort_2019, aes(x=doy, y=percent_cover)) + 
  geom_line(aes( group=plant_id), size=1) +
  geom_point(aes( group=plant_id), size=2) +
  geom_line(data=full_year, aes(y=smoothed_percent_green_lvs), color='#009e73', size=2) +
  geom_vline(data=nort_true_phenology, aes(xintercept=doy, color=metric), size=2) +
  scale_y_continuous(breaks = seq(0,100,10)) + 
  scale_color_manual(values=c('#e69f00','#0072b2','#e69f00'))


#------------
# Results to use in analysis
#-----------
nort_true_phenology
print(paste('2019 visits:',n_distinct(nort_2019$date)))
print(paste('n mesquite plants at NORT:',n_distinct(nort_2019$plant_id)))

