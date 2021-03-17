library(tidyverse)

rap_cover = read_csv('data/rap_cover.csv') %>%
  rename(ecoregion = NA_L3NAME) %>%
  filter(ecoregion != 'Chihuahuan Deserts') %>%     # This is a small chunk of the CH desert in AZ. 
  mutate(ecoregion = recode(ecoregion, 'Arizona/New Mexico Plateau'= 'Arizona-NM Plateau',
                                        'Sonoran Basin and Range' = 'Sonoran Desert')) %>%
  mutate(total_cover =  afag + pfag + shr + tree) %>%
  select(ecoregion, point_id, year,total_cover) %>%
  #pivot_longer(c(-ecoregion,-point_id,-year), names_to='cover_class', values_to='cover') %>%
  as_tibble()


# rap_cover = read_csv('data/rap_cover.csv') %>%
#   rename(ecoregion = NA_L3NAME) %>%
#   #mutate(tots_cover_1 =  afag + pfag + shr + tree,
#   #       tots_cover_2 = 100 - (bg + ltr)) %>%
#   #select(ecoregion, point_id, year, tots_cover_1, tots_cover_2) %>%
#   pivot_longer(c(-ecoregion,-point_id,-year), names_to='cover_class', values_to='cover')

# ggplot(rap_cover, aes(x=as.factor(year))) + 
#   geom_violin(aes(y=cover), position = position_dodge()) + 
#   facet_grid(cover_class~ecoregion)


median_cover = rap_cover %>%
  group_by(ecoregion, year) %>%
  summarise(cover_median = median(total_cover),
            cover_mean   = mean(total_cover),
            cover_high   = quantile(total_cover, 0.975),
            cover_low    = quantile(total_cover, 0.025),
            n=n()) %>%
  ungroup()

for(e in unique(median_cover$ecoregion)){
  
  fig =  median_cover %>%
    filter(ecoregion==e) %>%
      ggplot(aes(x=year, y=cover_median)) + 
        #geom_line(linetype='dotted', size=4) +
        geom_errorbar(aes(ymin=cover_low, ymax=cover_high),size=3, width=0) +
        geom_point(size=10, color='#009e73') + 
        scale_x_continuous(breaks=c(2013,2015,2017,2019)) + 
        scale_y_continuous(breaks=seq(0,100,20),limits = c(0,100), labels = function(x){paste0(x,'%')}, expand = c(0,0)) +
        scale_color_brewer(palette = 'Dark2') + 
        theme_bw() + 
        theme(axis.text = element_text(color='black', size=16),
              axis.title = element_text(size=21),
              plot.title = element_text(size=30),
              axis.title.x = element_blank(),
              panel.grid.major.y = element_line(color='grey50',size=1),
              panel.grid.minor.y = element_line(color='grey90',size=1),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.margin = margin(5,10,5,5,'mm')) +
        labs(y='Fractional\nVegetation Cover', title = str_wrap(e,20))

  ggsave(paste0('./manuscript/map_figure/',e,'.png'), fig, width=14, height=10, units='cm')
}

