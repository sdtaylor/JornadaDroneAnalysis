library(tidyverse)

# Look at some of the asd spectral data 
# Note this format is not consistent across all sites and files.
# especially which plant has which id changes quite a bit.

asd_id_info = tibble(
  ATCA = 0:9,
  PRGL = 10:19,
  GUSA = 20:29,
  YUCCA = 30:39,
  BARE_GROUND = 40:49) %>%
  pivot_longer(everything(), names_to='species', values_to='asd_id')

# for debugging
# filepath = './data/ASD_spectra/NORT/Reflectance/20190425/NO00000.asd.txt'

process_asd_file = function(filepath){
  filename = basename(filepath)
  site_id = str_sub(filename, 1,2)
  
  date_str = str_extract(filepath, '\\d{8}')
  date     = lubridate::ymd(date_str)
  
  id = str_extract(filename, '\\d{5}') # 5 digit number within the filename
  id = as.numeric(id)
  
  contents = read_tsv(filepath, skip = 1,
                      col_names = c('wavelength','reflectance'),
                      col_types = cols(wavelength = col_number(),
                                       reflectance = col_number()))
  
  contents$site_id = site_id
  contents$asd_id  = id
  contents$date    = date
  
  return(contents)
}

asd_files = list.files('./data/ASD_spectra/NORT', pattern = '*asd.txt$', recursive = T, full.names = T)

asd_data = map_df(asd_files, process_asd_file) %>%
  mutate(site_id = recode(site_id, 'NO'='NORT'))

asd_data = asd_data %>%
  left_join(asd_id_info, by='asd_id')

averaged_asd = asd_data %>%
  group_by(site_id, date, species, wavelength) %>%
  summarise(reflectance = mean(reflectance)) %>%
  ungroup() %>%
  filter(wavelength < 1000)

asd_ndvi = asd_data %>%
  filter(wavelength %in% c(680, 800)) %>%
  pivot_wider(names_from = 'wavelength', values_from='reflectance', names_prefix = 'w') %>%
  mutate(ndvi = (w800-w680)/(w800+w680)) %>%
  group_by(site_id, date, species) %>%
  summarise(ndvi = mean(ndvi, na.rm = T)) %>%
  ungroup()

ggplot(asd_ndvi, aes(x=date, y=ndvi, color=species)) + 
  geom_line() +
  geom_point() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_x_date(breaks = unique(asd_ndvi$date)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
  # 
# highlight_bands = c(650:700, 780:820)
# averaged_asd %>%
#   filter(wavelength %in% 600:900,
#          date>='2020-01-01') %>%
# ggplot(aes(x=wavelength, y=reflectance)) +
#   geom_point(aes(color=wavelength %in% highlight_bands)) + 
#   geom_vline(data=tibble(x=c(680,800)), aes(xintercept=x)) + 
#   facet_wrap(date~species)

#--------------------------------
asd_landsat_date_matching = tribble(
  ~site_id,  ~asd_date,    ~l8_date,
  'NORT',    '2019-04-25', '2019-04-20',
  'NORT',    '2019-07-05', '2019-07-09',
  'NORT',    '2019-08-14', '2019-08-10',
  'NORT',    '2019-09-27', '2019-09-27',
  'NORT',    '2019-10-16', '2019-10-13',
  'NORT',    '2019-11-13', '2019-11-14',
  'NORT',    '2020-01-02', '2020-01-01',
  'NORT',    '2020-05-19', '2020-05-24',
  'NORT',    '2020-07-29', '2020-07-27',
  'NORT',    '2020-10-06', '2020-09-29',  
  'NORT',    '2020-11-05', '2020-10-31',
)
asd_landsat_date_matching$l8_date = as.Date(asd_landsat_date_matching$l8_date)
asd_landsat_date_matching$asd_date = as.Date(asd_landsat_date_matching$asd_date)

site_fractional_cover = tribble(
  ~site_id, ~fractional_veg,
  'NORT',    0.31
)

#--------------------------------
l8_ndvi = read_csv('data/landsat_ndvi.csv') %>%
  mutate(ndvi = (B5-B4)/(B5+B4)) %>%
  mutate(date = lubridate::date(date))

ggplot(l8_ndvi, aes(x=date, y=ndvi, color=as.factor(site_id), group=pixel_id)) + 
  geom_point(size=2) + 
  geom_line() +
  scale_x_date(breaks = unique(l8_ndvi$date), limits=as.Date(c('2019-01-01','2020-12-31'))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


#--------------------------------
l8_ndvi2 = l8_ndvi %>%
  select(pixel_id, site_id, pixel_ndvi = ndvi, l8_date=date) %>%
  left_join(asd_landsat_date_matching, by=c('site_id','l8_date'))

estimated_error = asd_ndvi %>%
  filter(species %in% c('PRGL','BARE_GROUND')) %>%
  pivot_wider(names_from = species, values_from = ndvi, names_prefix='NDVI_') %>%
  filter(!is.na(NDVI_BARE_GROUND)) %>%
  left_join(l8_ndvi2, by=c('site_id','date'='asd_date')) %>% 
  left_join(site_fractional_cover, by='site_id') %>%
  mutate(error_estimate = pixel_ndvi - NDVI_PRGL*fractional_veg - (1-fractional_veg)*NDVI_BARE_GROUND,
         pixel_ndvi_estimate = NDVI_PRGL*fractional_veg - (1-fractional_veg)*NDVI_BARE_GROUND)

ggplot(estimated_error, aes(x=date, y=error_estimate)) + 
  geom_point() +
  scale_x_date(breaks = unique(estimated_error$date)) 

ggplot(estimated_error, aes(error_estimate)) + 
  geom_histogram() + 
  facet_wrap(~date)

estimated_error %>%
  group_by(date) %>%
  summarise(error_sd = sd(error_estimate))
