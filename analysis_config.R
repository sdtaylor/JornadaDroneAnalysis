
# ---------------
# Primary stuff
# ---------------

jorn_sites = c('NORT')

random_roi_sizes = tribble(
  ~pixel_size, ~n_rois,
  1,           5000,
  2,           5000,
  4,           3000,
  8,           2000,
  16,          1000,
  30,          500
)

#random_roi_sizes = c(1,2,4,8,16,30)
#random_rois_per_size = 1000
random_roi_file = './drone_analysis/data/gis/site_random_rois.geojson'
random_roi_ndvi_file = './drone_analysis/data/random_roi_ndvi.csv'

random_roi_percent_cover_file = './drone_analysis/data/random_roi_percent_cover.csv'