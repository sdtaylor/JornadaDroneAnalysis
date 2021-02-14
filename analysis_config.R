
# ---------------
# Primary stuff
# ---------------

jorn_sites = c('P9', 'GIBPE', 'NORT')

final_roi_file = 'data/gis/site_rois_3_final.geojson'
roi_ndvi_file = 'data/roi_ndvi.csv'
roi_ortho_file = 'data/roi_ortho.csv'

roi_percent_cover_file = 'data/roi_percent_cover.csv'


random_roi_size = 8
random_rois_per_site = 5000
random_roi_file = 'data/gis/site_random_rois.geojson'
random_roi_ndvi_file = 'data/random_roi_ndvi.csv'

# ---------------
# Site ROI info
# ---------------

# These are the larger ROIs
primary_roi_info = tribble(
  ~site_id, ~plant,        ~scale, ~n_cells,
  'NORT',   'soil',         2,      10,
  'NORT',   'mesquite',     4,      40,
  'NORT',   'none',         30,     10,

  'P9',     'soil',         2,      10,
  'P9',     'grass',        4,      40,
  'P9',     'mesquite',     2,      50,
  'P9',     'none',         30,     10,
  
  'GIBPE',     'soil',         2,      10,
  'GIBPE',     'grass',        4,      40,
  'GIBPE',     'mesquite',     2,      50,
  'GIBPE',     'none',         30,     10,
)

# ---------------------
# These are plant specific scales which will be randomly
# placed within the larger plant scales described above. 
# eg. Each 4m grass cell in GIBPE described above will get 4 randomly placed 1m cells within it.
# ---------------------
subset_roi_info = tribble(
  ~site_id, ~plant,           ~scale, ~n_subcells,
  'NORT',   'mesquite',     0.5,    8,
  'NORT',   'mesquite',     1,      4,
  'NORT',   'mesquite',     2,      3,
  'NORT',     'none',         8,      8,
  'NORT',     'none',         16,     3,
  
  'P9',     'grass',        0.5,    8,
  'P9',     'grass',        1,      4,
  'P9',     'grass',        2,      3,
  'P9',     'mesquite',     0.5,    8,
  'P9',     'mesquite',     1,      4,
  'P9',     'none',         8,      8,
  'P9',     'none',         16,     3,
  
  'GIBPE',     'grass',        0.5,     8,
  'GIBPE',     'grass',        1,       4,
  'GIBPE',     'grass',        2,       3,
  'GIBPE',     'mesquite',     0.5,    8,
  'GIBPE',     'mesquite',     1,      4,
  'GIBPE',     'none',         8,      8,
  'GIBPE',     'none',         16,     3
)
