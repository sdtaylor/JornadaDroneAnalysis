library(sf)
library(stars)

#-------------------
# This creates a 0/1 raster indicating shrub canopies at NORT
# Used to calcuate the percent cover within all the randomly placed rois (aka. simulated pixels)
#------------------

# This GIS layer I made by hand in QGIS.
nort_shrubs = sf::st_read('./drone_analysis/data/gis/NORT_shrubs.geojson')

# dummy value to designate 1 for raster shrub pixels
nort_shrubs$value = 1

x = stars::st_rasterize(nort_shrubs['value'],
                        dx = 0.1,   # 10cm cell size
                        dy = 0.1)

# everything outside polygons, ie not shrubs, set to 0
x=st_apply(x, 1:2, FUN = function(x){ifelse(is.na(x),0,x)})


stars::write_stars(x, './drone_analysis/data/nort_shrub_cover.tif')
