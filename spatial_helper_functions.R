library(sf)


make_polygon = function(ll_x, ll_y, scale){
  # Make a square geom given the lower left
  # coordinates and size.
  coords = rbind(c(ll_x, ll_y),
                 c(ll_x, ll_y + scale),
                 c(ll_x + scale, ll_y + scale),
                 c(ll_x+scale, ll_y),
                 c(ll_x, ll_y))
  return(st_polygon(list(coords)))
}

points_to_squares = function(points, scale){
  # Given an sf object of points, create a new 
  # object of the same number of squares where the
  # point represents the lower left corner. 
  points = as.data.frame(st_coordinates(points))
  cell_geoms = list()
  for(i in 1:nrow(points)){
    cell_geoms[[i]] = make_polygon(ll_x = points$X[i],
                                   ll_y = points$Y[i],
                                   scale = scale)
  }
  
  return(st_sfc(cell_geoms, crs = st_crs(points)))
}

polygon_sample = function(p, n, new_polygon_scale){
  # Within the polygon p, create n new square polygons
  # of size new_polygon_scale
  bbox = st_bbox(p)
  
  # Make a slightly cropped polygon to sample random points in
  # so that the new subset cells do not bleed outside it.
  cropped_scale = (bbox$xmax - bbox$xmin) - new_polygon_scale
  search_space = make_polygon(ll_x = bbox$xmin,
                              ll_y = bbox$ymin,
                              scale = cropped_scale)
  
  random_points = st_sample(search_space, size=n)
  subcell_geoms = points_to_squares(random_points,
                                   scale = new_polygon_scale)
  subcell_features = st_sf(geometry  = subcell_geoms,
                           crs   = st_crs(p),
                           precision = 1000)
  
  return(subcell_features)
}
