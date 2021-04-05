import rasterstats
import geopandas as gpd
import numpy as np
import pandas as pd
from shapely.geometry import MultiPoint

#---------------------
# attempt to query the rangeland analysis platform (RAP) percent cover
# rasters via cloud optimized geotiffs.
# not finished though, I just downloaded the rasters to atlas (~1TB)



l3_ecoregions = gpd.read_file('data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

# constraint to L1 ecoregions N.A. Deserts, Med California, S. Semi-arid highlands, & Temperate Sierras
#l3_ecoregions = l3_ecoregions[l3_ecoregions.NA_L1CODE.isin(['10','11','12','13'])]
l3_ecoregions = l3_ecoregions[l3_ecoregions.NA_L1CODE.isin(['10'])]

l3_ecoregions['geometry'] = l3_ecoregions.simplify(1000)
l3_ecoregions= l3_ecoregions.dissolve(by='NA_L3NAME')

n_points_per_ecoregion = 3


def sample_points_within_polygon(gpd_geom, n_points):
    # Returns a geoseries point object with n_points randomly distributed within
    # gpd_geom, a polygon/s
    polygon = gpd_geom.unary_union
    min_x, min_y, max_x, max_y = polygon.bounds
    ratio = polygon.area / polygon.envelope.area
    samples = np.random.uniform((min_x, min_y), (max_x, max_y), (int(n_points / ratio * 2), 2))
    multipoint = MultiPoint(samples)
    multipoint = multipoint.intersection(polygon)
    samples = np.array(multipoint)
    if samples.shape[0] < n_points:
        print('trying again with more starting points')
        return sample_points_within_polygon(gpd_geom, n_points*3)
    else:
        samples = samples[np.random.choice(len(samples), n_points)]
        return  gpd.GeoDataFrame(geometry=gpd.points_from_xy(samples[:,0], samples[:,1]))

random_points = l3_ecoregions.groupby('NA_L3NAME').apply(sample_points_within_polygon, n_points=n_points_per_ecoregion).reset_index()
random_points = random_points.set_geometry('geometry', crs=l3_ecoregions.crs)
random_points = random_points.to_crs(epsg=4326) # to RAP rasters crs
random_points['point_id'] = np.arange(len(random_points))

#-----------------------
rap_years = list(range(2013,2020))

def build_url(y):
    # http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v2/vegetation-cover-v2-2019.tif
    #return '/project/ltar_phenology_proj1/rap_cover/vegetation-cover-v2-{}.tif'.format(y)
    return 'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v2/vegetation-cover-v2-{}.tif'.format(y)

rap_bands = dict(afag = 1,  # annual forb and grass
                 bg   = 2,  # bare ground
                 ltr  = 3,  # litter
                 pfag = 4,  # perennial forb and grass
                 shr  = 5,  # shrub
                 tree = 6)

all_data = []
for year in rap_years:
    rap_url = build_url(year)
    extracted_values = random_points[['NA_L3NAME','point_id']].copy()
    extracted_values['year'] = year
    for band_name, band_i in rap_bands.items():
        pass
        extracted_values[band_name] = rasterstats.point_query(random_points, rap_url, band=band_i)
        extracted_values[band_name] = extracted_values[band_name].round(2)
    all_data.append(extracted_values)

pd.concat(all_data).to_csv('data/rap_cover.csv',index=False)





