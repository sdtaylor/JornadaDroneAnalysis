"""
Uses google earth engine which you need an account for and need to authenticate
https://developers.google.com/earth-engine/guides/python_install

Also a small extraction tool for earth engine

https://github.com/bluegreen-labs/gee_subset

pip install git+git://github.com/bluegreen-labs/gee_subset


TODO: potentially convert TOA to SR
https://github.com/samsammurphy/gee-atmcorr-S2/blob/master/jupyer_notebooks/sentinel2_atmospheric_correction.ipynb

surface reflectance is available as a different product but does not go
all the way back to 2016.
"""

import ee
from gee_subset.gee_subset import gee_subset
import pandas as pd
# ee.Authenticate()
ee.Initialize()


#site_locations = pd.read_csv('data/gis/other_locations.csv')
site_locations = pd.read_csv('data/gis/site_center_points.csv')

gee_product = 'LANDSAT/LC08/C01/T1_SR'
#gee_product = 'MODIS/MYD09Q1'
gee_bands = ['B4','B5',
             'pixel_qa']

gee_scale = 30
buffer_size = 0.1 # in km
start_date = '2015-01-01'
end_date   = '2020-12-31'


all_data = []
for loc_i, loc in site_locations.iterrows():
    pass
    df = gee_subset(product = gee_product,
                    bands = gee_bands,
                    scale = gee_scale,
                    start_date = start_date,
                    end_date   = end_date,
                    latitude = loc.lat,
                    longitude = loc.lon,
                    pad=buffer_size)
    df['site_id'] = loc.site_id
    df['pixel_id'] = df.index

    all_data.append(df)

all_data = pd.concat(all_data)

#scaling factor
for b in ['B4','B5']:
    all_data[b] = (all_data[b] * 0.0001).round(5)

all_data.to_csv('data/landsat_ndvi.csv', index=False)
