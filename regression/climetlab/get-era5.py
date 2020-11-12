#/bin/env python3

import cdsapi

cds = cdsapi.Client()
cds.retrieve('reanalysis-era5-pressure-levels', {
           "variable": "all",
           "pressure_level": "1000",
           "product_type": "reanalysis",
           "date": "2000-01-01",
           "time": "12:00",
           "format": "grib"
       }, 'era5-pl.grib')

cds.retrieve('reanalysis-era5-single-levels', {
           "variable": "all",
           "product_type": "reanalysis",
           "date": "2000-01-01",
           "time": "12:00",
           "format": "grib"
       }, 'era5-sfc.grib')
