# (C) Copyright 1996-2019 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import cftime
import xarray as xr
import numpy as np

from Magics import macro as magics

ref = "xarray6"

ds = xr.open_dataset('psl_Amon_GFDL-ESM2M_rcp45_r1i1p1_208101-208512.nc')

png = magics.output(output_name_first_page_number = "off", output_name = ref)

contour = magics.mcont(contour_automatic_setting = "ecmwf")

time = cftime.DatetimeNoLeap(2085, 12, 16, 12, 0, 0, 0, 5, 350)

magics.plot(png, magics.mxarray(ds = ds, var = "psl", time = time), contour, magics.mcoast())
