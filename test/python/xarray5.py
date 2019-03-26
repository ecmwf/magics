# (C) Copyright 1996-2019 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import xarray as xr
import numpy as np

from Magics import macro as magics

ref = "xarray5"

ds = xr.open_dataset('C3S_OZONE-L4-TC-ASSIM_MSR-201608-fv0020.nc', decode_times=False)

png = magics.output(output_name_first_page_number = "off", output_name = ref)

contour = magics.mcont(contour_automatic_setting = "ecmwf")

magics.plot(png, magics.mxarray(ds, "total_ozone_column"), contour, magics.mcoast())
