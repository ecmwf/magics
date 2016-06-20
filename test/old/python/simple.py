# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import Magics

Magics.init ()
Magics.setc("output_format","svg")
Magics.setc("output_name",  "python_simple")
Magics.setr("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0)
Magics.setr("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0)
Magics.setr("SUBPAGE_UPPER_RIGHT_LATITUDE",   65.0)
Magics.setr("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0)

Magics.setc("grib_input_file_name", "../data/z500.grb")
Magics.grib()

Magics.setc("map_coastline_colour", "khaki")
Magics.setc('map_coastline_land_shade', 'on')
Magics.setc('map_coastline_land_shade_colour', 'tan')
Magics.setc('map_coastline_sea_shade', 'on')
Magics.setc('map_coastline_sea_shade_colour', 'sky')
Magics.setc('map_boundaries', 'on')
Magics.setc('map_boundaries_colour', 'white')
Magics.setc('map_disputed_boundaries', 'on')
Magics.setc('map_disputed_boundaries_colour', 'red')
Magics.setc('map_cities', 'on')
Magics.setc("map_grid_colour","grey")
Magics.coast()

Magics.setc("contour",  "on")
Magics.setc("contour_line_colour","green")
Magics.cont()

Magics.text()
 
Magics.finalize()
