# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import Magics

Magics.init()
Magics.setc("output_format",            "ps")
Magics.setc("output_name",              "using_python01")

Magics.setc("grib_input_type",          "file")
Magics.setc("grib_input_file_name",     "data/z500.grb")
Magics.grib()

Magics.setc("map_coastline_colour",     "khaki")
Magics.setc("map_grid_colour",          "grey")	

Magics.setc("contour",                  "on")
Magics.setc("contour_line_colour",      "sky")
Magics.setc("contour_highlight_colour", "red")
Magics.setc("contour_label",            "on")
Magics.cont()

Magics.text ()
Magics.coast()

Magics.new_page("super_page")

Magics.setc("subpage_map_projection", "polar_stereographic")
Magics.setr("subpage_lower_left_latitude",    18.51)
Magics.setr("subpage_lower_left_longitude",  -37.27)
Magics.setr("subpage_upper_right_latitude",   51.28)
Magics.setr("subpage_upper_right_longitude",  65.0)

Magics.cont ()
Magics.text ()
Magics.coast()

Magics.finalize()
