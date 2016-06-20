#!/usr/bin/env python
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# -*- coding: utf-8 -*-

import Magics
import numpy
import math

dx =  1.5
dy = -1.5

def f(y,x):
	yy=  90.+y*dy
	xx=-180.+x*dx
	return xx + yy


Magics.init ()
Magics.setc("output_format","ps")
Magics.setc("output_name",  "py_arrays")

Magics.setr("INPUT_FIELD_INITIAL_LATITUDE",90.0)
Magics.setr("INPUT_FIELD_INITIAL_LONGITUDE",-180.0) 
Magics.setr("INPUT_FIELD_LATITUDE_STEP",dy)
Magics.setr("INPUT_FIELD_LONGITUDE_STEP",dx)

NLON = 240
NLAT = 121

FIELD = numpy.fromfunction(f,(NLAT,NLON),dtype=numpy.float64)

Magics.set2r("INPUT_FIELD",FIELD)
#Magics.setr("INPUT_FIELD_INITIAL_LONGITUDE",-180.)
#Magics.setr("INPUT_FIELD_INITIAL_LATITUDE",90.)
#Magics.setr("INPUT_FIELD_LONGITUDE_STEP",1.)
#Magics.setr("INPUT_FIELD_LATITUDE_STEP",-1.)


Magics.setc("map_coastline_colour", "khaki")
Magics.setc("map_grid_colour",      "grey")
Magics.setc ("contour_grid_value_plot",         "on")
Magics.setc ("contour_grid_value_plot_type",    "value")
Magics.seti ("contour_grid_value_lat_frequency", 8)
Magics.seti ("contour_grid_value_lon_frequency", 8)
Magics.setr ("contour_grid_value_height",        0.3)
    
Magics.setc("contour",  		"off")
Magics.setc("contour_line_colour",	"sky")
Magics.setc("CONTOUR_HIGHLIGHT_COLOUR", "GREEN")
Magics.setc("contour_hilo",		"off")
Magics.cont()

Magics.text()
Magics.coast()

Magics.new_page ("SUPER_PAGE")

Magics.setr("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0)
Magics.setr("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0)
Magics.setr("SUBPAGE_UPPER_RIGHT_LATITUDE",   65.0)
Magics.setr("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0)

Magics.cont()
Magics.text()
Magics.coast()

Magics.finalize()
