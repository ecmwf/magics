#!/usr/bin/env python

import magics as Magics
import numpy
import math

def f(x,y):
#	return ((math.sin((x-180.)/(2.*math.pi)))-(math.cos((y-90.)/(2.*math.pi))))
#	return ( (x-180.) + (y-90.)*3 )
	return (y-90.) 


Magics.init ()
Magics.setc("output_format","ps")
Magics.setc("output_name",  "py_arrays")

Magics.setr("INPUT_FIELD_INITIAL_LATITUDE",90.0)
Magics.setr("INPUT_FIELD_INITIAL_LONGITUDE",0.0)
Magics.setr("INPUT_FIELD_LATITUDE_STEP",-1.5)
Magics.setr("INPUT_FIELD_LONGITUDE_STEP",1.5)

NLON =360
NLAT =180

FIELD = numpy.fromfunction(f,(NLAT,NLON),dtype=numpy.float64)

Magics.set2r("INPUT_FIELD",FIELD,360,180)
Magics.setr("INPUT_FIELD_INITIAL_LONGITUDE",-180.)
Magics.setr("INPUT_FIELD_INITIAL_LATITUDE",90.)
Magics.setr("INPUT_FIELD_LONGITUDE_STEP",1.)
Magics.setr("INPUT_FIELD_LATITUDE_STEP",-1.)


Magics.setc("map_coastline_colour", "khaki")
Magics.setc("map_grid_colour",      "grey")

Magics.setc("contour",  		"on")
Magics.setc("contour_line_colour",	"sky")
Magics.setc("CONTOUR_HIGHLIGHT_COLOUR", "GREEN")
Magics.setc("contour_label",		"on")
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
