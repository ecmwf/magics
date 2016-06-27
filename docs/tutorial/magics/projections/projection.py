# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module
from ??? import *

# Setting of the output file name
output = ???({'output_name':'projection_python'})

# Setting the coordinates of the geographical area
map=mmap({
	"top longitude ? ": 180., 
	"right latitude ??": 90., 
	"left longitude ??": -180., 
	"projection name??": "cylindrical", 
	"bottom latitude": -90.})


# Setting the coastlines attributes
coast = ??( {"land shade colour ???": "white", 
		 "sea shade colour": "cream", 
		 "grid??": "on", 
		 "grid colour??": "grey", 
		 "map_coastline_land_shade": "on", 
		 "map_coastline_sea_shade": "on", 
		 "label ??": "off", 
		 "coastline colour? ": "grey"})

# Setting of the title
text = ???({'t???': ["Global Map!"]})

# Can you guess the plot command?
plot(output, ???, ???, ??)
