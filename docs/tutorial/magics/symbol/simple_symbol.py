# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from magmacro import *

#Setting of the output file name
output = output({"output_formats":['ps', 'png'], 
		'output_name': 'simple_symbol'})


#Background Coastlines 
background = mcoast( {???})

#Import the airep data 
airep =  mgeo({ "geo_input_file_name" : "../airep.geo"})

#Define the simple contouring for msl
airep_symbol = msymb( {"symbol_type" : ???,
		    "symbol_colour" : ???,
			"symbol_height" : 0.1,
			"symbol_marker": 15 })

lines =["My first symbol plotting"]

title = mtext({???})


#To the plot
plot(???)














