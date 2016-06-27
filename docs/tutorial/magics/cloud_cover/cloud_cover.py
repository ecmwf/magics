# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from ???? import *

#Setting of the output file name
files = output({"output_formats":['ps', 'png'], 
			'output_name':'cloud_cover_asia'})

#Setting the coordinates of the geographical area
asia = mmap({ ???})

#Coastlines setting
coast = mcoast( {???})

#Import the cloud data 
cloud_cover =  mgrib({ "grib_input_file_name" : "cloud_cover.grb" })

colour_list=  ['HSL(0,0,1)','HSL(29,0.14,0.92)',
			'HSL(29,0.29,0.83)','HSL(29,0.43,0.75)','HSL(300,0.08,0.92)',
			'HSL(360,0.16,0.84)','HSL(13,0.3,0.75)','HSL(18,0.44,0.67)',
			'HSL(300,0.16,0.83)','HSL(340,0.22,0.75)','HSL(360,0.34,0.67)',
			'HSL(8,0.47,0.58)','HSL(300,0.24,0.75)','HSL(330,0.28,0.67)',
			'HSL(349,0.38,0.58)','HSL(360,0.5,0.5)','HSL(180,0.17,0.92)',
			'HSL(120,0.08,0.84)','HSL(57,0.17,0.75)','HSL(44,0.3,0.67)',
			'HSL(209,0.14,0.84)','HSL(187,0,0.75)','HSL(29,0.15,0.67)',
			'HSL(29,0.29,0.59)','HSL(239,0.16,0.75)','HSL(299,0.08,0.67)',
			'HSL(360,0.17,0.58)','HSL(13,0.3,0.5)','HSL(258,0.21,0.67)',
			'HSL(299,0.16,0.59)','HSL(341,0.22,0.5)','HSL(360,0.33,0.42)',
			'HSL(180,0.34,0.83)','HSL(161,0.22,0.75)','HSL(120,0.16,0.67)',
			'HSL(78,0.21,0.58)','HSL(193,0.3,0.75)','HSL(180,0.17,0.67)',
			'HSL(120,0.08,0.58)','HSL(59,0.16,0.5)','HSL(209,0.29,0.67)',
			'HSL(209,0.15,0.58)','HSL(217,0,0.5)','HSL(29,0.14,0.42)',
			'HSL(224,0.3,0.58)','HSL(237,0.17,0.5)','HSL(299,0.08,0.42)',
			'HSL(360,0.16,0.33)','HSL(180,0.5, 0.75)','HSL(169,0.38,0.67)',
			'HSL(150,0.28,0.58)','HSL(120,0.24,0.5)','HSL(188,0.47,0.67)',
			'HSL(180,0.34,0.59)','HSL(160,0.22,0.5)','HSL(120,0.16,0.42)',
			'HSL(198,0.44,0.58)','HSL(193,0.3,0.5)','HSL(180,0.17,0.42)',
			'HSL(120,0.08,0.33)','HSL(209,0.43,0.5)','HSL(209,0.29,0.42)',
			'HSL(209,0.14,0.33)','HSL(191,0,0.25)']

#Define the cloud cover 
cloud_cover_contour = mcont({
		????
        'contour_shade_technique': 'cell_shading',
        'contour_shade_colour_method':  'list',
        'contour_shade_colour_list':  colour_list,
           })

texts = [ [colour_list[3], "Low"],
		  [colour_list[15], "L+M"],
		  [colour_list[12], "Medium"],
		  [colour_list[60], "M+H"],
		  [colour_list[48], "High"],
		  [colour_list[51], "H+L"]
		]
line = "      "
for t in texts:
	line = line + "<font colour='" + t[0] + "'> " + t[1] + " </font>"

#Here we configure the title and add some colours
lines = ["Cloud cover valid for <grib_info key='valid-date'/>",
		 line
		]

title = mtext({
           "text_lines" : lines,
           ?????})


#To the plot
??????

