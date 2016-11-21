# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#importing Magics module
from Magics.macro import *


ref = 'logo'
#Setting of the output file name
output = output(output_formats = ['png'], 
		output_name_first_page_number = "off",
        super_page_x_length = 21.,
        super_page_y_length = 29.7,
		output_name = ref)

projection1 = mmap(page_y_position = 0.,
			page_y_length = 9.,
		    page_id_line_logo_name = "ecmwf",
			page_frame = "on",
		    subpage_y_length = 6., 
		    subpage_y_position = 2., 
			)


title1 = mtext(
           text_lines = ["ecmwf logo..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")

page1 = page()
#Setting the cartesian view
projection2 = mmap( 
			page_y_position = 9.,
			page_y_length = 9.,
			page_frame = "on",
		    subpage_y_length= 6., 
		    subpage_y_position = 2., 
		    page_id_line_logo_name = "c3s",
			)

title2 = mtext(
           text_lines = ["c3s logo..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")


page2 = page()

#Setting the cartesian view
projection3 = mmap( 
			page_y_position = 18.,
			page_y_length = 9.,
			page_frame = "on",
		    subpage_y_length= 6., 
		    subpage_y_position = 2., 
		    page_id_line_logo_name = "cams",
			)


title3 = mtext(
           text_lines = ["cams,..."],
		   text_justification = "left",
		   text_font_size = 0.5,
           text_colour = "charcoal")


#To the plot
plot(output, projection1, mcoast(), title1,
		page1,  projection2, mcoast(), title2,
		page2, projection3, mcoast(), title3,
		)




