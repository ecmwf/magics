# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module

from Magics.macro import *





ref = 'symbol_ww'

# Setting of the output file name

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name=ref)

# Setting the coordinates of the geographical area
# Here we use Europe

area = mmap(subpage_map_projection='cartesian',
		subpage_frame='off', 
        subpage_x_max=100.,
        subpage_x_min=0.,
        subpage_y_max=0.,
        subpage_y_min=120.,
        page_y_length= 21.,
        subpage_y_length= 21.)
        
lines = ['Symbol List', "<font size='0.5'> [Note: Only few symbols can be outlined]</font>", ""]

text = mtext(text_lines=  lines,
    text_html= 'true',
    text_justification= 'centre',
    text_font_size= 1.,
    text_colour= 'charcoal',
    )
    
out = []

for p in range(0,9) :
	out.append(area)
	#list.append(title)
	y = 0.
	x = 30.
	sizes = [1.,]

	for i in range(0,20) :
		
		if (i % 10) == 0:
			y = 0.
			x += 45.
			
		if (i % 20) == 0:
			y = 0.
			x = 30.        
			out.append(page())
			out.append(area)  
			out.append(text)     
			
			
		name = "ww_%02d" % ( i + p*10)
		print name
			
		symbol = "symbol = %s"% name
	  
		xx = x
	   
		for size in sizes:
			title = "symbol = %s"% name
		# Import the input data
			out.append(minput(input_x_values=[ xx],
					input_y_values=[y],
				))

		# Define the symbol plotting
			out.append(msymb(
				legend='off',
				symbol_type='marker',
				symbol_colour='evergreen',
				symbol_height=size,
				symbol_marker_mode = "name",
				symbol_marker_name = name,
				symbol_text_position='left',
				symbol_text_font_size=0.5,
				symbol_text_font_colour='black',
				symbol_text_list=[title],
				
				))
			xx+=15
		y += 10. 
	out.append(page())
			
		






	# To the plot

plot( output,  out)

