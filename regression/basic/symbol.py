#!/usr/bin/env python
# importing Magics module

from Magics.macro import *

ref = 'symbol'

# Setting of the output file name

output = output(output_formats=['png'],
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
out.append(area)
#list.append(title)
y = 0.
x = 30.
sizes = [1.,]

for i in range(0,30) :
    if (i % 15) == 0:
        y = 0.
        x += 45.
        
    if (i % 50) == 0:
        y = 0.
        x = 30.        
        out.append(page())
        out.append(area)  
        out.append(text)     
        
        
        
    symbol = "symbol_marker_index = %i"% i
  
    xx = x
   
    for size in sizes:
    	title = "symbol_marker_index = %i"% i
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
	        symbol_text_position='left',
	        symbol_text_font_size=0.5,
	        symbol_text_font_colour='black',
	        symbol_text_list=[title],
            symbol_marker_mode='index',
            symbol_marker_index=i,
            symbol_outline="on",
            symbol_outline_colour="navy",
            symbol_outline_thickness=3,
            ))
        xx+=15
    y += 10. 
        
    






# To the plot

plot( output,  out, text)

