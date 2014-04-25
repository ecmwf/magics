from csv import reader
from datetime import datetime
from Magics.macro import *

#read input file
f= file('boxinfo.data','rb')
r= reader(f,delimiter=' ')
data= {}
for text, val1, val2, val3, val4 in r:
    print text
    print val1
    print val2
    print val3
    data[text] = [float(val1), float(val2), float(val3), float(val4)]
    
    
f.close()

print data
ref = 'symbol_box'

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
        
lines = ['Information', "<font size='0.5'>Example </font>", ""]

text = mtext(text_lines=  lines,
    text_html= 'true',
    text_justification= 'centre',
    text_font_size= 1.,
    text_colour= 'charcoal',
    )
    
out = []

out.append(area)  
out.append(text)

xx = 10.
y = 15.

for d in data :
	print d
	
	
	
	out.append(minput(input_x_values=[ xx],
			input_y_values=[y],
	))

	# Define the symbol plotting
	out.append(msymb(
				legend='off',
				symbol_type='text',				
				symbol_text_font_size=0.8,
				symbol_text_font_colour='black',
				symbol_text_list=[d],			
				))
	out.append(minput(input_x_values=[xx + 8 ,xx + 18, xx + 28, xx + 38],
			input_y_values=[y-5,y-5,y-5,y-5],
		))
	
	out.append(msymb(
				legend='off',
				symbol_type='text',				
				symbol_text_font_size=0.6,
			
				symbol_text_font_colour='black',
				symbol_text_list=["JFM", "AMJ", "JAS", "OND"],		
				))
				
	out.append(minput(input_x_values=[xx + 10, xx + 20, xx + 30, xx + 40],
			input_y_values=[y,y,y,y],
			input_values=data[d],
		))
		
	# Define the symbol plotting
	out.append(msymb(
				legend='off',
				symbol_type='marker',	
				symbol_height=1.,		
				symbol_marker_index = 18,
				symbol_text_position='top',
				symbol_text_font_size=0.5,
				symbol_text_font_colour='black',
				
				symbol_outline="on",
				symbol_outline_colour="navy",
				symbol_outline_thickness=3,
				symbol_table_mode = "advanced",
				symbol_advanced_table_selection_type = "list",
				symbol_advanced_table_level_list = [0.00, 1.00, 2.00, 3.00, 4.00, 5.00, 6.00],
				symbol_advanced_table_colour_method = "list",
				symbol_advanced_table_colour_list = ["orange", "blue", "red", "green ", "yellow", "cyan"],
				symbol_advanced_table_marker_list = [18],
				symbol_advanced_table_height_method = "list",
				symbol_advanced_table_height_list = [1.00]
				))
			
				
	y += 15. 
			
plot( output,  out)
