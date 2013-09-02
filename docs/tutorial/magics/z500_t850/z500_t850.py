
#importing Magics module
from magmacro import *

#Setting of the output file name
#Plesse try to add a PNG 
output = output({"output_formats":[????], 
			'output_name':'z500_t850_europe'})

#Setting the coordinates of the geographical area
#Take the Europe area

europe = mmap({?????????????})

#Coastlines setting
coast = pcoast({????????????})

#Import the z500 data 
z500 =  mgrib({ "grib_input_file_name" : "z500.grb",
                "grib_id": "z500"})

#Define the simple contouring for z500
z500_contour = mcont({
                #setup the visual attributes
                })

#Import the t850 data 
t850 =  mgrib({ 
			   # import z500.grb
                })


#Define the shading for t850
t850_contour = mcont({
                #setup the visual attributes
                 "contour_shade_colour_list": ["rgb(0,0,0.5)","rgb(0,0,0.5)",
						"rgb(0,0,0.85)","rgb(0,0,0.5)","rgb(0,0,0.85)","rgb(0.25,0,1)",
					    "blue_purple","greenish_blue","blue_green","bluish_green",
						"yellow_green","greenish_yellow","yellow",
						"orangish_yellow","orange_yellow","yellowish_orange",
						"orange","reddish_orange","red_orange","orangish_red",
						"red","magenta","magenta","magenta"]
         })

lines = ["<grib_info id='z500' key='base-date' format='Base Date:%A %d %B %Y at %H UTC'/>",
             "<grib_info id='z500' key='valid-date' format='Valid Date:%A %d %B %Y at %H UTC'/>",
             "parameter:<grib_info id='z500' key='name'/> at <grib_info id='z500' key='level'/>",
             "parameter:<grib_info id='t850' key='name'/> at <grib_info id='t850' key='level'/>"]

title = mtext({
		   # set the text!
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
		   #Position the text
           "text_mode": "positional",
           "text_box_x_position": ??????,
           "text_box_y_position": ???????,
           "text_box_x_length": ???,
           "text_box_y_length": ?????,
           "text_border": "on",
           "text_box_blanking": "on",
           "text_border_colour": "black",
           "text_justification" : "left"})

#add a legend
legend = mlegend({"legend":"on",
          "legend_display_type": "continuous" 
		#Position the legend!
			})

#To the plot
plot(??????????????)

