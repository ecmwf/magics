
#importing Magics module
from ???? import *

#Setting of the output file name
output = output({???})

#Setting the coordinates of the geographical area
#Here we use australia
australia = mmap({???})


#Background Coastlines 
background = ???( {???})

#Foreground Coastlines
foreground = ???( { ???? })

#Import the z500 data 
msl =  mgrib({ "grib_input_file_name" : "msl.grb"})

#Define the simple contouring for msl
msl_contour = ???({ ??? })


#Import the  wind speed at 200hPa speed200 
speed200 =  mgrib({ ??? })


#Define the shading for the wind speed
speed200_contour = mcont({ ???})


#Import the  wind  at 200hPa uv200 
uv200 =  mgrib({ "uv200.grb" })

uv200_wind = mwind({ ???})

lines =["My own title"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_mode": "positional",
           "text_box_x_position": 1.5,
           "text_box_y_position": 12.,
           "text_box_x_length": 20.,
           "text_box_y_length": 2.5,
           "text_border": "on",
           "text_border_colour": "black",
           "text_box_blanking": "on",
           "text_justification" : "left"})

#add a legend
legend = mlegend({"legend":"on",
           "legend_text_colour":"black",
           "legend_box_mode": "positional",
           "legend_box_x_position": 27.,
           "legend_box_y_position": 0.75,
           "legend_box_x_length": 2.,
           "legend_box_y_length": 12.,
           "legend_border": "on",
           "legend_border_colour": "black",
           "legend_box_blanking": "on",
           "legend_display_type": "continuous" })

#To the plot
plot(???)
