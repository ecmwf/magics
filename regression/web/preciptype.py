
#importing Magics module
from Magics.macro import *


'''
Precipitation type (0-8) uses WMO Code Table 4.201
Values of ptype defined in the IFS:
0 = No precipitation
1 = Rain (60 green)
3 = Freezing rain (i.e. supercooled) (66 - red)
5 = Snow (70 - navy)
6 = Wet snow (i.e. starting to melt) (68 - turquoise)
7 = Mixture of rain and snow (68 - blue)
8 = Ice pellets (79 - yellow)
'''

ref = 'preciptype'
#Setting of the output file name
output = output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = ref)

#Setting the coordinates of the geographical area
projection = mmap(subpage_map_projection = 'cylindrical')


#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
                map_coastline_land_shade  =  'on',
                map_coastline_land_shade_colour  =  'cream',
                map_coastline_colour =  "tan")


#Import the z500 data
precip =  mgrib(grib_input_file_name  = "fc_type.grib")


#Define the simple contouring for z500
contour = mcont(contour_level_selection_type = "level_list",
  contour_level_list = [1.00, 2.00, 3.00, 4.00, 5.00, 6.00, 8.00, 9.00],
  contour_shade = "on",
  contour_shade_technique = "marker",
  contour_shade_colour_table = ["green", "none", "green", "none", "evergreen", "green", "evergreen"],
  contour_shade_height_table = [0.10],
  contour_shade_marker_table = ["ww_60", "none", "ww_66", "none", "ww_70", "ww_68", "ww_79"]
)

symbol = msymb(legend='on', symbol_table_mode = "advanced",
    symbol_type="marker",
    symbol_advanced_table_selection_type = "list",
    symbol_marker_mode = "name",
    symbol_advanced_table_colour_method = "list",
    symbol_advanced_table_colour_list = ["green", "none", "red", "none", "navy", "turquoise", "orange_yellow"],
    symbol_advanced_table_marker_name_list =  ["ww_60", "none", "ww_66", "none", "ww_70", "ww_68", "ww_79"],
    symbol_advanced_table_level_list = [1.00, 2.00, 3.00, 4.00, 5.00, 6.00, 8.00, 9.00],
    )



title = mtext(
           text_lines = ["Advanced Symbol plotting using ww markers ",
           "    use to show the precipitations type"],
           text_justification = "left",
           text_font_size = 0.8,
           text_colour =  "charcoal")

legend = mlegend(legend_user_lines = ["Rain", "Freezing rain", "Snow", "Wet snow", "Ice pellets"],
  legend_text_composition = "user_text_only", 
  legend_text_colour = "charcoal", 
  legend_text_font_size = 0.5, 
  legend_symbol_height_factor = 4. )


#To the plot
plot(output, projection, coast, precip, symbol, title, legend)














