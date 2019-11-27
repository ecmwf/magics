
import Magics.macro as magics



# Setting the projection
projection = magics.mmap(
    subpage_map_projection        = "lambert",
    subpage_lower_left_latitude   = -0.00,
    subpage_lower_left_longitude  = -80.00,  
    subpage_upper_right_latitude  = 70.00,
    subpage_upper_right_longitude = 160.00,
    subpage_map_area_definition   = "corners",
    page_id_line                  = "off")

# Defining the coastlines
coast = magics.mcoast(
    map_coastline_resolution        = "automatic",
    map_coastline_colour            = "tan",
    map_coastline_land_shade        = "on",    
    map_coastline_land_shade_colour = "cream",
    map_grid                        = "on",
    map_grid_line_style             = "dot",
    map_grid_colour                 = "tan"
)

png = magics.output(
    output_formats = ['png'],
    output_name_first_page_number = "off",
    output_name = "my_png")

magics.plot(png, projection, coast)
