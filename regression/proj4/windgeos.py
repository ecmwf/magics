#importing Magics module
from Magics.macro import *


import numpy


ref = 'windgeos'
#Setting of the output file name

ps = output(output_formats = ['ps'],output_name_first_page_number = "off",output_name = "%s"%(ref,))


todo = []
latref = numpy.arange(-180., 180, 60.)
#latref = [-180.]

for lat in latref:


#Setting the coordinates of the geographical area
    projection = mmap(subpage_map_projection = 'geos',
          subpage_map_vertical_longitude= lat,
          )

#Coastlines setting
    coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
                map_coastline_land_shade  =  'off',
                map_coastline  =  'on',
                map_coastline_land_shade_colour  =  'cream',
                map_coastline_colour =  "tan",
                map_coastline_style = "solid")

#Title settings
    title = mtext(
      text_lines = ["<font size='1'>Geo latitiude reference %f</font>" % (lat,)],
      text_justification = "left",
      text_font_size = 0.8,
      text_colour = "charcoal")

    z = mgrib( grib_input_file_name =  "zuv.grib", grib_field_position=1 )

    contour = mcont(
                    contour_highlight= "off",
                    contour_hilo= "off",
                    contour_line_thickness= 3,
                    contour_line_colour= "black",
                    contour_level_selection_type= "interval",
                    contour_interval= 5.,
                    legend= "on",
                    contour_label= "off",
                )
    uv = mgrib( grib_input_file_name =  "zuv.grib", 
            grib_wind_position_1 = 2,
            grib_wind_position_2 = 3)


    wind =   mwind()

#To the plot
    todo.append(projection)

    todo.append(uv)
    todo.append(wind)
    todo.append(z)
    todo.append(contour)
    todo.append(coast)

    todo.append(title)
    todo.append(page())


plot(ps, todo)










