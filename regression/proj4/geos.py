# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#importing Magics module
from Magics.macro import *


import numpy


ref = 'geos_rotate'
#Setting of the output file name

ps = output(output_formats = ['ps'],output_name_first_page_number = "off",output_name = "%s"%(ref,))


todo = []
latref = numpy.arange(-180., 180, 20.)
#latref = [-180.]

for lat in latref:
    print "+proj=geos +h=42164000 +ellps=WGS84 +lon_0=%f +units=meters" % (lat,)


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

    uv = mgrib( grib_input_file_name =  "uv.grib" )

    uv_contour = mcont(
                    contour_highlight= "off",
                    contour_hilo= "off",
                    legend= "on",
                    contour_label= "off",
                    contour_legend_text= "Contour shade (Range: 30 / 100, green)",
                    contour_level_list= [30., 40., 50., 60., 70., 80., 90., 100],
                    contour_level_selection_type= "level_list",
                    contour_reference_level= 0.,
                    contour_shade= "on",
                    contour_shade_max_level_colour= "evergreen",
                    contour_shade_method= "area_fill",
                    contour_shade_min_level= 30.,
                    contour_shade_min_level_colour= "yellow"
                )
    msl = mgrib( grib_input_file_name =  "msl.grib" )

    msl_contour =   mcont(
                    contour_highlight_colour= "black",
                    contour_hilo= "off",
                    contour_interval= 5.,
                    contour_label= "off",
                    contour_label_frequency= 2,
                    contour_label_height= 0.4,
                    contour_legend_text= "Interval 5, thickness 2",
                    contour_level_selection_type= "interval",
                    contour_line_colour= "black"
                )


#To the plot
    todo.append(projection)

    todo.append(msl)
    todo.append(msl_contour)
    todo.append(uv)
    todo.append(uv_contour)
    todo.append(coast)

    todo.append(title)
    todo.append(page())


plot(ps, todo)










