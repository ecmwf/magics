
#importing Magics module
from Magics.macro import *


ref = 'handling_missing2'
#Setting of the output file name
output = output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = ref)

area = [24.,-26.,71.,85.]
area = [24.,-26.,51.,78.]
area = [24.,-26.,71.,85.]

method = "nearest"
fill = 5

#Setting the coordinates of the geographical area
projection = mmap(#subpage_map_projection = 'polar_stereographic',
                subpage_lower_left_latitude = area[0],
                subpage_lower_left_longitude = area[1],
                subpage_upper_right_latitude = area[2],
                subpage_upper_right_longitude = area[3],
                )



#Coastlines setting
coast = mcoast( map_grid =  "on",
                map_grid_colour  =  "tan",
                map_coastline_land_shade  =  'off',
                map_coastline_land_shade_colour  =  'cream',
                map_coastline_colour =  "tan")


#Import the z500 data
data =  mgrib(grib_input_file_name  = "sst.grb",
              grib_interpolation_method = method,
              grib_interpolation_method_missing_fill_count = fill,
              )

level = [-2.,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36]
levels = []
for l in level:
    levels.append(l +273.15)


#Define the simple contouring for z500
contour = mcont(  legend               = "on",
          contour                      = "off",
          contour_shade                = "on",
          contour_level_selection_type = "level_list",
          contour_level_list           = levels,
          contour_shade_colour_method  = "list",

          contour_method= 'linear',
          contour_internal_reduction_factor = 2.,
          contour_shade_colour_list    = ["RGB(0.29,0.00,0.48)","RGB(0.61,0.00,1.00)","RGB(0.80,0.47,1.00)","RGB(0.00,0.00,1.00)",
                                          "RGB(0.00,0.35,1.00)","RGB(0.00,0.55,1.00)","RGB(0.04,0.49,0.00)","RGB(0.04,0.75,0.00)",
                                          "RGB(0.04,1.00,0.00)","RGB(0.63,0.61,0.00)","RGB(0.84,0.81,0.00)","RGB(1.00,0.93,0.00)",
                                          "RGB(0.66,0.33,0.00)","RGB(0.84,0.42,0.00)","RGB(1.00,0.52,0.00)","RGB(0.75,0.04,0.00)",
                                          "RGB(1.00,0.05,0.00)","RGB(1.00,0.52,0.50)","RGB(1.00,0.84,0.83)"],
          contour_shade_method         = "area_fill",
          contour_label                = "off"
  )





title = mtext(
           text_lines = ["Handling missing data : grib_interpolation_method=%s" % method],
           text_justification = "left",
           text_font_size = 0.6,
           text_colour =  "charcoal")


#To the plot
plot(output, projection, data, contour,  coast, title)














