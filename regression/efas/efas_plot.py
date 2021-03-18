from Magics.toolbox import *
from Magics.macro import *



png =output(output_formats = ['png'],
           output_name_first_page_number = 'off',
           output_name = 'efas_plot')


#Setting the coordinates of the geographical area
area = { "subpage_upper_right_longitude" : 40.,
         "subpage_upper_right_latitude"  :  75.,
         "subpage_lower_left_longitude"  : -10.,
         "subpage_lower_left_latitude"   : 35.
        }

area2 = { "subpage_map_projection" : "polar_stereographic", 
         "subpage_lower_left_longitude" : -17.,
         "subpage_upper_right_longitude" : 45.00,
         "subpage_upper_right_latitude" : 60.,
         "subpage_lower_left_latitude" : 30.}

data = mnetcdf(netcdf_type = "geomatrix",
  netcdf_filename = "efas.nc",
  netcdf_value_variable = "accumulation",
  netcdf_longitude_variable = "x",
  netcdf_latitude_variable = "y",
  #netcdf_dimension_setting = ["leadtime:17100"],
  netcdf_missing_attribute = "_fillvalue"
)


shading = { "contour" : "off",
            "contour_level_selection_type" : "level_list",
            "contour_level_list" :  [0.001, 0.2, 0.5, 0.8, 1, 1.5, 2, 5, 10.00],
            "contour_shade" : "on",
            "contour_label" : "off",
            "contour_min_level" : 0.001,
            "legend" : "on",
            "contour_shade_method" : "area_fill",
            "contour_shade_max_level_colour" : "rgb(1,0,0.13)",
            "contour_shade_min_level_colour" : "rgb(0,0.02,0.62)",
            "contour_shade_colour_direction" : "clockwise"
          }

geoplot(data, contour=shading, output=png, area=area, )



