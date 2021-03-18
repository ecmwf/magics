# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



from Magics.macro import *
#setting the output
output = output(
                output_formats = ['png'],
                output_name = "xsect_wind",
                output_name_first_page_number = "off"
        )
# Setting the cartesian view
projection = mmap(
        page_id_line = 'off',
        subpage_map_projection='cartesian',
        subpage_x_axis_type='geoline',
        subpage_y_axis_type='logarithmic',
        subpage_x_min_longitude= -180.,
        subpage_y_min= 1000.,
        subpage_x_max_longitude= 180.,
        subpage_y_max= 300.,
        )
# Vertical axis
vertical = maxis(
        axis_orientation='vertical',
        axis_grid='on',
        axis_type='logarithmic',
        axis_tick_label_height=0.4,
        axis_tick_label_colour='charcoal',
        axis_grid_colour='charcoal',
        axis_grid_thickness=1,
        axis_grid_reference_line_style='solid',
        axis_grid_reference_thickness=1,
        axis_grid_line_style='dash',
        axis_title='on',
        axis_title_text='Pressure',
        axis_title_height=0.6,
        )
# Horizontal axis
horizontal = maxis(
        axis_orientation='horizontal',
        axis_tick_label_height=0.4,
        axis_type='geoline',
        axis_tick_label_colour='charcoal',
        axis_grid='on',
        axis_grid_colour='charcoal',
        axis_grid_thickness=1,
        axis_grid_line_style='dash',
        )
# Definition of the netCDF data and interpretation
data = mnetcdf(netcdf_filename = "xsect_wind.nc",
      netcdf_type = "matrix",
      netcdf_value_variable = "u",
  netcdf_x_component_variable = "u",
  netcdf_y_component_variable = "v",
  netcdf_y_variable = "v_lev",
  netcdf_x_variable = "lon",
  netcdf_x_auxiliary_variable = "lat"
    )
wind = mwind()
plot(output, projection, horizontal, vertical, data, wind)
