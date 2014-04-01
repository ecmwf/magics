
from Magics.macro import *

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name="tephi_mv")

projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='tephigram',
    subpage_x_automatic='on',
    subpage_y_automatic='on',
    )


tephi = mtephi()

# Curve

tempe = mnetcdf(netcdf_type = "xypoint",
                          netcdf_x_variable        =        "t",
                          netcdf_y_variable        =        "pres",
                          netcdf_filename          =     "obs.nc",
                          netcdf_missing_attribute  =   "_FILL_VALUE"
                        )

tempe_graph =  mgraph(
                legend                       = "on",
                graph_line_colour                 = "black",
                graph_line_thickness         = 8,
                graph_missing_data_mode      = "join",
                graph_missing_data_style     = "solid",
                graph_missing_data_colour    = "black",
                 graph_missing_data_thickness = 8
				)  


td = mnetcdf(netcdf_type = "xypoint",
                          netcdf_x_variable        =        "td",
                          netcdf_y_variable        =        "pres",
                          netcdf_filename          =     "obs.nc",
                          netcdf_missing_attribute  =   "_FILL_VALUE"
                        )

td_graph =  mgraph(
                legend                       = "on",
                graph_line_colour                 = "black",
                graph_line_thickness         = 8,
                graph_missing_data_mode      = "join",
                graph_missing_data_style     = "solid",
                graph_missing_data_colour    = "black",
                 graph_missing_data_thickness = 8
                )

wind = mnetcdf(
                                netcdf_type            = "vector",
                                netcdf_x_position_variable  = "xwind",
                                netcdf_y_position_variable  = "pres",
                                netcdf_x_component_variable = "u",
                                netcdf_y_component_variable = "v",
                                netcdf_filename    = "obs.nc"
                               )
wind_plot = mwind(
                     wind_field_type        =        "flags",
                     wind_flag_colour        =        "black"
                    )

plot(output,  projection, tephi, 
    tempe, tempe_graph, 
    td, td_graph, 
    wind, wind_plot, 

	)

