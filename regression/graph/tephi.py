
from Magics.macro import *

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name="tephi")

projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='tephigram',
    subpage_x_automatic="on",
    subpage_y_automatic="on",

    )


# Tephigram grid
tephi = mtephi( tephigram_isotherm_grid_colour = "magenta",
				tephigram_isotherm_grid_thickness = 5,
				tephigram_isotherm_label_font_style= "bold",
				tephigram_isotherm_label_colour= "red",
				tephigram_isotherm_label_frequency= 5,
				tephigram_isotherm_label_font_size= 0.5)


tephi = mtephi()

# Curve

tempe = minput(
                input_x_values = [10., 25., 30.],
				input_y_values = [1000., 800., 600.]
                )

tempe_graph =  mgraph(
				graph_line_colour="red",
                graph_line_thickness=4, 
				)  

tempe2 = minput(
                input_x_values = [14., 29., 44.],
				input_y_values = [1000., 800., 600.]
                )

dot_graph =  mgraph(
				graph_line_colour="red",
                graph_line_thickness=4, 
                graph_line_style="dash", 
				)  

wind = minput(
                input_x_values = [1000.5, 1000.5, 1000.5, 1000.5],
				input_y_values = [1000., 800., 600., 300.],
				input_x_component_values = [20., -20., 0., 0.],
				input_y_component_values = [0., 0., 20., -20.]
                )    
dot =  msymb(
			    symbol_type="marker",
			    symbol_marker_index=15,
			    symbol_colour="red",
				graph_line_colour="red",
                graph_line_thickness=4, 
				)                 
                
t = minput(
                input_x_values = [1000.5, 1000.5, 1000.5, 1000.5],
				input_y_values = [900., 700., 500., 400.],
                )
wind = minput(
                input_x_values = [1000.5, 1000.5, 1000.5, 1000.5],
				input_y_values = [1000., 800., 600., 300.],
				input_x_component_values = [20., -20., 0., 0.],
				input_y_component_values = [0., 0., 20., -20.]
                )    

wind_nc =  mnetcdf( netcdf_type = "vector",
        netcdf_filename = "thermoBufr.nc",
        netcdf_x_component_variable = "u",
        netcdf_y_component_variable = "v ",
        netcdf_y_position_variable = "pres",
        netcdf_x_position_variable = "xwind"
            )

dot =  msymb(
			    symbol_type="marker",
			    symbol_marker_index=15,
			    symbol_colour="red",
				graph_line_colour="red",
                graph_line_thickness=4, 
				)                 
                
plot(output,  projection, tephi, 
    tempe, tempe_graph, 
    tempe2, dot_graph, 
    t, dot,
    wind, mwind(),
	)

