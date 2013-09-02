
from Magics.macro import *

output = output(output_formats=['ps'],
                output_name_first_page_number='off',
                output_name="tephi")

newpage = page(page_x_length=12., page_y_length=12., page_id_line='off',
               subpage_y_length=10., subpage_x_length=10.)
projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='tephigram'
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

infobox = mmap(
    subpage_x_position=0.05,
    subpage_x_length=2.,
    subpage_map_projection='tephiinfo'
    )
vertical = maxis(
    axis_orientation='vertical',
    )

# Horizontal axis

horizontal = maxis(
    axis_orientation='horizontal',
    )    
wind = minput(
                input_x_values = [0., 0.5, 0., 0.5],
				input_y_values = [1000., 800., 600., 300.],
				input_values = [1000., 800., 600., 300.]
                )    
wind_graph =  msymb(
			    symbol_type="marker",
			    symbol_marker_index=15,
			    symbol_colour="red",
				graph_line_colour="red",
                graph_line_thickness=4, 
				)                 
                
plot(output, newpage, projection, tephi, tempe, tempe_graph, 
    newpage, infobox,  wind, wind_graph,
    
	)
