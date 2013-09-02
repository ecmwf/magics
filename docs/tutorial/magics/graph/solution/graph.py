
#importing Magics module
from Magics.macro import *

#Setting of the output file name
output = output({"output_formats":['ps', 'png'], 
		'output_name': 'graph'})


#define the cartesian projection
projection =mmap({"subpage_map_projection":"cartesian", 
	"subpage_y_length": 14.,
	"subpage_y_position": 1.5 })



#define horizontal axis
horizontal= maxis({"axis_orientation":"horizontal", 
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_min_value":-50., 
			"axis_max_value":50.})

#define vertical axis
vertical= maxis({"axis_orientation":"vertical",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_min_value":0., 
			"axis_max_value":100.})

#define the data for the boxplot
pos=numpy.array([-25.,0.,25.])
min=numpy.array([5.,10.,7.])
max=numpy.array([95.,80.,92.])
upper=numpy.array([80.,72.,81.])
lower=numpy.array([12.,18.,30.])
median=numpy.array([45.,52.,47.])


box=mboxplot({ "boxplot_positions" :pos,
			"boxplot_minimum_values" : min,
			"boxplot_maximum_values" : max,
			"boxplot_box_upper_values" : upper,
			"boxplot_box_lower_values" : lower,
			"boxplot_median_values" : median
	})


#define  the  data 
x = numpy.array([-25.,0.,25.])
y = numpy.array([5.,25.,75.])


#Define the graph 
graph = mgraph( { "graph_line_colour" : "red",
			"graph_line_thickness":8,
			"graph_symbol": "on",
			"legend": "on",
			"legend_user_text": "<font colour='red'> My red curve </font>",
			"graph_symbol_marker_index": 1,
			"graph_symbol_height": 0.5,
			"graph_curve_x_values" : x,
			"graph_curve_y_values" : y
} )


lines =["My Graph"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_font_size" : 0.6,
           "text_mode": "positional",
           "text_box_x_position": 1.5,
           "text_box_y_position": 16.5,
           "text_box_x_length": 20.,
           "text_box_y_length": 2.5,
           "text_border": "off",
           "text_justification" : "left"})

legend = mlegend({ "legend": "on", 
			"legend_text_colour":"black"})

#To the plot
plot(output, projection, horizontal, vertical, box, graph, title, legend)














