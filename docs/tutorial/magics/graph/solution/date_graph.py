
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
			"axis_type" : "date",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_date_min_value":"2011-03-01 12:00:00", 
			"axis_date_max_value":"2011-03-03 12:00:00"})

#define vertical axis
vertical= maxis({"axis_orientation":"vertical",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_min_value":10., 
			"axis_max_value":30.})


#define  the  data 
x =  ["2011-03-02 00:00:00","2011-03-02 12:00:00","2011-03-03 00:00:00"]
y_min = numpy.array([12.,17.,15.])
y_max = numpy.array([22.,21.,17.])



#Define the graph 
min = mgraph( { "graph_line_colour" : "blue",
			"graph_line_thickness":8,
			"graph_symbol": "on",
			"legend": "on",
			"legend_user_text": "<font colour='blue'> min </font>",
			"graph_symbol_marker_index": 1,
			"graph_symbol_height": 0.5,
			"graph_symbol_colour": "black",
			"graph_curve_date_x_values" : x,
			"graph_curve_y_values" : y_min
} )

#Define the graph 
max = mgraph( { "graph_line_colour" : "red",
			"graph_line_thickness":8,
			"graph_symbol": "on",
			"legend": "on",
			"legend_user_text": "<font colour='red'> max </font>",
			"graph_symbol_marker_index": 1,
			"graph_symbol_colour": "black",
			"graph_symbol_height": 0.5,
			"graph_curve_date_x_values" : x,
			"graph_curve_y_values" : y_max
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
plot(output, projection, horizontal, vertical, min, max, title, legend)














