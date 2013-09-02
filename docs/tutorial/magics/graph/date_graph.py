
#importing Magics module
from magmacro import *

#Setting of the output file name
output = ???


#define the cartesian projection
projection = ???



#define horizontal axis
horizontal= maxis({"axis_orientation":"???", 
			"axis_type" : "???",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"???":"2011-03-01 12:00:00", 
			"???":"2011-03-03 12:00:00"})

#define vertical axis
vertical= maxis({"axis_orientation":"vertical",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"???":10., 
			"???":30.})


#define  the  data 
x =  ["2011-03-02 00:00:00","2011-03-02 12:00:00","2011-03-03 00:00:00"]
y_min = numpy.array([12.,17.,15.])
y_max = numpy.array([22.,21.,17.])



#Define the graph 
min = mgraph( { "graph_line_colour" : "blue",
			"graph_line_thickness":8,
			"graph_symbol": "on",
			"legend_user_text": "<font colour='blue'> min </font>",
			"graph_symbol_marker_index": 1,
			"graph_symbol_height": 0.5,
			"graph_symbol_colour": "black",
			"???date???" : ???, 
			"graph_curve_y_values" : y_min
} )

#Define the graph 
max = mgraph( { ???? } )




lines =["My Graph"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_font_size" : 0.6,
           "text_mode": "positional",
           "text_box_x_position": 1.5,
           "text_box_y_position": 15.5,
           "text_box_x_length": 20.,
           "text_box_y_length": 2.5,
           "text_border": "off",
           "text_justification" : "left"})

legend = mlegend({ "legend": "on", 
			"legend_text_colour":"black"})

#To the plot
plot(????)














