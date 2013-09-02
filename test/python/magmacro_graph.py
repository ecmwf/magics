#importing Magics module
from Magics.macro import *


#define  the  data 
tseries_key='meanobsvalue'
tseries_values= numpy.array([4.0, 25.5, 6.8, 2., 12.4, 8.9, 3.2, 5.3])
ts_key='andate'
taxis_values=["2011-09-01 00:00", "2011-10-01 00:00","2011-11-01 00:00", "2011-12-01 00:00",
              "2012-01-01 00:00", "2012-02-01 00:00","2012-03-01 00:00", "2012-04-01 00:00"]


#Setting of the output file name
output = output({"output_formats":['ps', 'png'], 
		'output_name': 'date_graph'})


#define the cartesian projection
projection =mmap({"subpage_map_projection":"cartesian", 
	"subpage_y_position": 2. })



#define horizontal axis
horizontal= maxis({"axis_orientation":"horizontal", 
			"axis_type" : "date",
			"axis_date_type" : "months",
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_date_min_value":"2011-08-01 00:00", 
			"axis_date_max_value":"2012-08-01 00:00"})

#define vertical axis
vertical= maxis({"axis_orientation":"vertical",
			"axis_title_text": tseries_key, 
			"axis_grid" : "on",
			"axis_grid_colour": "grey",
			"axis_grid_thickness": 1,
			"axis_grid_line_style": "dot",
			"axis_tick_interval": 5.,
			"axis_min_value":0., 
			"axis_max_value":30.})



#Define the graph 
curve = mgraph( { "graph_line_colour" : "blue",
			"graph_line_thickness":2,
			"graph_symbol": "on",
			"legend_user_text": "<font colour='blue'> My legend </font>",
			"graph_symbol_marker_index": 1,
			"graph_symbol_height": 0.5,
			"graph_symbol_colour": "black",
			"graph_curve_date_x_values" : taxis_values,
			"graph_curve_y_values" : tseries_values
} )



lines =["Example date axis with python"]

title = mtext({
           "text_lines" : lines,
           "text_html" : "true",
           "text_colour" : "black",
           "text_font_size" : 0.6,
           "text_justification" : "left"})

legend = mlegend({ "legend": "on", 
			"legend_text_colour":"black"})

#To the plot
plot(output, projection, horizontal, vertical, curve,  title, legend)














