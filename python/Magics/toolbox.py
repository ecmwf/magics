
import macro

def substitute(default, user):
	out = default
	if user != None:
		for key in user:
			out[key] = user[key]
	return out    



def graph(x,y, title="", graph = None) :

    default = {
	   "graph" : { "graph_line_colour"  : "ecmwf_blue",
                    "graph_line_thickness" : 2,
	   } 
    }

    
    x[0] = x[0]*1.
    y[0] = y[0]*1.
   

    #Setting the cartesian view
    projection = macro.mmap(subpage_map_projection = 'cartesian',
                subpage_x_automatic = 'on',
                subpage_y_automatic = 'on',
                     )
    #Vertical axis
    vertical = macro.maxis(axis_orientation = "vertical",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")

    #Horizontal axis
    horizontal = macro.maxis(axis_orientation = "horizontal",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")

    #define the input data
    input = macro.minput(input_x_values =  x,
                input_y_values =  y)

    #Define the graph 
    graph = macro.mgraph( substitute(default["graph"], graph)
                )
    #Define the title
    title = macro.mtext(text_lines = [title],
                  text_font_size = 0.8,
                  text_justification = "left"
                )
    return macro.plot(projection, vertical, horizontal, input, graph, title)


colour = "ecmwf_blue"
font_size = 0.35
defaults = { "eps" : 
		{
		   "projection" : {
	        	"subpage_map_projection" : 'cartesian',
	        	"subpage_x_axis_type" : 'date',
	        	"subpage_x_automatic" : 'on',
	        	"subpage_y_axis_type" : 'regular',
	            "subpage_y_automatic" : 'on',
		   },
	       "vertical_axis" :  { 
	            "axis_orientation" : "vertical",
	            "axis_grid" : "on",
	            "axis_grid_colour" : "navy",
	            "axis_grid_line_style" :"dash",
	            "axis_grid_reference_level" :  0.,
	            "axis_grid_reference_thickness" : 1,
	            "axis_line" :  "on",
	            "axis_line_colour" : "navy",
	            "axis_tick_colour" : "navy",
	            "axis_tick_label_colour" : "navy",
	            "axis_tick_label_height":  font_size
	            },
	        "horizontal_axis" :  { 
	            "axis_orientation" : "horizontal",
	            "axis_date_type" : "days",
	            "axis_days_label" : "both",
	            "axis_days_label_colour" :  "navy",
	            "axis_days_label_height" : font_size,
	            "axis_grid" :  "on",
	            "axis_grid_colour" : "navy",
	            "axis_grid_line_style" : "dash",
	            "axis_line_colour" : "navy",
	            "axis_minor_tick" : "off",
	            "axis_minor_tick_colour=" : "navy",
	            "axis_months_label" : "off",
	            "axis_tick_colour" : "navy",
	            "axis_type" :  "date",
	            "axis_years_label" : "off"
	            },
	        "epsgraph" : {
	            "eps_box_border_thickness" : 2,
	            "eps_box_width" : 1.5,
	            "eps_box_colour" : colour,
	            "eps_box_forecast_colour" : "black",
	            "eps_font_colour" :"navy",
	            "eps_legend_font_size" :  font_size,
	            "eps_grey_legend" : "off",
	            "legend" :'off'
	        },
	        "epsclim" : {
	           "eps_shade_colour": colour,
	           "eps_shade_line_thickness": 4,
	        }
    	}
    }

def epsgram(parameter, input, **args):
    
    print parameter, input, "ARGS--->", args
    
    actions = []
   
    projection = macro.mmap( substitute(defaults["eps"]["projection"], args.get("projection", None)) )

    # define horizontal axis
    horizontal = macro.maxis(substitute(defaults["eps"]["horizontal_axis"], args.get("horizontal_axis", None)))  
    vertical = macro.maxis(substitute(defaults["eps"]["vertical_axis"], args.get("vertical_axis", None)))
   
    
    data = macro.mwrepjson(
                            wrepjson_family =  "eps",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_parameter_information =  args.get("title", parameter),
                            wrepjson_parameter_scaling_factor = 1.,
                        )

    graph = macro.mepsgraph(substitute(defaults["eps"]["epsgraph"], args.get("epsgraph", None)) )
    
    actions.append(projection)
    actions.append(vertical)
    actions.append(horizontal)
    
    print "DATA", input
    
    if args["clim"] : 
    	print "FILE--->", data
    	clim = macro.mwrepjson(
                            wrepjson_family =  "clim",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_parameter_information =   args.get("title", parameter),           
                            wrepjson_parameter_scaling_factor = 1.,
                            wrepjson_ignore_keys = "100"
                        )
        shade = macro.mepsshading(substitute(defaults["eps"]["epsclim"], args.get("epsclim", None)) )
        actions.append(clim)
        actions.append(shade)
    
    actions.append(data)
    actions.append(graph)           


    text = macro.mtext(
                    text_colour =  "navy",
                    text_font_size = font_size*2,
                    text_justification =  "left",
                    text_lines =  ["EPS Meteogram <json_info key='expver'/>",
                    "<json_info key='station_name'/><json_info key='location'/><json_info key='grid_point'/><json_info key='height'/>",
                    "<json_info key='product_info'/><json_info key='date'/>",
                    "<font size='0.5' colour='white'>.</font>",
                    "<json_info key='parameter_info'/>",]

                )

    actions.append(text)
    
    if args["output"] != "" :
    	#Setting of the output file name
		png = macro.output(output_formats = ['png'], 
			output_name_first_page_number = "off",
			output_name = args["output"])

		return macro._plot(
			png,
            actions
    	)	

    return macro.plot(
            actions
    )

def epsclimgram(**kw):

	print kw
	args = {"clim" :True }
	args.update(kw)
	print args

	return epsgram(**args)

