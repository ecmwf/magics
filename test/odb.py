from Magics.macro import *

ps = output({'output_formats': ['png','ps'], 'output_name':'myview'});


coastlines = pcoast({'map_coastline_land_shade' : 'off',
	                 'map_coastline_land_shade_colour' : 'white',
					 'map_coastline_sea_shade' : 'off',
					 'map_coastline_sea_shade_colour' : 'white'})


odb = odb_geopoints({'odb_filename' : 'test.odb',
           'odb_latitude_variable' :'lat@hdr', 
           'odb_longitude_variable' : 'lon@hdr',
           'odb_value_variable': 'lat@hdr'
        })

symbol = psymb({'symbol_table_mode' : 'off', 
                'symbol_colour'     : 'blue',
                'symbol_height'     : 0.16,
                'symbol_marker'     : 15,
               'symbol_type':'marker'})
			     

legend = plegend({'legend' : 'on', 
				  'legend_text_colour':'black', 
				  'legend_display_type':'continuous'})

text = ptext({'text_justification': 'left', 
			  'text_colour':'navy'})

plot(ps, odb, symbol, coastlines,  text)
