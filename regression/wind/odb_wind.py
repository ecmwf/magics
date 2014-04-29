from Magics.macro import *

ps = output(output_formats =  ['png'], 
        output_name = 'odb_wind', 
        output_name_first_page_number = "off");


coastlines = pcoast({'map_coastline_land_shade' : 'off',
	                 'map_coastline_land_shade_colour' : 'white',
					 'map_coastline_sea_shade' : 'off',
					 'map_coastline_sea_shade_colour' : 'white'})


odb = odb_geovectors(odb_filename= 'ascat.odb',
           odb_latitude_variable = 'lat', 
           odb_longitude_variable =  'lon',
           odb_x_component_variable =  'u',
           odb_y_component_variable =  'v',
           odb_value_variable = 'u'
        )

legend = plegend({'legend' : 'on', 
				  'legend_text_colour':'black', 
				  'legend_display_type':'continuous'})

text = ptext({'text_justification': 'left', 
			  'text_colour':'navy'})

plot(ps, odb, mwind(), coastlines,  text)
