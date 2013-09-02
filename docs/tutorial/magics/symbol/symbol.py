
#importing Magics module
from magmacro import *

#Setting of the output file name
output = output({???})

#Setting the coordinates of the geographical area

#Background Coastlines 
background = mcoast( {???})



#Import the airep data 
airep =  mgeo({ "geo_input_file_name" : "airep.geo"})

#Define the simple contouring for msl
airep_symbol = msymb( {"symbol_type" : "marker",
			"symbol_table_mode": "advanced",
			??????
			"symbol_marker": 15 })

lines =["Monitoring of airep data"]

title = mtext({
           "text_lines" : lines,
		   ??????
           "text_justification" : "left"})

legend = ???({ "legend": "on", 
			"legend_text_colour":"black",
			"legend_display_type": "continuous"})

#To the plot
???????????














