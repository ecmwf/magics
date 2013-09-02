
from example2jira import *


list = [ {"file" : "epsboxplot", 
	 "title" : "Box Plotting example",
	  "data" : ["cloud_box.json"]},
	 {"file" : "epsrose",
	 "title" : "Rose plotting example",
	 "data"  : ["cloud_rose.json"]},
	 {"file" : "epswave",
	 "title" : "Wave plotting example",
	 "data"  : ["wave.json"]},
	 {"file" : "epswind",
	 "title" : "Wind Direction example",
	 "data"  : ["wind_direction.json"]},
	 ]

prepare("epsgrams.json", list, "Epsgram Examples")

put("epsgrams.json", "Epsgram examples", "Epsgram examples")




