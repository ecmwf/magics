
from example2jira import *

list = [ {"file" : "axis1",
          "title": "Regular Cartesian Projection...",
          "data" : []},
	 {"file" : "axis2", 
	 "title" : "Using a time serie ...",
          "data" : []},
	 {"file" : "axis3", 
	 "title" : "Using an horizontal geoline and vertical logarithmic axis...",
          "data" : []},
	 {"file" : "axis4", 
	 "title" : "Demonstrating automatic labelling of time series...",
          "data" : []}
	]

prepare("axis.json", list, "Axis examples")

put("axis.json", "Axis examples", "Axis examples")




