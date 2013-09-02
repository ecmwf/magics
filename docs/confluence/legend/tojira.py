 
from example2jira import *


list = [ {"file"  : "legend1",
	  "data"  : ["z500.grb", "t850.grb"],
          "title" : "Vertical legend with user defined text"},
	 {"file"  : "legend2",
          "data"  : ["t850.grb"],
	  "title" : "Disjoint legend with user defined labels"},
	 {"file"  : "legend3",
          "data"  : ["z500.grb", "t850.grb"],
         "title"  : "Positional and histogram legend"} ]

prepare("legend.json", list, "Legend examples")

put("legend.json", "Legend examples", "Legend examples")




