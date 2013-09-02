
from example2jira import *


list = [ {"file" : "odb1", 
	 "title" : "Using odb_geopoint",
	 "data"  : ["data.odb"]},
	 {"file" : "odb2",
	  "title" : "Using numpy arrays.",
	  "data"  : ["data.odb"]},
	 ]

prepare("odb.json", list, "Odb Visualisation example")

put("odb.json", "Odb Visualisation example", "odb examples")




