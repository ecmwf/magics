
from example2jira import *


list = [ {"file" : "contour1", 
	 "title" : "Simple contouring...",
	 "data"  : ["z500.grb"]},
	 {"file" : "contour2",
	  "title" : "Playing  with more line options...",
	 "data"  : ["z500.grb"]},
	 {"file" : "contour3",
	  "title" : "displaying the grid values..",
	 "data"  : ["t850.grb"]},
	 {"file" : "contour4",
	  "title" : "Using multiple contours...",
	 "data"  : ["t850.grb"]},
	 {"file" : "contour5",
	  "title" : "Using shading and positional legend...",
	 "data"  : ["t850.grb"]}
	 ]

prepare("cont.json", list, "Contour examples")

put("cont.json", "Contour examples", "Contour examples")




