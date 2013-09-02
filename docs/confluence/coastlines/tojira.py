
from example2jira import *


list = [	 {"file" : "coastlines1", 
		 "title" : "High resolution hash coastline",
                  "data" : []},
		 {"file" : "coastlines2",
		  "title" : "Administrative boundaries, cities and rivers",
                  "data" : []},
		 {"file" : "coastlines3",
		  "title" : "Grid lines, boundaries and rivers",
                  "data" : []},
		 {"file" : "coastlines4",
		  "title" : "Sea, lakes and rivers",
                  "data" : []}
]

prepare("coast.json", list, "Coastlines examples")

put("coast.json", "Coastlines examples", "Coastlines examples")




