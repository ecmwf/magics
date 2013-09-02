from example2jira import *

list = [         {"file" : "projection1", 
		 "title" : "Global Map",
                  "data" : []},
		 {"file" : "projection2",
		 "title" : "Europe",
                  "data" : []},
		 {"file" : "projection3",
		 "title" : "North America",
                  "data" : []},
		 {"file" : "projection4",
		 "title" : "Australia",
                  "data" : []},
		 {"file" : "projection5",
		 "title" : "New in 2.16! Lambert projection",
                  "data" : []},
		 {"file" : "projection6",
		 "title" : "New in 2.16! Mollweide projection",
                  "data" : []}
		 ]

prepare("subpages.json", list, "Subpage examples")

put("subpages.json", "Subpage examples", "Subpage examples")




