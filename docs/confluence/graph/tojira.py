
from example2jira import *


list = [ {"file" : "graph1", 
		 "title" : "Simple graph with legend...",
                  "data" : []},
		 {"file" : "graph5", 
		 "title" : "2 curves on the same plot ...",
                  "data" : []},
		 {"file" : "graph2", 
		 "title" : "Simple Bar Plotting ...",
                  "data" : []},
		 {"file" : "graph3", 
		 "title" : "Bar Justification ...",
                  "data" : []},
		 {"file" : "graph4", 
		 "title" : "Bar Style and Annotations...",
                  "data" : []},
		 {"file" : "graph6", 
		 "title" : "Plotting Flags...",
                  "data" : []},
		 {"file" : "graph7", 
		 "title" : "Plotting Arrows...",
                  "data" : []},
		 {"file" : "graph8", 
		 "title" : "Layout and Graph...",
                  "data" : []},
		 {"file" : "graph9", 
		 "title" : "Shaded Bars example..",
                  "data" : []},
		 {"file" : "graph10", 
		 "title" : "Simple graph with Symbol and outline ( avalaiable from Magics2.20)...",
                  "data" : []},
		 {"file" : "graph11", 
		 "title" : "Simple graph and boxplot ",
                  "data" : []}


		 ]

prepare("graph.json", list, "Graph examples")

put("graph.json", "Graph examples", "Graph examples")




