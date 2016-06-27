# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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




