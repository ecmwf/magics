# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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




