# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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




