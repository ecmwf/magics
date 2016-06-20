# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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




