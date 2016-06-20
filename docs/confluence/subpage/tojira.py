# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

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




