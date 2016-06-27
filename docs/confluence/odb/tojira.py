# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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




