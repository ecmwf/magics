
from example2jira import *


list = [ {"file" : "symbol1", 
	 "title" : "Big red dots...",
	  "data" : ["input.geo"]},
	 {"file" : "symbol7",
	 "title" : "Simple Symbol plotting in cartesian",
	 "data"  : []},
	 {"file" : "symbol2",
	 "title" : "Using cloud symbol",
	 "data"  : ["input.geo"]},
	 {"file" : "symbol3",
	 "title" : "Using Shower symbol",
	 "data"  : ["input.geo"]},
	 {"file" : "symbol4",
	 "title" : "Using pictogram",
	 "data"  : ["D96.png"]},
	 {"file" : "symbol5",
	 "title" : "Attach a text to a symbol",
	 "data"  : []},
	 {"file" : "advanced",
	 "title" : "Using Advanced table mode",
	 "data"  : ["airep.geo"]},
	 {"file" : "geo_wind",
	 "title" : "Using Wind as Symbol on a geographical area",
	 "data"  : []},
	 {"file" : "xy_wind",
	 "title" : "Using Wind as Symbol on a Cartesian projection",
	 "data"  : []}
	 ]

prepare("symbols.json", list, "Symbol examples")

put("symbols.json", "Symbol examples", "Symbol examples")




