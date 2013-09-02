
from example2jira import *

list = [
	{"file" : "wind1",  "title" : "Wind arrows with legend",                  "data" : ["msl.grb","speed200.grb","uv200.grb"]},
	{"file" : "wind2",  "title" : "Wind flags where speed is over 20 m/s",    "data" : ["speed200.grb","uv200.grb"]},
	{"file" : "wind3",  "title" : "Wind arrow colour is a function of speed", "data" : ["uv200.grb"]},
	{"file" : "wind4",  "title" : "Vorticity and wind flags",                 "data" : ["wind.grib","vorticity.grib"]}
	]

prepare("wind.json", list, "Wind examples")

put("wind.json", "Wind examples", "Wind examples")




