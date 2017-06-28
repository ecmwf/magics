

import json
import datetime
import pprint
import numpy as np 

with open('2t.json') as json_data:
    d = json.load(json_data)




data = d["2t"]

print d["date"]
start = datetime.datetime.strptime(d["date"], "%Y%m%d")
start = start + datetime.timedelta(hours=48)
print start.strftime("ixxx%Y%m%d")

steps =  map(lambda x : start + datetime.timedelta(hours=int(x)), data["steps"])
steps =  map(lambda x : x.strftime("%Y-%m-%d %H:%M"), steps)
val =   map(lambda x : x -273.15, data["median"])
np.set_printoptions(precision=2)

print repr(np.array(val))

