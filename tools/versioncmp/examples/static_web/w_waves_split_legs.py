# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import sys
import string

if len(sys.argv) <> 3:
	raise ValueError,'Expects <list of steps> <number of legs>'

p_steps = sys.argv[1]
legs = int(sys.argv[2])

steps = string.split(p_steps,',')
an = steps[0]
fc = steps[1:]
count = int(len(fc) / legs)
rest  = len(fc) % legs
if rest <> 0:
	raise ValueError,'The number of steps is not a multiple of legs'

lists = []
for i in range(legs):
	lists.append(fc[i*count:i*count+count])

print an + ',' + string.join(lists[0],',')
for i in range(1,legs):
	print string.join(lists[i],',')
