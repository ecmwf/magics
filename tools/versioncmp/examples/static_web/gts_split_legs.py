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
