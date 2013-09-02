# -*- coding: utf-8 -*-
from sys import argv
import re

# 3 input parameters expected
assert(len(argv)==4)

# read lines of first file to compare
f= open(argv[1],'r')
s= f.read()
f.close()
lines1= s.split('\n')
#for l in lines1: print l

# read lines of second file to compare
f= file(argv[2],'r')
s= f.read()
f.close()
lines2= s.split('\n')

# read diff output for the files
f= file(argv[3],'r')
diff1,diff2= [],[]
for l in f:
    m= re.match(r'(\d+),?(\d+)?([adc])(\d+),?(\d+)?',l)
    if m==None: continue
    g= m.groups()
    # 'added' lines in second file
    if g[2]=='a':
      if not g[4]==None:
	diff2+= range(int(g[3])-1,int(g[4]))
      else:
	diff2+= [int(g[3])-1]
    # 'deleted' lines in first file
    if g[2]=='d':
      if not g[1]==None:
	diff1+= range(int(g[0])-1,int(g[1]))
      else:
	diff1+= [int(g[0])-1]
    # 'changed' lines in both files
    if g[2]=='c':
      if not g[1]==None:
	diff1+= range(int(g[0])-1,int(g[1]))
      else:
	diff1+= [int(g[0])-1]
      if not g[4]==None:
	diff2+= range(int(g[3])-1,int(g[4]))
      else:
	diff2+= [int(g[3])-1]

# highlight the modified lines
prefix= '</pre><font size="3" face="Courier New" color="red">'
sufix=  '</font><pre style="font-family:\'Courier New\'">'
for i in diff1:
  if 0<=i<len(lines1):
    infix= lines1[i].replace(' ','&nbsp;')
    lines1[i]= prefix+infix+sufix
for i in diff2:
  if 0<=i<len(lines2):
    infix= lines2[i].replace(' ','&nbsp;')
    lines2[i]= prefix+infix+sufix
    
# overwrite first file
f= file(argv[1],'w')
for l in lines1: f.write(l+'\n')
f.close()

# overwrite second file
f= file(argv[2],'w')
for l in lines2: f.write(l+'\n')
f.close()

