'''
Created on 30 May 2012

@author: cgjd
'''
from pythonTidy import tidy_up
import re

def removePattern(s,pat):
    res= re.search(pat,s,re.MULTILINE)
    if not res==None:
        pat= res.group(0)
        s= s.replace(pat,'') 
    return s

def prettify(filein,fileout):
    print 'Prettifying',filein,
    
    #read python code
    f= file(filein)
    s= f.read()
    f.close()

    #remove the unwanted code
    s= '\n'+s
    sold=''
    while not sold == s:
        sold= s
        s= removePattern(s,'^#\s*For.+documentation.*$')
        s= removePattern(s,'^tofortran\([^)]*\)')
        s= removePattern(s,'^tomv4\([^)]*\)')
        s= removePattern(s,'^tohtml\([^)]*\)')
        s= s.replace('\n\n\n','\n\n')
        s= s.replace(' \n','\n')
    
    #save the file
    f= file(fileout,'w')
    f.write(s)
    f.close()

    #apply pythonTidy function to prettify the code
    tidy_up(fileout,fileout)
    print 'Done'
