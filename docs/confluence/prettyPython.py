# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

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
