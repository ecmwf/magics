#!/usr/bin/env python

"""The script runs a executable (magic's test) and uploads:
   - The output image (reference)
   - The execution stdout (reference.out)
   - The execution stderr (reference.err)
   - The process resource usage info (reference.usa)
into "http://download.ecmwf.int/" server to use them as Magics version reference.
"""

#Python standard library 
import sys
import os
import json
import resource
from subprocess import Popen,PIPE
from optparse import OptionParser

#Project modules
from regression_util import extension,prefix,upload,usage2Dict

__author__  = 'cgs,cgjd'
__date__    = '2013-10-01'
__version__ = '0.1'

def uploadTest(version,executable,reference,destination,interpreter,run):

    #run the test
    if run:
        p= None
        if interpreter:
            p = Popen([interpreter,executable],stdout=PIPE, stderr=PIPE)
        else:
            p = Popen(executable,stdout=PIPE,stderr=PIPE,shell=True)

        #get test's run information
        stdout,stderr= p.communicate()
        usage= usage2Dict(resource.getrusage(resource.RUSAGE_CHILDREN))
                
        with open(extension(reference,'out'),'w') as f: f.write(stdout)
        with open(extension(reference,'err'),'w') as f: f.write(stderr)
        with open(extension(reference,'usa'),'w') as f: f.write(json.dumps(usage,sort_keys=True,indent=4, separators=(',', ': ')))

    ofiles= [reference,extension(reference,'out'),extension(reference,'err'),extension(reference,'usa')]

    #check if output is available  
    for filename in ofiles:   
        if not os.path.exists(filename):
            print "File %s has not been generated"%reference
            sys.exit(1)

    #upload the files
    for filename in ofiles:
        target= destination+'/'+prefix(filename,version+'_')
        e= upload(filename,target)
        if e>0:
            print "File %s has not been upload"%reference
            sys.exit(1)

if __name__ == "__main__":

    cmd_parser = OptionParser(usage="usage: %prog <version> <executable> <reference-file> <destination-dir>", version='%prog : '+__version__, description = __doc__, prog = 'upload.py')
    cmd_parser.add_option("-i", "--interpreter" , default=None, help="Interpreter command")
    cmd_parser.add_option("-n", "--no-run",default=True,action="store_false",dest="run",help="do NOT run executable, only upload files")

    print sys.argv
    
    optional,positional = cmd_parser.parse_args()
    version,executable,reference,destination= [None]*4

    if positional: version=     positional.pop(0)     
    if positional: executable=  positional.pop(0)    
    if positional: reference=   positional.pop(0)    
    if positional: destination= positional.pop(0)    

    uploadTest(version,executable,reference,destination,optional.interpreter,optional.run)
