#!/usr/bin/env python

"""The script uploads an image file to "http://download.ecmwf.int/" server to use it as Magics version reference.
"""

import sys
import os
from subprocess import call
from optparse import OptionParser

__author__  = 'cgs'
__date__    = '2013-10-01'
__version__ = '0.1'


def main(executable,reference, destination,interpreter):

    if not interpreter:
        cmd = ["./"+executable]
    else :
        print interpreter
        cmd = [interpreter,executable]

    e = call(cmd)
    e = os.path.exists(reference)
	
    if not e:
		print "File %s has not been generated"%reference
		sys.exit(1)

    print "File %s has been generated"%reference
    where = "deploy@download-admin:test-data/%s"%destination
    print where
    e = call(["scp",reference, where])
    sys.exit(e)

if __name__ == "__main__":

    print sys.argv

    cmd_parser = OptionParser(usage="usage: %prog <executable> <reference-file> <destination-dir> [<interpreter>]", version='%prog : '+__version__, description = __doc__, prog = 'upload.py')
    _,positional = cmd_parser.parse_args()
    executable,reference,destination,interpreter= [None]*4
 
    if positional: executable=  positional.pop(0)    
    if positional: reference=   positional.pop(0)    
    if positional: destination= positional.pop(0)    
    if positional: interpreter= positional.pop(0)    

    main(executable,reference, destination,interpreter)
