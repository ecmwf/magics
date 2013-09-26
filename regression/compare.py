#!/usr/bin/env python

import sys
import os
from subprocess import call


def main(args=None):
    if args == None:
            args = sys.argv
    print args
    e = call([args[1]])
    e = os.path.exists(args[2])
    print e 
    if ( e == False ) :
		print "File %s has not been generated" % ( args[2] )
    

    sys.exit(e)


if __name__ == "__main__":
    main()

