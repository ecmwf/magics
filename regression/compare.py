#!/usr/bin/env python

import sys
import os
from subprocess import call


def main(args=None):
    if args == None:
            args = sys.argv
    print args
    if os.path.exists(args[2]):
		os.remove(args[2])
		print "Clean File %s before running the test" % ( args[2] )

    e = call([args[1]])

    e = os.path.exists(args[2])
	
    if ( e == False ) :
		print "File %s has not been generated" % ( args[2] )
		sys.exit(1)

    print "File %s has been generated" % ( args[2] )
    sys.exit(0)


if __name__ == "__main__":
    main()

