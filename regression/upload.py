#!/usr/bin/env python

import sys
import os
from subprocess import call


def main(args=None):
    if args == None:
            args = sys.argv
    print args

    cmd = []
    if args[4] == None :
        cmd = ["./"+args[1]]
    else :
        cmd = [args[4], args[1]]

    e = call(cmd)
    e = os.path.exists(args[2])
	
    if ( e == False ) :
		print "File %s has not been generated" % ( args[2] )
		sys.exit(1)

    print "File %s has been generated" % ( args[2] )
    where = "deploy@download-admin:test-data/%s" % ( args[3])
    print where
    e = call(["scp", args[2], where])
    sys.exit(e)


if __name__ == "__main__":
    main()

