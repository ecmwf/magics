# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


import lxml.etree as etree
import sys

x = etree.parse(sys.argv[1])
beau = etree.tostring(x, pretty_print = True)

file=open(sys.argv[1], "w")

file.write(beau)

file.close()


