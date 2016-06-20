# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *

#Setting of the output file name
output = output({'output_name':'hello_world_python'})

#Setting of the coastlines
coast = ???()

#Setting of the text
text = ???({'???': 'Hello World!',
            'text_line_count': 1 })

#or alternative setting using an vector of strings instead!
text = mtext({'???': ['Hello World!']})

plot(output, coast, text)
