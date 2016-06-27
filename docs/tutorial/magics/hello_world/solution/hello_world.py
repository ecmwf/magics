# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from Magics.macro import *

#Setting of the output file name
output = output({'output_name':'hello_world'})

#Setting of the coastlines
coast = mcoast()

#Setting of the text
text = mtext({'text_line_1': 'Hello World!',
              'text_line_count': 1 })

#or aternative setting using an vestor of strings insteed!
text = mtext({'text_lines': ['Hello World!']})

plot(output, coast, text)
