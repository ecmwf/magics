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
