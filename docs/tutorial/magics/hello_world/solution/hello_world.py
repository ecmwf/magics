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
