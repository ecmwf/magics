
import lxml.etree as etree
import sys

x = etree.parse(sys.argv[1])
beau = etree.tostring(x, pretty_print = True)

file=open(sys.argv[1], "w")

file.write(beau)

file.close()


