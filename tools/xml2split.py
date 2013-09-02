

from xml.sax.handler import ContentHandler
from xml.sax import make_parser
import sys

class ObjectHandler(ContentHandler):
	title = ""
	name = ""
	tab1 = 0
	tab2 = 2
	open_file = 0

	def tab(self, n):
		sep=''
		i = 0
		while i < n:
			sep = sep + '\t'
			i = i +1
		return sep

	def nice(self, name, attrs):
		sep1 = self.tab(self.tab1)
		sep2 = self.tab(self.tab2)
		self.file.write("%s<%s " % (sep1, name))
		for key in attrs.getNames():
			self.file.write(" %s='%s'" % (key , attrs.getValue(key)))
		self.file.write(">")

	def characters(self, data):
		if self.open_file == 1:
			self.file.write(data);

	def startElement(self, name, attrs):
		print "found %s " % name
		if (name == "magics") :
			return
		if (name == "static") :
			return

		if (name == "class"):
			self.name = attrs.get("name")
			filename= "xml/%s.xml" % self.name
			self.file=open(filename, "w")
			self.open_file = 1;
			self.file.write("<magics>\n")
			print "open %s " % filename
		self.nice(name, attrs)
		self.tab1 = self.tab1+1
		self.tab2 = self.tab2+1

	def endElement(self,name):
		print "close %s\n " % name
		if ( self.open_file == 0):
			return

		self.tab1 = self.tab1-1
		self.tab2 = self.tab2-1
		self.file.write("%s</%s>" % (self.tab(self.tab1), name)) 
		if (name == "class") :
			self.file.write("</magics>\n")
			self.file.close()
			self.open_file = 0

object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

datasource = open(sys.argv[1],"r")
saxparser.parse(datasource)
