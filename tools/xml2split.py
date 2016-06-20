# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



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
			filename= "../params/%s.xml" % self.name
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
			self.file.write("\n</magics>\n")
			self.file.close()
			self.open_file = 0

object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

datasource = open(sys.argv[1],"r")
saxparser.parse(datasource)
