import jinja2
from xml.sax.handler import ContentHandler
from xml.sax import make_parser
from datetime import date

import sys
import json


class ObjectHandler(ContentHandler):

	
	generated = date.today().strftime('%Y-%m-%d')
	magics = {}
	current = ""
	basic = {
		"bool" : "getBool",
		"int"  : "getInt",
		"float" : "getDouble",
		"string" : "getString",
		"stringarray" : "getStringArray",
		"intarray" : "getIntArray",
		"floatarray" : "getDoubleArray",
		"longintarray" : "getLongIntArray",
	}
	types = {
		"float" : "double",
		"floatarray" : "doublearray",
		"longintarray" : "longintarray",
	}
	
	
	def newclass(self, attrs):
		self.current = attrs.get("action", "unknown")
		self.parameters = self.magics.get(self.current, {})
	

	def parameter(self, attrs):
		if attrs.get("implemented") == "no" :
			return
		self.parameters.update( { attrs["name"] : { key : attrs[key] for key in ["from", "default"] }})

		
	options = { "class" : newclass,
				"parameter" : parameter,
			}		



	def startElement(self, name, attrs):
		if name in self.options.keys():
			self.options[name](self, attrs)
	def endElement(self, name):
		self.magics[self.current] = self.parameters
		

object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

import glob


for a in glob.glob("../src/params/*.xml") :
	datasource = open(a, "r")
	saxparser.parse(datasource)	

small = { key : object.magics[key] for key in ["unknown", "pcont"] }


print json.dumps(object.magics, indent = 4)


