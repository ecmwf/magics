import jinja2
from xml.sax.handler import ContentHandler
from xml.sax import make_parser
from datetime import date

import sys



class ObjectHandler(ContentHandler):

	name = ""
	tag = ""
	generated = date.today().strftime('%Y-%m-%d')
	parameters = { "basic" : [],
				   "factory" : [] }
	basic = {
		"bool" : "getBool",
		"int"  : "getInt",
		"float" : "getFloat",
		"string" : "getString",
		"stringarray" : "getStringArray",
	}
	factory = {
		"LineStyle" : "setAttribute",
		"Colour": "setAttribute",
	}

	def newclass(self, attrs):
		self.name = attrs.get("name")
		self.prefix = [attrs.get("prefix")] # need to split by /
		self.tag = attrs.get("xmltag")
		self.current = {}

	def parameter(self, attrs):
		
		type = attrs.get("to")

		if type in self.basic :
			self.parameters["basic"].append (
					{
						"name" : attrs.get("name"), 
						"from" : attrs.get("from"), 
						"to" : attrs.get("to"), 
						"member" : attrs.get("member"), 
						"default" : attrs.get("default"),
						"method" : self.basic[type]
					}
				)
		else :

			self.parameters["factory"].append (
					{
						"name" : attrs.get("name"), 
						"from" : attrs.get("from"), 
						"to" : attrs.get("to"), 
						"member" : attrs.get("member"), 
						"default" : attrs.get("default"),
						"include" : attrs.get("include"),
						"options"  : [],
						"method" : self.factory.get(type, "setMember"),
					}
				)
			if ( type in ["LineStyle"] ):
				self.parameters["factory"][-1]["enum"] = True
				print self.parameters["factory"][-1]
			if ( type in ["LineStyle", "Colour"] ):
				self.parameters["factory"][-1]["niceprint"] = True
				print self.parameters["factory"][-1]


	def option(self, attrs):
		param = self.parameters["factory"][-1]
		param["options"].append({
			"key" : attrs.get("xml"), 
			"parent" : param["to"],
			"object" : attrs.get("name") } )

		param["options"].append({
			"key" : attrs.get("fortran"), 
			"parent" : param["to"],
			"object" : attrs.get("name") } )

	

	options = { "class" : newclass,
				"parameter" : parameter,
				"option" : option }

	def startElement(self, name, attrs):
		if name in self.options.keys():
			self.options[name](self, attrs)
	def endElement(self, name):
		pass

object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

toolssource = sys.argv[1]

datasource = open(sys.argv[2], "r")
destination = sys.argv[3]

saxparser.parse(datasource)	


with open("%s/source.template" % toolssource,  "r") as source:
	template = jinja2.Template(source.read())
with open("%s/%sAttributes.cc" % (destination, object.name), "wt") as out:
	out.write(template.render(object = object.name, 
						string_parameters = object.parameters["basic"],
						factory_parameters = object.parameters["factory"],
						date = object.generated,
						tag = object.tag,
						prefix = object.prefix
						)
			)
with open("%s/header.template" % (toolssource), "r") as source:
	template = jinja2.Template(source.read())
with open("%s/%sAttributes.h" % (destination, object.name), "wt") as out:
	out.write(template.render(object = object.name, 
						string_parameters = object.parameters["basic"],
						factory_parameters = object.parameters["factory"],
						date = object.generated,
						tag = object.tag,
						prefix = object.prefix
						)
			)