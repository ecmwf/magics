#!/usr/bin/python
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from xml.sax.handler import ContentHandler
from xml.sax import make_parser
from types import *

import sys
import json


class ObjectHandler(ContentHandler):
	path ="../src/xml/"
	tag = ""
	doc = ""
	magics = {}
	objects = {}
	object = {}
	param = {}
	paramdoc = ""
	actiondoc = ""

	def boolset(p, attrs):
		p["values"] = ["on", "off"]

	def lineset(p, attrs):
		p["values"] = ["solid", "dash", "dot"]
		p["type"] = "toggle"
		
	def anyset(p, attrs):
		if  attrs.has_key("values") :
			val = attrs["values"]
			values = val.split("/")
			p["values"] = values
			p["type"] = "toggle"
		return
	
	types = { 
		"bool": boolset,
		"string": anyset,
		"Colour": anyset,
		"LineStyle": lineset,
	}


	def get(self, attrs, name, default):
		if  attrs.has_key(name):
			return attrs.get(name)
		else :
			return default

	def newparam(self, attrs):
		p = {}
		p["name"] = attrs.get("name")
		p["type"] = attrs.get("to")
		p["values"] = []
		t = p["type"]
		if  self.types.has_key(t) :
			self.types[t](p, attrs)
		else :
			p["type"]="toggle"



		p["default"] = attrs.get("default")
		return p

	def newobject(self, attrs):
		o = {}
		o["name"] = attrs.get("name")
		o["documentation"] = ""
		o["parameters"] = []
		if  attrs.has_key("inherits") :
			o["inherits"]=attrs.get("inherits")
			print o["name"] + " inhrits from " + o["inherits"]
		return o

	def startElement(self, name, attrs):
		self.tag = name
		if ( name == "class" ) : 
			self.doc = name
		# New object
			self.object = self.newobject(attrs)
			self.magics[attrs.get("name")] = self.object;
		if ( name == "parameter" ) : 
			if  self.get(attrs, "visible", "on") == "on" :
				self.doc = name
				self.param = self.newparam(attrs)
				self.object["parameters"].append(self.param)
			else :
				self.doc = "nodoc"
		if ( name == "option") : 
			val = attrs.get("fortran")
			self.param["values"].append(val)
			if ( self.param.has_key(val) == False ):
				self.param[val] = []
			self.param[val].append(attrs.get("name"))
		if ( name == "set" ) : 
			val = attrs.get("value")
			if ( self.param.has_key(val) == False ):
				self.param[val] = []
			self.param[val].append(attrs.get("name"))

		

	def characters(self, content):
		if ( self.tag == "documentation" ) : 
			if ( self.doc == "class" ) :
				self.actiondoc += content.lstrip();
			if ( self.doc == "parameter" ) :
				self.paramdoc += content.lstrip();
				

	def endElement(self, name):
		if ( name == "parameter"  and self.doc == "parameter") : 
			self.param["documentation"] = self.paramdoc
			self.paramdoc= ""
		if ( name == "class" ) : 
			self.object["documentation"] = self.actiondoc
			self.actiondoc= ""
			
	def printDef(self):
		print json.dumps(self.magics, indent=2)
	

	def parse(self, file):
		saxparser = make_parser()
		saxparser.setContentHandler(self)
		datasource = open(self.path + file, "r")
		saxparser.parse(datasource)
		return self.magics
	
	def build(self, actions, args):
		defs = args["parameters"]
		parameters = []
			
		for a in actions:
			if ( type(a) != dict and self.magics.has_key(a) ) :
			# external object
				action = self.magics[a]
				if action.has_key("inherits") :
					parent = action["inherits"]
					definition = self.magics[parent]
					print  a + " xxxxinherits from " + parent
			
					print "??????????????????"
					for p in definition["parameters"]:
						print p["name"]	
						parameters.append(p)
					print "---------------"

				for param in action["parameters"]:
					#print "add" + param["name"]
					parameters.append(param)
					for val in param["values"]:
						if param.has_key(val) :
							param[val] = self.build(param[val], action)
						else :
							param[val] = [] 
			else :
				for param in defs:
					if (param["name"] == a) :
						parameters.append(param)
						defs.remove(param)
					

		return parameters;

	def getParams(self, action):
		return self.build(action, self.magics[action[0]]);

	def getDoc(self, action):
		action = self.magics[action]

		return action["documentation"]
			


object = ObjectHandler()
object.parse("Coastlines.xml")
object.parse("CoastPlotting.xml")
object.parse("LabelPlotting.xml")
object.parse("GridPlotting.xml")
parameters = object.getParams(["Coastlines"]);
doc = object.getDoc("Coastlines");

magics={}
magics["magics"] = []
action={}
action["action"]="coastlines"
action["documentation"]=doc
action["parameters"]=parameters
magics["magics"].append(action)



f = open('coast.json', "w")
f.write(json.dumps(magics, indent=1))

