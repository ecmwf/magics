#!/usr/bin/env python 
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
	parameters = {}

	def boolset(p, attrs):
		p["set"] = "psetc"
		p["values"] = ["on", "off"]

	def lineset(p, attrs):
		p["set"] = "psetc"
		p["values"] = ["solid", "dash", "dot", "chain_dash", "chain_dot"]
		p["type"] = "toggle"

	def justification(p, attrs):
		p["set"] = "psetc"
		p["values"] = ["left", "right", "centre"]
		p["type"] = "toggle"

	def listpolicy(p, attrs):
		p["set"] = "psetc"
		p["values"] = ["lastone", "cycle"]
		p["type"] = "toggle"
		
	def anyset(p, attrs):
		p["set"] = "psetc"
		if  attrs.has_key("values") :
			val = attrs["values"]
			values = val.split("/")
			p["values"] = values
			p["type"] = "toggle"
		return
	
	
	def intnumber(p, attrs):
		p["type"] = "number"
		p["set"] = "pseti"
		return
	def floatnumber(p, attrs):
		p["type"] = "number"
		p["set"] = "psetr"

	def intlist(p, attrs):
		p["type"] = "numberlist"
		p["set"] = "pset1i"
		return

	def floatlist(p, attrs):
		p["type"] = "numberlist"
		p["set"] = "pset1r"
		return

	def stringlist(p, attrs):
		p["type"] = "stringlist"
		p["set"] = "pset1c"
		return

	types = { 
		"bool": boolset,
		"float": floatnumber,
		"int": intnumber,
		"floatarray": intlist,
		"intarray": floatlist,
		"stringarray": stringlist,
		"string": anyset,
		"AxisAutomaticSetting": anyset,
		"Colour": anyset,
		"LineStyle": lineset,
		"ListPolicy": listpolicy,
		"Justification": justification
	}


	def get(self, attrs, name, default):
		if  attrs.has_key(name):
			return attrs.get(name)
		else :
			return default

	def newparam(self, attrs):
		p = {}
		self.parameters[attrs.get("name")] = p
		p["name"] = attrs.get("name")
		p["type"] = attrs.get("to")
		if attrs.has_key("metview") :
			p["metview"]= attrs["metview"]
		p["values"] = []
		t = p["type"]
		if  self.types.has_key(t) :
			self.types[t](p, attrs)
		else :
			p["set"]="psetc"
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
	

	def prepare(self, defparam):
		param = defparam.copy()
		print "prepare-->"
		print param
		print "<---prepare"
		if param.has_key("values") == False:
			return param
		if len(param["values"]) == 0:
			return param
		for val in param["values"]:
			print val + " For " + param["name"]
			newp = []
			if param.has_key(val):
				for v in param[val]:
					print "prepare-->" + v
					# Can be a parameter or another object!
					if self.parameters.has_key(v) :
						self.append(newp, v)
					else:
						self.append(newp, self.getList(v))
			param[val] = newp

		return param

	def append(self, p1, p2):
		for p in p2:
			p1.append(p)


	def getList(self, object):
		definition = self.magics[object] 
		list = []
		if definition.has_key("inherits"):
			self.append(list, self.getList(definition["inherits"]))
		for p in definition["parameters"]:
			list.append(p["name"])
		return list;
			
			

	def buildParam(self, param):
		parameters = []
		if param.has_key("inherits"):
			print param["name"] + " inherits from " + param["inherits"]	
			parent = self.buildParam(self.magics[param["inherits"]])
			self.append(parameters, parent) 
		parameters.append(self.prepare(param))
		return parameters
				
		

	def build(self, params):
		parameters = []
		for p in params:
			print "find class " + p
			if ( self.magics.has_key(p) ) :
				defp = self.magics[p]
			# internal object
				for p in defp["parameters"]:
					self.append(parameters, self.buildParam(p))
		return parameters

	def getParams(self, action):
		return self.build(action);

	def getDoc(self, action):
		action = self.magics[action]

		return action["documentation"]
			


def createAction(abbr, name, files):

	object = ObjectHandler()
	for f in files:
		object.parse(f)

	parameters = object.getParams([name]);
	doc = object.getDoc(name);


	magics={}
	magics["magics"] = []
	action={}
	action["action"]= abbr;
	action["metview"]="m" + abbr
	action["fortran"]="p" + abbr
	action["documentation"]=doc
	action["parameters"]=parameters
	magics["magics"].append(action)



	f = open(abbr+'.json', "w")
	f.write(json.dumps(magics, indent=1))


#createAction("coast", "Coastlines",  ["Coastlines.xml", "CoastPlotting.xml", "LabelPlotting.xml", "GridPlotting.xml"]) 
#createAction("axis",  "Axis", ["Axis.xml"]) 
#createAction("symb",  "SymbolPlotting", ["SymbolPlotting.xml"]) 
#createAction("subpage",  "FortranViewNode", ["SubPage.xml", "GeoRectangularProjection.xml", 
#											 "XYTransformation.xml", "Proj4Projection.xml", "PolarStereographicProjection.xml"]) 

createAction("wind",  "Wind", ["wind.xml"]) 
