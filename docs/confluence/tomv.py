#!/usr/bin/python
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from types import *

import sys
import json

all = []




def toggle(out, param):
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t[ help_text = '%s' ]\n" %  param["documentation"])
    out.write("\t{\n")
    for val in param["values"] :
        out.write("\t\t%s; %s\n" % ( val.upper(), val.upper()))
    deffile.write("\t} = %s\n\n" % param["metview_default"].upper()) 
    
    

def any(out, param) :
	val = param["metview_default"]
	if val == "(automatic)":
		val = "\"" + val + "\""
	out.write("\t%s\n" %  param["name"].upper())
	out.write("\t[ help_text = '%s' ]\n" %  param["documentation"])
	if val == "":
		out.write("\t{\n\t\t@\n\t} \n\n")
		return
	if val == " ":
		out.write("\t{\n\t\t@\n\t} \n\n")
		return
	else:
		out.write("\t{\n\t\t@\n\t} = %s\n\n" % val)
    

def number(out, param) :
	val = param["metview_default"].upper()
	if param["metview_default"].upper() == "INT_MAX":
		val = "1.0E+21"
	if param["metview_default"].upper() == "-INT_MAX":
		val = "-1.0E+21"
	out.write("\t%s\n" %  param["name"].upper())
	out.write("\t[ help_text = '%s' ]\n" %  param["documentation"])
	out.write("\t{\n\t\t*\n\t} = %s\n\n"% val)
    
def stringlist(out, param) :
	out.write("\t%s\n" %  param["name"].upper())
	out.write("\t[ help_text = '%s' ]\n" %  param["documentation"])
	if len(param["values"]) == 0 :
		param["values"] = ["@"]
	if param.has_key("values") :
		sep = ""
		out.write("\t{\t\t")
		for val in param["values"] :
			tokens = val.split(":")
			if len(tokens) == 1: 
				out.write("%s%s; %s" % (sep, val, val))
			else: 
				out.write("%s%s; %s" % (sep, tokens[1], tokens[0]))
			sep = "\n\t\t"
		out.write("\n\t\t/\n\t}\n")
	else :
		out.write("\t{\n\t\t@\t\t\n\t\t/\n\t}\n")

def numberlist(out, param) :
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t[ help_text = '%s' ]\n" %  param["documentation"])
    out.write("\t{\n\t\t*\t\t\n\t\t/\n\t} \n")

def colour(out, param) :
    colour = param["metview_default"]
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t[ help_text = '%s', help = help_colour,interface = colour]\n" %  param["documentation"])
    if colour == "automatic":
		colour="black"
    out.write("\t{\n\t\t&PARAMSHARE&COLOUR\n\t} = %s\n\n"% colour.upper())
    
helpers = {
   "toggle": toggle,
   "bool": toggle,
   "number": number,
   "numberlist": numberlist,
   "Colour": colour,
   "stringlist": stringlist
  }

done = {}

def printdef(out, param):      
    
    if  param.has_key("type") :
    	to = param["type"]
    	if ( helpers.has_key(to)):
       		helpers[to](deffile, param) 
    	else :
        	any(deffile, param)



def child(params, rules, name, val):
	
	for param in params:
		print "child unset-->" + param["name"]
		if  param.has_key("values") :
			for v in  param["values"]:
				if  param.has_key(v) :
					for p in param[v] :
						if rules.has_key(p) == False:
							rules[p] = []
						rules[p].append({"param":name, "value": val})
						child(getParam(p),rules, name, val)
						

def set(params, rules, name, val):

	for param in params:
		if  param.has_key("values") :
			for v in  param["values"]:
				if  param.has_key(v) :
					for p in param[v] :
						if rules.has_key(p) == False:
							rules[p] = []
						rules[p].append({"param":param["name"], "value": v})
						print "look for child-->" + p 
						child(getParam(p), rules, param["name"], v)
						
					
						
					


def printrules(param, rules):      
	set([param], rules, "", "")

def getParam(name):
	print "getParam"
	for p in all:
		print p 
		print "xx" 
		
		if p["name"] == name:
			return [p]
	return []
	
							

actions = ["coast", "axis", "symb", "wind", "graph", "contour"]
actions = ["wind"]
actions = ["legend"]
actions = ["taylor"]
actions = ["kml"]
actions = ["boxplot"]

for action in actions:
	f = open( action + '.json', "r")
	definition=json.load(f)
	deffile = open('M'+action.upper()+'Def', "w")
	rulesfile = open('M'+action.upper()+'Rules', "w")
	rules={}

	deffile.write("PARAMSHARE ; ParamShare; PARAMSHARE\n")
	deffile.write("{\n")
	deffile.write("\tCOLOUR {\n")
	deffile.write("\t\t%include MagicsColors.h\n")
	deffile.write("\t\t}\n")
	deffile.write("\tSTYLE {\n")
	deffile.write("\t\tSOLID; SOLID\n")
	deffile.write("\t\tDASH; DASH\n")
	deffile.write("\t\tDOT; DOT\n")
	deffile.write("\t\tCHAIN_DOT; CHAIN_DOT\n")
	deffile.write("\t\tCHAIN_DASH; CHAIN_DASH\n")
	deffile.write("\t}\n")
	deffile.write("}\n")

	

	for entry in definition:
		for a in definition[entry]:
			
			
			deffile.write( "M%s; Magics; Automatically generated\n" % (action.upper()) ) 
			deffile.write("{\n")
			all = a["parameters"]
			for  param in a["parameters"]:
				
				printdef(deffile, param)
				printrules(param, rules)
			deffile.write("}\n")


	print "------------------------------------------------------"
	for unset in rules:
		
		for param in rules[unset]:
			rule  = "%%if  %s <> %s " % (param["param"].upper(), param["value"].upper())
			rule += "%then \n"
			rule += "\t%%unset %s\n" % (unset.upper()) 
			print >>rulesfile,  rule


