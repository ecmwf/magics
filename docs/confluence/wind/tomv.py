#!/usr/bin/env python 
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




def toggle(out, param):
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t{\n")
    for val in param["values"] :
        out.write("\t\t%s; %s\n" % ( val.upper(), val.upper()))
    deffile.write("\t} = %s\n\n" % param["default"].upper()) 
    for val in param["values"] :
        if param.has_key(val) :
            for p in param[val] :
                printdef(out, p)
        
    

def any(out, param) :
	val = param["default"]
	if val == "(automatic)":
		val = "\"" + val + "\""
	out.write("\t%s\n" %  param["name"].upper())
	if val == "":
		out.write("\t{\n\t\t@\n\t} \n\n")
		return
	if val == " ":
		out.write("\t{\n\t\t@\n\t} \n\n")
		return
	else:
		out.write("\t{\n\t\t@\n\t} = %s\n\n" % val)
    

def number(out, param) :
	val = param["default"].upper()
	if param["default"].upper() == "INT_MAX":
		val = "1.0E+21"
	if param["default"].upper() == "-INT_MAX":
		val = "-1.0E+21"
	out.write("\t%s\n" %  param["name"].upper())
	out.write("\t{\n\t\t*\n\t} = %s\n\n"% val)
    
def stringlist(out, param) :
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t{\n\t\t@\t\t\n\t\t/\n\t}\n")

def numberlist(out, param) :
    out.write("\t%s\n" %  param["name"].upper())
    out.write("\t{\n\t\t*\t\t\n\t\t/\n\t} \n")

def colour(out, param) :
	colour = param["default"]
	if colour == "automatic":
		colour="black"
	out.write("\t%s\n" %  param["name"].upper())
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
    if  done.has_key(param["name"]) :
        return
    done[param["name"]] = True    
    if  param.has_key("type") :
    	to = param["type"]
    	if ( helpers.has_key(to)):
       		helpers[to](deffile, param) 
    	else :
        	any(deffile, param)

def addRules(rules, name, param, val) :
	if len(param) == 0:
		return;
	if len(val) == 0:
		return
	if rules.has_key(name) == False:
		rules[name] = []
	rules[name].append({"param":param, "value": val})



def unset(params, rules, name, val):
	for param in params:
		print "unset" + param["name"]  + " if " + name + " != " + val
		addRules(rules, param["name"], name, val)
		if  param.has_key("values") :
			for v in  param["values"]:
				print "look for " + v
				if param.has_key(v):
					print "found " + v
					unset(param[v], rules, param["name"], v)


def printrules(param, rules):      
	unset([param], rules, "", "")
				

actions = ["coast", "axis", "symb", "wind"]
actions = ["graph"]

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
			for  param in a["parameters"]:
				printdef(deffile, param)
				printrules(param, rules)
			deffile.write("}\n")


	print "------------------------------------------------------"
	for unset in rules:
		rule = "%if"
		sep = ""
		for param in rules[unset]:
			rule  += sep + " %s <> %s " % (param["param"].upper(), param["value"].upper())
			sep = " \n\t\t%and "

		rule += "%then \n"
		rule += "\t%%unset %s\n" % (unset.upper()) 
	
		print >>rulesfile,  rule


