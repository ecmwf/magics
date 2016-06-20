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
	name = param["name"].replace("_", " ").title();
	out.write("<h3> %s</h3>\n" %  name)
	out.write("<p>%s</p>\n" %  param["documentation"])





actions = ["coast", "axis", "symb", "wind", "graph", "contour"]
actions = ["wind"]
actions = ["legend"]
actions = ["taylor"]

for action in actions:
	f = open( action + '.json', "r")
	definition=json.load(f)
	deffile = open('M'+action.upper()+'Doc', "w")
	

	for entry in definition:
		for a in definition[entry]:
			
			
			all = a["parameters"]
			for  param in a["parameters"]:
				printdef(deffile, param)




