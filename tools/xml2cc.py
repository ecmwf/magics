#!/usr/bin/env python
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
    include = {}
    include_options = {}
    implements = []

    def addinterface(self, name):
        if name == "" :
            return
        add = name.split('/')
        for a in add:
            self.implements.append(a)

    def addimplements(self, name) :
        if name == "" :
            return
        add = name.split('/')
        for a in add:
            self.include["%s.h" % a] = "%s.h" % a
        self.addinterface(name)

    def newclass(self, attrs):
        self.name = attrs.get("name")
        self.prefix = attrs.get("prefix", "").split("/")
        self.tag = attrs.get("xmltag")
        self.addimplements(attrs.get("implements", ""))
        self.addinterface(attrs.get("interface", ""))
        self.current = {}

    def parameter(self, attrs):
        if attrs.get("implemented") == "no" :
            return
        type = attrs.get("to")
        fromt = attrs.get("from")
        to = self.types.get(type, type)

        if type in self.basic :
            self.parameters["basic"].append (
                {
                    "name" : attrs.get("name"),
                    "from" : self.types.get(fromt, fromt),
                    "to" : to,
                    "member" : attrs.get("member"),
                    "default" : attrs.get("default"),
                    "method" : self.basic[type]
                }
                )
            if type in ["string", "bool"]:
                self.parameters["basic"][-1]["delimiter"] = "\""
        else :

            self.parameters["factory"].append (
                {
                    "name" : attrs.get("name"),
                    "from" : attrs.get("from"),
                    "to" : attrs.get("to"),
                    "member" : attrs.get("member"),
                    "default" : attrs.get("default"),
                    "options"  : [],
                    "method" : "setMember",
                    "delimiter" : "\""
                }
                )
            if ( type in ["LineStyle", "ArrowPosition", "Justification",
                          "AxisAutomaticSetting", "ListPolicy", "Position",
                          "Hemisphere", "DisplayType", "Matrix", "cairo_t*"]):
                self.parameters["factory"][-1]["enum"] = True
                self.parameters["factory"][-1]["method"] = "setAttribute"
                self.parameters["factory"][-1]["niceprint"] = True

            else :
                self.include[attrs.get("include", "%s.h" % attrs.get("to"))] = attrs.get("include", "%s.h" % attrs.get("to"))
            if type in ["Matrix"]:
                self.parameters["factory"][-1]["delimiter"] = ""
                self.include["Matrix.h"] = "Matrix.h"
            if type in ["cairo_t*"]:
                self.parameters["factory"][-1]["delimiter"] = ""
                self.include["cairo.h"] = "cairo.h"
            if type in ["Colour"]:
                self.parameters["factory"][-1]["niceprint"] = True



    def option(self, attrs):

        param = self.parameters["factory"][-1]
        name = attrs.get("xml")
        if name != None:
            param["options"].append({
                "key" : attrs.get("xml"),
                "name" : name.replace(":", "_"),
                "parent" : param["to"],
                "object" : attrs.get("name") } )
            self.include_options[attrs["include"]] = attrs["include"]

        if attrs.get("fortran") != attrs.get("xml"):
            name = attrs.get("fortran")
            if name != None:
                param["options"].append({
                    "key" : attrs.get("fortran"),
                    "name" : name.replace(":", "_"),
                    "parent" : param["to"],
                    "object" : attrs.get("name") } )
                self.include_options[attrs["include"]] = attrs["include"]



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
                              include = object.include,
                              include_options = object.include_options,
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
                              include = object.include,
                              include_options = object.include_options,
                              implements = object.implements,
                              date = object.generated,
                              tag = object.tag,
                              prefix = object.prefix
                             )
             )
