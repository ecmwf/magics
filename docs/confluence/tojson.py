#!/usr/bin/python
from xml.sax.handler import ContentHandler
from xml.sax import make_parser
from types import *

import sys
import json


class ObjectHandler(ContentHandler):
    path ="../../src/params/"
    tag = ""
    doc = ""
    magics = {}
    objects = {}
    object = {}
    param = {}
    paramdoc = ""
    actiondoc = ""
    parameters = {}
    result = []
    keywords = ""
    done = {}

    def reset(self):
        self.path ="../../src/params/"
        self.tag = ""
        self.doc = ""
        self.magics = {}
        self.objects = {}
        self.object = {}
        self.param = {}
        self.paramdoc = ""
        self.actiondoc = ""
        self.parameters = {}
        self.result = []
        self.keywords = ""
        self.done = {}
        print "RESET"

    def boolset(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["on", "off"]

    def lineset(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["solid", "dash", "dot", "chain_dash", "chain_dot"]
        p["type"] = "toggle"

    def arrowposition(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["tail", "centre"]
        p["type"] = "toggle"

    def justification(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["left", "right", "centre"]
        p["type"] = "toggle"

    def listpolicy(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["lastone", "cycle"]
        p["type"] = "toggle"

    def colourtechnique(p, attrs):
        p["set"] = "psetc"
        p["values"] = ["calculate", "list"]
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
        p["type"] = "intlist"
        p["set"] = "pset1i"
        return

    def floatlist(p, attrs):
        p["type"] = "numberlist"
        p["set"] = "pset1r"
        return

    def stringlist(p, attrs):
        p["type"] = "stringlist"
        if  attrs.has_key("values") :
            val = attrs["values"]
            values = val.split("/")
            p["values"] = values
        p["set"] = "pset1c"
        return

    def frequency(p, attrs):
        p["min"] = 1
        p["max"] = 10
        p["set"] = "pseti"
        p["type"] = "number"
        return

    def thickness(p, attrs):
        p["min"] = 1
        p["max"] = 12
        p["set"] = "pseti"
        p["type"] = "number"

        return

    types = {
        "bool": boolset,
        "float": floatnumber,
        "int": intnumber,
        "floatarray": floatlist,
        "intarray": intlist,
        "stringarray": stringlist,
        "string": anyset,
        "AxisAutomaticSetting": anyset,
        "Colour": anyset,
        "LineStyle": lineset,
        "ListPolicy": listpolicy,
        "ColourTechnique": colourtechnique,
        "ArrowPosition": arrowposition,
        "Justification": justification,
        "thickness": thickness,
        "frequency": frequency
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
        print "newparam", p["name"]
        p["type"] = attrs.get("to")
        specials=["frequency", "thickness"]
        for key in specials:
            if  p["name"].find(key) != -1 :
                p["type"] = key
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
        if  attrs.has_key("metview_default") :
            p["metview_default"] = attrs.get("metview_default")
        else :
            p["metview_default"] = attrs.get("default")
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
            if  self.get(attrs, "visible", "on") == "on" :
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

    def endElement(self, name) :
        if ( name == "parameter"  and self.doc == "parameter") :
            self.param["documentation"] = self.paramdoc
            self.paramdoc= ""
        if ( name == "class" ) :
            self.object["documentation"] = self.actiondoc
            self.actiondoc= ""

    def printDef(self,name):
        for p in self.magics:
            print p
        #print json.dumps(self.magics, indent=2)
        #print ""

    def parse(self, file):
        saxparser = make_parser()
        saxparser.setContentHandler(self)
        print self.path + file

        datasource = open(self.path + file, "r")
        saxparser.parse(datasource)
        return self.magics


    def prepare(self, defparam):
        param = defparam.copy()
        if param.has_key("values") == False:
            return param
        if len(param["values"]) == 0:
            return param
        for val in param["values"]:

            newp = []
            if param.has_key(val):
                for v in param[val]:
                    self.keywords +=  ", " + v
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

    def  prepareParam(self, param):
        self.keywords +=  ", " + param["name"]
        print "keywords", self.keywords
        newparam = {}
        for p in param:
            newparam[p] = param[p]
        for v in param["values"]:
            if param.has_key(v):
                newparam[v] = []
                for val in param[v]:
                # if this a parametername or a values!
                    if self.magics.has_key(val):
                        self.append(newparam[v], self.prepareObjectParams(self.magics[val]))
                    else:
                        pp = []
                        if self.parameters.has_key(val):
                            pp = self.prepareParam(self.parameters[val])

                        if len(pp)!=0 :
                            newparam[v].append(self.prepareParam(self.parameters[val]))

        return newparam

    def addParam(self, param):
        # first check if in the list
        if param["name"] in self.done :
            return;
        self.done[param["name"]] = param["name"]
        self.result.append(param)

        for v in param["values"]:

            if param.has_key(v):

                values = []

                for val in param[v]:

                    if self.magics.has_key(val):
                        values = values + (self.prepareParams(self.magics[val]))
                    else :
                        values.append(val)

                param[v] = values;



    def prepareParams(self, object):
        values = []
        if object.has_key("inherits"):
        # get inherited object
            parent = self.magics[object["inherits"]]
            for param in parent["parameters"]:


                if isinstance(param["name"], (list, tuple)):
                    values = values + param["name"]
                else:
                    values.append(param["name"])
                self.addParam(param)
            ## Add  inherited params!
            #self.append(parameters, parent)
        # Add own parameters:

        for param in object["parameters"]:

            print json.dumps(param, indent=2)

            if isinstance(param["name"], (list, tuple)):

                values = values + param["name"]
            else:
                values.append(param["name"])
            self.addParam(param)
        return values




    def getParams(self, action):
        object = self.magics[action]

        return self.prepareParams(object);

    def getDoc(self, action):
        action = self.magics[action]

        return action["documentation"]



def createAction(version, abbr, name, files):

    object = ObjectHandler()
    object.reset()
    for f in files:
        print "FILE", f
        x = object.parse(f)


    #object.printDef(name)
    parameters = object.getParams(name);
    doc = object.getDoc(name);

    magics={}
    magics["magics"] = []
    action={}
    action["action"]= abbr;
    action["metview"]="m" + abbr
    action["fortran"]="p" + abbr
    action["documentation"]=doc + "[version " + version + "]"
    action["parameters"]=object.result
    magics["magics"].append(action)



    f = open(abbr+".json", "w")

    f.write(json.dumps(magics, indent=1))

    return



