# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import sys
import os
import numpy
import json

from . import Magics

class Context(object):
    def __init__(self):
        self.tmp = []
        pass

global context
context  = Context()

actions={
    "mobs": "pobs",
    "mcoast": "pcoast",
    "pcoast": "pcoast",
    "mtext": "ptext",
    "ptext": "ptext",
    "psymb": "psymb",
    "msymb": "psymb",
    "pcont": "pcont",
    "mcont": "pcont",
    "pgeo": "pgeo",
    "mgeojson": "pgeojson",
    "mgeo": "pgeo",
    "mlegend": "",
    "plegend": "",
    "mgrib": "pgrib",
    "pgrib": "pgrib",
    "mwind": "pwind",
    "pwind": "pwind",
    "mgraph": "pgraph",
    "pgraph": "pgraph",
    "maxis": "paxis",
    "paxis": "paxis",
    "minput": "pinput",
    "mtable": "ptable",
    "ptable": "ptable",
    "pboxplot": "pboxplot",
    "mboxplot": "pboxplot",
    "pinput": "pinput"
    }




class Action(object):
    def __init__(self, verb, action, html, args):
        self.verb = verb
        self.action = action
        self.args = args
        if ( html == "") :
            self.html = verb
        else :
            self.html = "<a href=/wiki/display/MAGP/%s target='_blank'>%s</a>" % (html, verb)

    def __repr__(self):
        x = ""
        for key in list(self.args.keys()):
            x = x + " %s = '%s'\n" % (key, self.args[key])
        return x

    def inspect(self):
        print(self)

    def quote(self, v):
        return "\"" + v + "\""

    def tohtml(self, file):
        sep=""
        val="%s("%self.html

        for key in list(self.args.keys()):
            if isinstance(self.args[key], str):
                if key == 'odb_data':
                    Magics.setc('odb_filename', self.args[key])
                else:
                    val+= '%s%s = "%s"'%(sep, key, self.args[key])
            elif isinstance(self.args[key], int):
                val +=  '%s%s = %d'%(sep, key, self.args[key])
            elif isinstance(self.args[key], float):
                val += '%s%s = %0.2f'%(sep, key, self.args[key])
            elif isinstance(self.args[key], list) :
                if isinstance (self.args[key][0], str):
                   vval = ""
                   vsep = ""
                   if (len(self.args[key]) < 5 ):
                        for v in self.args[key]:
                            vval += vsep + self.quote(v)
                            vsep = ", "
                   else :
                        vval = self.quote(self.args[key][0]) + ", " + self.quote(self.args[key][1]) + ",...," + self.quote(self.args[key][-2]) +  ", " + self.quote(self.args[key][-1])
                   vval += ""
                   val+= '%s%s = [%s]'%(sep, key, vval)
                elif isinstance(self.args[key][0], int):
                   vval = ""
                   vsep = ""
                   if (len(self.args[key]) < 5 ):
                        for v in self.args[key]:
                            vval += vsep + ("%df"%v)
                            vsep = ", "
                   else :
                            vval = ("%d"%self.args[key][0]) + ", " + ("%d"%self.args[key][1]) + ",...," + ("%d"%self.args[key][-2]) +  ", " + ("%d"%self.args[key][-1])
                   vval += ""
                   val+= '%s%s = %s'%(sep, key, vval)
                elif isinstance(self.args[key][0], float):
                   vval = ""
                   vsep = ""
                   if (len(self.args[key]) < 5 ):
                        for v in self.args[key]:
                            vval += vsep + ("%0.2f"%v)
                            vsep = ", "
                   else :
                        vval = ("%0.2f"%self.args[key][0]) + ", " + ("%0.2f"%self.args[key][1]) + ",...," + ("%0.2f"%self.args[key][-2]) +  ", " + ("%0.2f"%self.args[key][-1])

                   vval += ""
                   val+= '%s%s = [%s]'%(sep, key, vval)
            elif isinstance(self.args[key], numpy.ndarray) :
                type = self.args[key].dtype
                dim  = len(self.args[key].shape)
                if isinstance(self.args[key][0], int):
                    if (dim == 2) :
                        print("pset2i")
                    else :
                        print("pset1i")
                elif ( type == 'float64' or type == 'float32') :
                    if (dim == 2) :
                        print("pset2r")
                    else :
                        vval = ""
                        vsep = ""
                        if (len(self.args[key]) < 5 ):
                            for v in self.args[key]:
                                vval += vsep + ("%0.2f"%v)
                                vsep = ", "
                        else :
                            vval = ("%0.2f"%self.args[key][0]) + ", " + ("%0.2f"%self.args[key][1]) + ",...," + ("%0.2f"%self.args[key][-2]) +  ", " + ("%0.2f"%self.args[key][-1])
                        vval += ""
                        val+= '%s%s = [%s]'%(sep, key, vval)
                else :
                    print("type???->", key)
            sep=",\n\t"

        print(file, val + ")\n")

    def tomv4(self, file):
        sep="\t"
        val="%s,\n"%self.verb.upper()

        for key in list(self.args.keys()):
            if isinstance(self.args[key], str):
                if key == 'odb_data':
                    Magics.setc('odb_filename', self.args[key])
                else:
                    val+= '%s%s = %s'%(sep, key.upper(), self.args[key].upper())
            elif isinstance(self.args[key], int):
                val +=  '%s%s = %d'%(sep, key.upper(), self.args[key])
            elif isinstance(self.args[key], float):
                val += '%s%s = %0.2f'%(sep, key.upper(), self.args[key])
            elif isinstance(self.args[key], list) :
                if isinstance (self.args[key][0], str):
                   vval = "["
                   vsep = ""
                   for v in self.args[key]:
                        vval += vsep + v
                        vsep = ", "
                   vval += "]"
                   val+= '%s%s = %s'%(sep, key.upper(), vval)
                elif isinstance(self.args[key][0], int):
                   print("pset1i")
                elif isinstance(self.args[key][0], float):
                   print("pset1r")
            elif isinstance(self.args[key], numpy.ndarray) :
                type = self.args[key].dtype
                dim  = len(self.args[key].shape)
                if isinstance(self.args[key][0], int):
                    if (dim == 2) :
                        print("pset2i")
                    else :
                        print("pset1i")
                elif ( type == 'float64' or type == 'float32') :
                    if (dim == 2) :
                        print("pset2r")
                    else :
                        vval = "["
                        vsep = ""
                        for v in self.args[key]:
                                vval += vsep + ("%0.2f"%v)
                                vsep = ", "
                        vval += "]"
                        val+= '%s%s = %s'%(sep, key.upper(), vval)

                else :
                    print("type???->", key)
            sep=",\n\t"

        print(file, val + "\n")





    def tofortran(self, f):
        if self.action == Magics.new_page :
            print(f, '\tcall pnew("page")')
            return
        for key in list(self.args.keys()):
            if isinstance(self.args[key], str):
                if key == 'odb_data':
                    Magics.setc('odb_filename', self.args[key])
                else:
                    print (f, '\tcall psetc("%s", "%s")'%(key, self.args[key]))
            elif isinstance(self.args[key], int):
                print (f, '\tcall pseti("%s", %d)'%(key, self.args[key]))
            elif isinstance(self.args[key], float):
                print (f, '\tcall psetr("%s", %0.2f)'%(key, self.args[key]))
            elif isinstance(self.args[key], list) :
                if isinstance (self.args[key][0], str):
                   nb = 0
                   for v in self.args[key]:
                        nb = max(nb, len(v))

                   val = "(/"
                   sep = ""
                   newline = 70
                   for v in self.args[key]:
                        val += sep + self.quote(v.ljust(nb))
                        sep = ", "
                        if len(val) > newline  :
                            sep = ",&\n\t\t"
                            newline = newline + 70
                   val += "/)"
                   print (f, '\tcall pset1c("%s", %s, %d)'%(key, val, len(self.args[key])))
                elif isinstance(self.args[key][0], int):
                   print("pset1i")
                elif isinstance(self.args[key][0], float):
                    val = "(/"
                    sep = ""
                    for v in self.args[key]:
                        val += sep + ("%0.2f" % v)
                        sep = ", "
                    val += "/)"
                    print (f, '\tcall pset1r("%s", %s, %d)'%(key, val, len(self.args[key])))
            elif isinstance(self.args[key], numpy.ndarray) :
                type = self.args[key].dtype
                dim  = len(self.args[key].shape)
                if isinstance(self.args[key][0], int):
                    if (dim == 2) :
                        print("pset2i")
                    else :
                        print("pset1i")
                elif ( type == 'float64' or type == 'float32') :
                    if (dim == 2) :
                        print("pset2r")
                    else :
                        val = "(/"
                        sep = ""
                        for v in self.args[key]:
                            val += sep + ("%0.2f" % v)
                            sep = ", "
                        val += "/)"
                        print (f, '\tcall pset1r("%s", %s, %d)'%(key, val, len(self.args[key])))
                elif isinstance(self.args[key][0], int):
                        print("pset1r")
                else :
                    print("type???->", key)

        if self.action != None and actions[self.verb] != "" and actions[self.verb] != "pinput":
            print (f, "\tcall %s\n"%actions[self.verb])
            for key in list(self.args.keys()):
                print (f, "\tcall preset('%s')"%key)
            print (f, "")

        else:
            print (f, "")


    def clean_object(self, obj):
      if sys.version_info[0] < 3:
        if type(obj) in (int, float, str, bool, numpy.float64):
            return obj
        elif type(obj) == unicode:
            return str(obj)
        elif type(obj) in (list, tuple, set, numpy.ndarray) and len(obj):
            if type(obj[0]) != unicode:
                return obj
            obj = list(obj)
            for i,v in enumerate(obj):
                obj[i] = self.clean_object(v)
        elif type(obj) == dict:
            for i,v in list(obj.items()):
                obj[i] = self.clean_object(v)
        else:
            print("Invalid object in data, converting to string: ")
            print(type(obj))
            obj = str(obj)
      return obj

    def find_type(self, data):
        for v in data:
            if not isinstance(v, int):
                return "float"
        return "int"

    def execute(self):

        if ( self.action != Magics.odb) :
            self.args = self.clean_object(self.args)
        for key in list(self.args.keys()):
            if isinstance(self.args[key], str):
                if key == 'odb_data':
                    Magics.setc('odb_filename', self.args[key])
                else:
                    Magics.setc(key, self.args[key])
            elif isinstance(self.args[key], int):
                Magics.seti(key, self.args[key])
            elif isinstance(self.args[key], float):
                Magics.setr(key, self.args[key])
            elif isinstance(self.args[key], list) and len(self.args[key]):
                if isinstance(self.args[key][0], str):
                   Magics.set1c(key, self.args[key])
                else:
                    type = self.find_type(self.args[key])
                    if type == "int":
                        Magics.set1i(key, numpy.array(self.args[key], dtype='i'))
                    else :
                        Magics.set1r(key, numpy.array(self.args[key]))
            elif isinstance(self.args[key], numpy.ndarray) :
                type = self.args[key].dtype
                data = self.args[key].copy() 
                size = data.shape
                dim  = len(size)
                type = self.find_type(self.args[key])
                if type == "int":
                    if (dim == 2) :
                        Magics.set2i(key, numpy.int64(data), size[0], size[1])
                    else :
                        Magics.set1i(key, numpy.int64(data), size[0])
                elif type == "float": 
                    if (dim == 2) :                      
                        Magics.set2r(key, numpy.float64(data), size[1], size[0])
                    else :
                        Magics.set1r(key, numpy.float64(data))
                else :
                    print("can not interpret type %s for %s ???->", (type, key) ) 
            else:
                self.args[key].execute(key)

        if self.action != None :
            if self.action != Magics.new_page :
                if self.action == Magics.legend :
                    Magics.setc("legend", "on")
                self.action()
                if self.action != Magics.obs and self.action != Magics.minput:
                    for key in list(self.args.keys()):
                        Magics.reset(key)
            else:
                self.action("page")

def make_action(verb, action, html=""):
    def f(_m = None,**kw):
        args = {}
        if _m is not None:
            args.update(_m)
        args.update(kw)
        return Action(verb, action, html, args)
    return f

mcoast = make_action("mcoast", Magics.coast, "Coastlines")
pcoast = make_action("pcoast", Magics.coast)
maxis = make_action("maxis", Magics.axis, "Axis")
paxis = make_action("paxis", Magics.axis)
mcont = make_action("mcont", Magics.cont, "Contouring")
pcont = make_action("pcont", Magics.cont)
msymb = make_action("msymb", Magics.symb, "Symbol")
psymb = make_action("psymb", Magics.symb)
pimport = make_action("pimport", Magics.mimport)
mimport = make_action("mimport", Magics.mimport)
mtaylor = make_action("mtaylor", Magics.taylor)
mgeo = make_action("mgeo", Magics.geo)
pgeo = make_action("pgeo", Magics.geo)
pgrib = make_action("pgrib", Magics.grib, "Grib+Input")
mgrib = make_action("mgrib", Magics.grib, "Grib+Input")
pmapgen = make_action("pmapgen", Magics.mapgen)
mmapgen = make_action("mmapgen", Magics.mapgen)
pnetcdf = make_action("pnetcdf", Magics.netcdf)
mnetcdf = make_action("mnetcdf", Magics.netcdf)
odb_geopoints = make_action("odb_geopoints", Magics.odb, "Odbviewer")
odb_geovectors = make_action("odb_geovectors", Magics.odb,"Odbviewer" )
odb_xypoints = make_action("odb_xypoints", Magics.odb, "Odbviewer")
odb_xyvectors = make_action("odb_xyvectors", Magics.odb, "Odbviewer")
pmap = make_action("pmap", None, "Subpage")
mmap = make_action("mmap", None, "Subpage")
plegend = make_action("plegend", Magics.legend, "Legend")
mlegend = make_action("mlegend", Magics.legend, "Legend")
ptext = make_action("mtext", Magics.text)
mtext = make_action("mtext", Magics.text)
output = make_action("output", None, "PNG Output")
pwind = make_action("pwind", Magics.wind, "Wind+Plotting")
mwind = make_action("mwind", Magics.wind, "Wind+Plotting")
pline = make_action("pline", Magics.line)
mline = make_action("mline", Magics.line)
pgraph = make_action("pgraph", Magics.graph, "Graph+Plotting")
mgraph = make_action("mgraph", Magics.graph, "Graph+Plotting")
pboxplot = make_action("pboxplot", Magics.boxplot)
mboxplot = make_action("mboxplot", Magics.boxplot)
pobs = make_action("pobs", Magics.obs)
mobs = make_action("mobs", Magics.obs)
page = make_action("page", Magics.new_page)
pinput = make_action("pinput", Magics.minput)
minput = make_action("minput", Magics.minput, "Input+Data")
mtable = make_action("mtable", Magics.mtable, "CSV+Table+Decoder")


mgeojson = make_action("mgeojson", Magics.geojson, "GeoJSon")
mwrepjson = make_action("mwrepjson", Magics.wrepjson, "WrepJSon")
mepsinput = make_action("mepsinput", Magics.epsinput, "EpsInput")
mepscloud = make_action("mepscloud", Magics.epscloud)
mepslight = make_action("mepslight", Magics.epslight)
mepsbar = make_action("mepsbar", Magics.epsbar)
mepswind = make_action("mepswind", Magics.epswind)
mepswave = make_action("mepswave", Magics.epswave)
mepsshading = make_action("mepsshading", Magics.epsshading)
mepsgraph = make_action("mepsgraph", Magics.epsgraph)
mepsplumes = make_action("mepsplumes", Magics.epsplumes)
mtephi = make_action("mtephi", Magics.tephi)

mmetgraph = make_action("mmetgraph", Magics.metgraph)
mmetbufr = make_action("mmetbufr", Magics.metbufr)

def examine(*args):
    for n in args:
        try :
            n.inspect()
        except:
            break

def _execute(o):
	if isinstance(o, list) or isinstance(o, tuple):
		for x in o:
			_execute(x)

	else:

		o.execute()

def _plot(*args):
    Magics.init()
    for n in args:
        _execute(n)

    #Collect the drivers!
    Magics.finalize()
    for f in context.tmp:
        if os.path.exists(f):
            os.remove(f)




def tofortran(file, *args):
    return
    f = open(file+".f90",'w')
    print(f, "\tprogram magics\n")
    print(f, "\tcall popen\n")
    for n in args:
        n.tofortran(f)
    print(f, "\tcall pclose\n")
    print(f, "\tend")


def tohtml(file, *args):
    return
    f = open(file+".html",'w')
    print (f, "<html>")
    for n in args:
        n.tohtml(f)
    print (f, "</html>")

def tomv4(file, *args):
    return
    f = open(file+".mv4",'w')
    for n in args:
        n.tomv4(f)

class  odb_filter(object):
    def __init__(self, _m = None,**kw):
        args = {}
        self.verb = "odbfilter"
        if _m is not None:
            args.update(_m)
        self.args = args
    def execute(self, key):
        file = "data%d" % numpy.random.randint(1,1000)
        odb = "%s.odb" % file
        context.tmp.append(odb)
        cmd = "odbsql -q \"" + self.args["query"] + "\" -i " + self.args["path"] + " -f newodb -o " + odb
        print(cmd)
        if (os.system(cmd)) :
            print("Error in filtering ODB data... Aborting")
            os.abort();
        Magics.setc('odb_filename', odb)
    def inspect(self):
        cmd = "odbsql -q \"" + self.args["query"] + "\" -i " + self.args["path"] + " -o data.ascii"
        if (os.system(cmd)) :
            print("Error in filtering ODB data... Aborting")
            os.abort();
        cmd =  os.environ['ODB_REPORTER'] + " %s" % "data.ascii"
        if (os.system(cmd)) :
            print("Error in viewing ODB data... Aborting")
            os.abort();


import threading
import tempfile

try:
    from IPython.display import Image

    LOCK = threading.Lock()

    def plot(*args):

        with LOCK:
            f, tmp = tempfile.mkstemp(".png")
            os.close(f)

            base, ext = os.path.splitext(tmp)

            img = output(output_formats=["png"],
                              output_name_first_page_number='off',
                              output_name=base)
            
            all = []
            all.append(img)
            for i in args :
              all.append(i)
            _plot(all)

            image = Image(tmp)
            os.unlink(tmp)
            return image
except ImportError:
    plot = _plot



def wmsstyles(data):

    f, tmp = tempfile.mkstemp(".json")
    os.close(f)
    os.environ["MAGPLUS_QUIET"] =  "on"
    os.environ["MAGPLUS_WARNING"] =  "off"

    f,img = tempfile.mkstemp(".png")
    os.close(f)

    base, ext = os.path.splitext(img)

    out = output(output_formats=["png"],
                      output_name_first_page_number='off',
                      output_name=base)

    meta = mmap(
            metadata_wms_file = tmp
            )
    #Define the simple contouring for z500
    _plot(out, meta, data, mcont(contour_automatic_setting = "web", contour_legend_only = "on")  )
    with open(tmp) as data_file:
       
        info = json.load(data_file)
    os.unlink(tmp)
    os.unlink(img)

    return info

def wmscrs():
    os.environ["MAGPLUS_QUIET"] =  "on"
    os.environ["MAGPLUS_WARNING"] =  "off"
    return { "crss" : [
                {
                    "name" : "EPSG:4326",
                    "min_x" : -180.,
                    "min_y" : -90,
                    "max_x" : 180,
                    "max_y" : 90
                },
                 {
                    "name" : "EPSG:3857",
                    "min_x" : -20026376.39,
                    "min_y" : -20048966.10,
                    "max_x" : 20026376.39,
                    "max_y" : 20048966.10
                },
                {
                    "name" : "EPSG:3857",
                    "min_x" : -20026376.39,
                    "min_y" : -20048966.10,
                    "max_x" : 20026376.39,
                    "max_y" : 20048966.10
                },
                {
                    "name" : "EPSG:32661",
                    "min_x" : 1994055.62,
                    "min_y" : 5405875.53,
                    "max_x" : 2000969.46,
                    "max_y" : 2555456.55
                },
                ],


              "geographic_bounding_box" : {
                    "w_lon" : -180,
                    "e_lon" : 180,
                    "s_lat" : -90,
                    "n_lat" : 90
              }
            
            }
                
    