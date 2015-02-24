import os
import Magics
import numpy

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
		for key in self.args.keys():
			x = x + " %s = '%s'\n" % (key, self.args[key])
		return x

	def inspect(self):
		print self

	def quote(self, v):
		return "\"" + v + "\""

	def tohtml(self, file):
		sep=""
		val="%s("%self.html

		for key in self.args.keys():
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
				   		print "pset2i" 
					else :
				   		print "pset1i" 
				elif ( type == 'float64' or type == 'float32') :
					if (dim == 2) :
				   		print "pset2r" 
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
					print "type???->", key
			sep=",\n\t"
				
		print >>file, val + ")\n"

	def tomv4(self, file):
		sep="\t"
		val="%s,\n"%self.verb.upper()

		for key in self.args.keys():
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
				   print "pset1i"
				elif isinstance(self.args[key][0], float):
				   print "pset1r" 
			elif isinstance(self.args[key], numpy.ndarray) :
				type = self.args[key].dtype
				dim  = len(self.args[key].shape)
				if isinstance(self.args[key][0], int):
					if (dim == 2) :
				   		print "pset2i" 
					else :
				   		print "pset1i" 
				elif ( type == 'float64' or type == 'float32') :
					if (dim == 2) :
				   		print "pset2r" 
					else :
				   		vval = "["
				   		vsep = ""
				   		for v in self.args[key]:
				   				vval += vsep + ("%0.2f"%v)
				   				vsep = ", "
				   		vval += "]"
				   		val+= '%s%s = %s'%(sep, key.upper(), vval)
				   		
				else :
					print "type???->", key
			sep=",\n\t"
				
		print >> file, val + "\n"





	def tofortran(self, f):
		if self.action == Magics.new_page :
			print >> f, '\tcall pnew("page")'
			return
		for key in self.args.keys():
			if isinstance(self.args[key], str):
				if key == 'odb_data':
					Magics.setc('odb_filename', self.args[key])
				else:
					print >> f, '\tcall psetc("%s", "%s")'%(key, self.args[key])
			elif isinstance(self.args[key], int):
				print >>f, '\tcall pseti("%s", %d)'%(key, self.args[key])
			elif isinstance(self.args[key], float):
				print >> f, '\tcall psetr("%s", %0.2f)'%(key, self.args[key])
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
				   print >>f, '\tcall pset1c("%s", %s, %d)'%(key, val, len(self.args[key]))
				elif isinstance(self.args[key][0], int):
				   print "pset1i"
				elif isinstance(self.args[key][0], float):
				   	val = "(/"
				   	sep = ""
				   	for v in self.args[key]:
				   		val += sep + ("%0.2f" % v)
				   		sep = ", "
				   	val += "/)"
				   	print >>f, '\tcall pset1r("%s", %s, %d)'%(key, val, len(self.args[key]))
			elif isinstance(self.args[key], numpy.ndarray) :
				type = self.args[key].dtype
				dim  = len(self.args[key].shape)
				if isinstance(self.args[key][0], int):
					if (dim == 2) :
				   		print "pset2i" 
					else :
				   		print "pset1i" 
				elif ( type == 'float64' or type == 'float32') :
					if (dim == 2) :
				   		print "pset2r" 
					else :
				   		val = "(/"
				   		sep = ""
				   		for v in self.args[key]:
				   			val += sep + ("%0.2f" % v)
				   			sep = ", "
				   		val += "/)"
				   		print >>f, '\tcall pset1r("%s", %s, %d)'%(key, val, len(self.args[key]))
				elif isinstance(self.args[key][0], int):
				   		print "pset1r" 
				else :
					print "type???->", key

		if self.action != None and actions[self.verb] != "" and actions[self.verb] != "pinput":
			print >>f, "\tcall %s\n"%actions[self.verb] 
			for key in self.args.keys():
				print >>f, "\tcall preset('%s')"%key 
			print >>f, ""

		else:
			print >>f, ""


	def clean_object(self, obj):

		if type(obj) in (int, float, str, bool, numpy.float64):
			return obj
		elif type(obj) == unicode:
			return str(obj)
		elif type(obj) in (list, tuple, set, numpy.ndarray):
			if type(obj[0]) != unicode:
				return obj
			obj = list(obj)
			for i,v in enumerate(obj):
				obj[i] = self.clean_object(v)
		elif type(obj) == dict:
			for i,v in obj.iteritems():
				obj[i] = self.clean_object(v)
		else:
			print "Invalid object in data, converting to string: " 
			print  type(obj)
			obj = str(obj) 
		return obj


	def execute(self):

		if ( self.action != Magics.odb) :
		    self.args = self.clean_object(self.args)
		for key in self.args.keys():
			if isinstance(self.args[key], str):
				if key == 'odb_data':
					Magics.setc('odb_filename', self.args[key])
				else:
					Magics.setc(key, self.args[key])
			elif isinstance(self.args[key], int):
				Magics.seti(key, self.args[key])
			elif isinstance(self.args[key], float):
				Magics.setr(key, self.args[key])
			elif isinstance(self.args[key], list) :
				if isinstance(self.args[key][0], str):
				   Magics.set1c(key, self.args[key])
				elif isinstance(self.args[key][0], int):
				   Magics.set1i(key, numpy.array(self.args[key], dtype='i'))
				elif isinstance(self.args[key][0], float):
				   Magics.set1r(key, numpy.array(self.args[key]))
			elif isinstance(self.args[key], numpy.ndarray) :
				type = self.args[key].dtype
				dim  = len(self.args[key].shape)
				if isinstance(self.args[key][0], int):
					if (dim == 2) :
						Magics.set2i(key, self.args[key].copy())
					else :
						Magics.set1i(key, self.args[key].copy())
				elif ( type == 'float64' or type == 'float32') :
					if (dim == 2) :
						Magics.set2r(key, self.args[key].copy())
					else :
						Magics.set1r(key, self.args[key].copy())
				else :
					print "type???->", key

			else:
				self.args[key].execute(key)

		if self.action != None :
			if self.action != Magics.new_page :
				if self.action == Magics.legend :
					Magics.setc("legend", "on")
				self.action()
				if self.action != Magics.obs and self.action != Magics.minput:
					for key in self.args.keys():
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


mwrepjson = make_action("mwrepjson", Magics.wrepjson, "WrepJSon")
mepsinput = make_action("mepsinput", Magics.epsinput, "EpsInput")
mepscloud = make_action("mepscloud", Magics.epscloud)
mepsbar = make_action("mepsbar", Magics.epsbar)
mepswind = make_action("mepswind", Magics.epswind)
mepswave = make_action("mepswave", Magics.epswave)
mepsshading = make_action("mepsshading", Magics.epsshading)
mepsgraph = make_action("mepsgraph", Magics.epsgraph)
mepsplumes = make_action("mepsplumes", Magics.epsplumes)
mtephi = make_action("mtephi", Magics.tephi)


def examine(*args):
	for n in args:
		try :
			n.inspect()
		except:
			break


def plot(*args):
	Magics.init()
	for n in args:
		if isinstance(n, list):
			for nn in n:
				nn.execute()
		else:
				n.execute()
	
	#Collect the drivers!
	Magics.finalize()
	for f in context.tmp:
		if os.path.exists(f):
			os.remove(f)


def tofortran(file, *args):
	f = open(file+".f90",'w')
	print >>f, "\tprogram magics\n"
	print >>f, "\tcall popen\n"
	for n in args:
		n.tofortran(f)
	print >>f, "\tcall pclose\n"
	print >>f, "\tend"


def tohtml(file, *args):
	f = open(file+".html",'w')
	print >>f, "<html>"
	for n in args:
		n.tohtml(f)
	print >>f, "</html>"

def tomv4(file, *args):
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
		print cmd 
		if (os.system(cmd)) :
			print "Error in filtering ODB data... Aborting"
			os.abort();
		Magics.setc('odb_filename', odb)
	def inspect(self):
		cmd = "odbsql -q \"" + self.args["query"] + "\" -i " + self.args["path"] + " -o data.ascii"
		if (os.system(cmd)) :
			print "Error in filtering ODB data... Aborting"
			os.abort();
		cmd =  os.environ['ODB_REPORTER'] + " %s" % "data.ascii"
		if (os.system(cmd)) :
			print "Error in viewing ODB data... Aborting"
			os.abort();
