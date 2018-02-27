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


if(len(sys.argv) != 6) :
    print("\n\tYou need to give 4 input parameters:")
    print("\n\t  %s source.xml targetDef CLASS_NAME rulesDef\n",sys.argv[0])
    sys.exit()



# ObjectHandler::optionalparams
# - will store dictionary with optional param name as key, and list of tuples as value
# - each tuple is a pair: (conditional_param_name, value-to-enable)
# e.g.
# {"symbol_text_font_name": (("symbol_type", "text"), ("symbol_type", "both"))}
# - which means that if "symbol_type" is either "text" or "both", then "symbol_text_font_name"
#   should be enabled; or in Metview rules terms, if "symbol_type is neither"
#   "text" nor "both", then "symbol_text_font_name" should be disabled.
#   NOTE: we make an assumption - if two conditional parameters are the same (e.g.
#   "symbol_type" in the above example), then the relationship is %or, otherwise it
#   is %and. This should be the case because of the way the .xml files are arranged.

# ObjectHandler::optionalparams2
# - a reorganised version of ObjectHandler::optionalparams
# - this time, the keys are the tuples of conditions and the values are the
#   set of parameters that are to be set. This is so that we can combine all
#   the %unset parameters together under the same condition.


# ObjectHandler::optionalparams3
# - an ordered version of optionalparams2 using a tuple instead of a dictionary (so that we
#   can ensure a 'working' order of the conditions, ie make it impossible to unset a parameter
#   before it needs to be queried, because this causes an error in applying the rules).
#   (  ((condition1,condition2), (unsetparam1,unsetparam2)),  ((condition3,condition4), (unsetparam3,unsetparam4)) )


# ObjectHandler::classes
# - stores all the parsed class names along with their associated parameters, e.g.
# {"MarkerShadingTechnique": {"inherits": ["ShadingTechnique"],
#                             "params": ["contour_shade_colour_table", "contour_shade_height_table"]}}

class ObjectHandler(ContentHandler):
	title = ""
	classname = ""
	defparam = ""
	tab1 = 0
	tab2 = 2
	ignore = False
	mydef = []   # the growing list of parameters
	param = ""
	myoptions = []
	optionalparams  = {}
	optionalparams2 = {}  # see reorganiseOptionalParameters()
	optionalparams3 = ()
	myrules = {}
	objects = []
	last = ""   # the currently-parsed parameter
	classes = {}
	#inheritedlast = ""
	unset = {}
	filehistory = []  # a list of files we have opened (so that we don't open the same one twice)
	debug = 0  # gives some output on stdout
	toplevel = True

	def setTopLevel(self, top):
		self.toplevel = top


	def default(self, attrs):
		val = attrs.get("metview_default");
		if val is None:
			val = attrs.get("default");

		if (val == "") :
			val = attrs.get("default");
		if (val == ".") :
			val = "'.'";
		if (val.upper() == "-INT_MAX"):
			return "-1.0E21"
		if (val.upper() == "INT_MAX"):
			return "1.0E21"
		if (val.upper() == "-LLONG_MAX"):
			return "-1.0E21"
		if (val.upper() == "LLONG_MAX"):
			return "1.0E21"
		if (val == "floatarray()"):
			return "''"
		if (val == "intarray()"):
			return "''"
		if (val == "stringarray()"):
			return "''"
		if (len(val) == 0):
			return "''"
		if (val == "(automatic)"):
			return "'(AUTOMATIC)'"
		return val

	def tab(self, n):
		sep = ''
		i = 0
		while i < n:
			sep = sep + '\t'
			i = i + 1
		return sep

	def nice(self, name, attrs):
		sep1 = self.tab(self.tab1)
		sep2 = self.tab(self.tab2)
		self.file.write("%s<%s " % (sep1.upper(), name.upper()))
		for key in attrs.getNames():
			self.file.write(" %s='%s'" % (key.upper() , attrs.getValue(key).upper()))
		self.file.write(">\n")

	def characters(self, data):
		pass


	# addhidden - if the appropriate flag is set in the attributes, return the text that will
	#             make the parameter hidden (but available) in Metview
	def addhidden(self, attrs):
		if attrs.get("metview") == 'hidden':
			return "\t[ visible = false ]\n"
		else:
			return ""

	def toggle(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\tON;  ON\n"
		s = s + "\t\tOFF; OFF\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s


	def any(self, attrs):
		v = attrs.get("values")
		if  v != None:
			return	self.list(attrs)
		x = -1
		for key in self.predefined_list:
			x = self.param.find(key)
			if x != -1 :
				f = self.predefined[key]
				# only use the predefined function if the seach string
				# occurs at the end of the parameter name
				if x + len(key) == len(self.param):
					self.newparam(self.param, f(self, attrs), self.default(attrs))
					return
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t@\n"
		default_value = self.default(attrs)
		# special case: html tags seem to have their quotes removed, so we need to put them back
		if len(default_value) > 0 and default_value[0] == "<":
			default_value = "'" + default_value + "'"
		else:
			default_value = default_value.upper()
		s = s + "\t} = %s\n" % default_value
		return s


	def number(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t*\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s

	def listofnumbers(self, attrs):
		if (attrs.get("name").upper() == "OUTPUT_FRAME_LIST"):
			s = "\n"
			return s
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t*\n"
		s = s + "\t\t/\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s

	def listofstrings(self, attrs):
		if (attrs.get("colourlist") == "on" ) :
			 s = "\t%s [ help = help_colour,interface = colour ]\n%s\t{ \n" % (attrs.get("name").upper(), self.addhidden(attrs))
			 s = s + "\t\t&PARAMSHARE&COLOUR"
			 s = s + "\t\t/\n"
			 s = s + "\t} = %s\n" % self.default(attrs).upper()
		elif (attrs.get("countrylist") == "on" ):
			s = "\t%s [ help = help_multiple_selection, exclusive = False ]\n%s\t{\n" % (attrs.get("name").upper(),self.addhidden(attrs) )
			countries = attrs.get("values").split("/")
			for c in countries:
				parts = c.split(':')
				shortname = parts[0]
				longname = parts[1].replace(',', ' ') # cannot have special characters here
				longname = longname.replace('(', ' ')
				longname = longname.replace(')', ' ')
				s = s + "\t\t%s;%s\n" %(longname.upper(), shortname.upper())
			s = s + "\t\t*\n"
			s = s + "\t\t/\n"
			s = s + "\t} = %s\n" % self.default(attrs).upper()
		else:
			s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
			s = s + "\t\t@\n"
			s = s + "\t\t/\n"
			s = s + "\t} = %s\n" % self.default(attrs).upper()

		return s


	def colour(self, attrs):
		s = "\t%s [ help = help_colour,interface = colour ]\n%s\t{ \n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t&PARAMSHARE&COLOUR\n"
		mi = attrs.get("metview_interface")
		if mi == "ColourWithExtra":
			s = s + "\t\t%s\n" % self.default(attrs).upper()
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s

	def colourtechnique(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\tCALCULATE;CALCULATE\n"
		s = s + "\t\tLIST;LIST\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s.upper()

	def linestyle(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t&PARAMSHARE&STYLE\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s

	def policy(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name"), self.addhidden(attrs))
		s = s + "\t\tlastone;lastone\n"
		s = s + "\t\tcycle;cycle\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s.upper()

	def fontstyle(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\tnormal;normal\n"
		s = s + "\t\tbold;bold\n"
		s = s + "\t\titalic;italic\n"
		s = s + "\t\tbolditalic;bolditalic\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s.upper()

	def justification(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\tleft;left\n"
		s = s + "\t\tcentre;centre\n"
		s = s + "\t\tright;right\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()

		return s.upper()
	def font(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		s = s + "\t\t&PARAMSHARE&FONT\n"
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s.upper()

	def quality(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name"), self.addhidden(attrs))
		s = s + "\t\thigh;high\n"
		s = s + "\t\tmedium;medium\n"
		s = s + "\t\tlow;low\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s.upper()

	def list(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		keys = attrs.get("values").split("/")
		for key in keys:
			s = s + "\t\t%s;%s\n" %(key.upper(), key.upper())
		s = s + "\t} = %s\n" % self.default(attrs).upper()
		return s

	def options(self, attrs):
		s = "\t%s\n%s\t{\n" % (attrs.get("name").upper(), self.addhidden(attrs))
		self.defparam = self.default(attrs).upper()
		return s

	predefined = {"font_style" : fontstyle,
			  "font": font,
			  "quality": quality
			  }

	predefined_list = ["font_style", "font", "quality" ]

	types = { "bool"            : toggle,
			  "string"          : any,
			  "Path"            : any,
			  "Position"        : any,
			  "int"             : number,
			  "Colour"          : colour,
			  "ColourWithExtra" : colour,
			  "ColourTechnique" : colourtechnique,
			  "LineStyle"       : linestyle,
			  "Justification"   : justification,
			  "ListPolicy"      : policy,
			  "floatarray"      : listofnumbers,
			  "intarray"        : listofnumbers,
			  "stringarray"     : listofstrings,
			  "float"           : number}

	def newparam(self, param, value, default):
		#print('  newparam: ', param, ' ', value, ' ', default)
		for p in self.mydef:
			if p[0] == param:
				return
		self.mydef.append((param, value, default))


	def addOptionalParam(self, paramname, requirements):
		if paramname in self.optionalparams.keys():
			if not(requirements[0] in self.optionalparams[paramname]):
				self.optionalparams[paramname] = self.optionalparams[paramname] + requirements
		else:
			self.optionalparams[paramname] = requirements


	def addRelatedOptionalRequirements(self):
		optparams = self.optionalparams.keys()
		for optparam in optparams:
			conditions = self.optionalparams[optparam]
			for condition in conditions:
				if condition[0] in optparams:
					self.addOptionalParam(optparam, (condition,))


	def addRelatedOptionalRequirements2(self):
		optparams = self.optionalparams.keys()
		for optparam in optparams:
			conditions = self.optionalparams[optparam]
			for condition in conditions:
				if condition[0] in optparams:
					addreqs = self.optionalparams[condition[0]]
					self.addOptionalParam(optparam, addreqs)




	# see top of file for description of optionalparams and optionalparams2
	def reorganiseOptionalParameters(self):
		for optparam in self.optionalparams.keys():
			condition = self.optionalparams[optparam]
			if condition in self.optionalparams2.keys():
				self.optionalparams2[condition].add(optparam)
			else:
				self.optionalparams2[condition] = set([optparam])


	# put the dictionary in optionalparams2 into tuple in optionalparams3 like this:
	#   (  ((condition1,condition2), (unsetparam1,unsetparam2)),  ((condition3,condition4), (unsetparam3,unsetparam4)) )
	# see comments at top of file
	def ensureOptionalParameters2DependenciesAreInRightOrder(self):
		self.optionalparams3 = () # empty tuple, will contain an ordered version of optionalparams2

		for conditions2 in self.optionalparams2.keys():
			# does this rule unset something that is needed in the optionalparams3 rules?
			unsetparams2 = self.optionalparams2[conditions2]
			newtuple3 = (conditions2, unsetparams2)
			position = 0
			foundconflict = False
			for clause3 in self.optionalparams3:  # look through the new set of rules
				unsetparams3 = clause3[1]
				for enquireparam2 in conditions2:
					if enquireparam2[0] in unsetparams3:  # this clause2 should go *after* the current clause3
						foundconflict = True
						break
				if foundconflict:
					break

				position = position + 1

			# we now know that the clause should go into position 'position' in the new tuple
			if self.debug:
				print("put into position " + str(position))
			if position == 0:
				firstpart = ()
				secondpart = (newtuple3,)
				lastpart = self.optionalparams3
			else:
				if position == len(self.optionalparams3):
					firstpart = self.optionalparams3
					secondpart = (newtuple3,)
					lastpart = ()
				else:
					firstpart = self.optionalparams3[0:position]
					secondpart = (newtuple3,)
					lastpart = self.optionalparams3[position:len(self.optionalparams3)]

			self.optionalparams3 = firstpart + secondpart + lastpart




	def newClass(self, c):
		if not (c in self.classes.keys()):
			self.classes[c] = {"inherits_reqs_from": set(),   # 'inherits_params_from' is an empty set
			                   "inherits_params_from": set()}
			#print "newClass " + c
			#print self.classes


	def addParameterToClass(self, p, c):
		self.newClass(c)
		params = self.classes[c].get("params", [])
		if not (p in params):
			params.append(p)
			self.classes[c]["params"] = params


	def addParamsFromOneClassToAnother(self, cfrom, cto):
		for p in self.classes[cfrom].get("params", []):
			#print "Adding params from " + cfrom + " to " + cto
			self.addParameterToClass(p, cto)



	def addRequirementToClass(self, reqparam, reqval, c):
		#print "\naddRequirementToClass " + reqparam + " " + reqval + " " + c
		self.newClass(c)
		reqs = self.classes[c].get("reqs", ())
		newreq = ((reqparam, reqval),)
		if newreq[0] not in reqs:
			reqs = reqs + newreq
			self.classes[c]["reqs"] = reqs


	# the purpose of collecting the class details was to add some new
	# optional parameter details - we do that here, adding the information
	# from self.classes to self.optionalparams
	# {"symbol_text_font_name": (("symbol_type", "text"), ("symbol_type", "both"))}
	def addClassDetailsToOptionalParameters(self):
		for c in self.classes:
			reqs = self.classes[c].get("reqs", ())
			params = self.classes[c].get("params", ())
			for p in params:
				x = reqs
				if x != ():
					self.addOptionalParam(p, x)
					#self.addOptionalParam(p, (x,))





	# recusrive function to deal with class inheritance
	def addRecursiveInheritedClassRequirements(self, classname, reclevel):
		spaces = " " * reclevel
		#print spaces + "rec: " + classname
		i = self.classes[classname].get('inherits_reqs_from', None)
		#print 'i (all inherited classes) for class ' + classname
		#print i
		#print spaces + "enter rec, class = " + classname
		for parent in i:
			#print parent
			if parent != None:
				reqs = self.addRecursiveInheritedClassRequirements(parent, reclevel+1)
				#print spaces + "reqs: "
				#print reqs
				if reqs != None:
					for req in reqs:
						#print "ADDING REQ FROM RECURSIVE"
						self.addRequirementToClass(req[0], req[1], classname)
				#self.addParamsFromOneClassToAnother(parent, classname)

		#print spaces + "escape clause"
		reqs = self.classes[classname].get("reqs", None)
		return reqs



	# recusrive function to deal with class inheritance
	def addRecursiveInheritedClassParams(self, classname, reclevel):
		spaces = " " * reclevel
		#print spaces + "rec: " + classname
		i = self.classes[classname].get('inherits_params_from', ())
		#print 'i (all inherited classes) for class ' + classname
		#print i
		#print spaces + "enter rec, class = " + classname
		for parent in i:
			#print parent
			if parent != None:
				params = self.addRecursiveInheritedClassParams(parent, reclevel+1)
				#print spaces + "reqs: "
				#print reqs
				if params != None:
					for params in params:
						#print "ADDING REQ FROM RECURSIVE"
						self.addParamsFromOneClassToAnother(parent, classname)

		#print spaces + "escape clause"
		params = self.classes[classname].get("params", None)
		return params



	def addInheritedClassRequirements(self):
		#print '^^^^^^'
		for c in self.classes:
		#	print "addInheritedClassRequirements: " + c
			self.addRecursiveInheritedClassRequirements(c, 1)

	def addInheritedClassParams(self):
		#print '&&&&&&&&&'
		for c in self.classes:
		#	print "addInheritedClassParams: " + c
			self.addRecursiveInheritedClassParams(c, 1)


	def startElement(self, name, attrs):
		if (name == "magics") :
			return

		if (name == "class"):
			self.classname = attrs.get("name")
			if (self.debug) :
				print("class: " + self.classname)
			#if attrs.get("metview") :
			#	self.classname = attrs.get("metview")

			self.newClass(self.classname)

			doc_inherits = attrs.get("doc_inherits", None)
			self.inherits = attrs.get("inherits")
			if doc_inherits != 'no' and self.inherits != '' and self.inherits != None:
				self.classes[self.classname]["inherits_params_from"].add(self.inherits)
				self.classes[self.classname]["inherits_reqs_from"].add(self.inherits)
				fname = "%s/%s.xml" % (sys.argv[1], self.inherits)
				if not (fname in self.filehistory):  # don't open the same file twice
					try:
						file = open(fname, "r")
						if (self.debug):
							print("Opened (start class)" + fname)
						self.filehistory.append(fname)
						object = ObjectHandler()
						object.myoptions = []
						object.myrules = {}
						object.mydef = []
						object.filehistory = self.filehistory
						object.classes = self.classes
						object.setTopLevel(False)
						parser = make_parser()
						parser.setContentHandler(object)
						parser.parse(file)
						for s in object.mydef:
							self.mydef.append(s)
						for s in object.myrules:
							self.myrules[s]  = object.myrules[s]
						for s in object.myoptions:
							self.myoptions.append(s)
						self.classes = object.classes
					except:
						#print "Could not open file " + fname
						pass

		if (name == "parameter") or (name == "metview_parameter"):
			self.ignore = False
			self.param = attrs.get("name")
			if (self.debug) :
				print("  param: " + self.param)
			if (attrs.get("implemented") == 'no'):
				return
			if (attrs.get("metview") == 'no'):
				return
			if (attrs.get("visible") == 'no') or (attrs.get("visible") == 'off') or (attrs.get("visible") == 'false'):
				return
			if (attrs.get("inherit_parent_reqs") != 'no'):
				docclass = attrs.get("doc_class", None)
				if docclass != None:
					paramclass = docclass
				else:
					paramclass = self.classname
				self.addParameterToClass(self.param, paramclass)
			newtype = attrs.get("to")
			metview_type = attrs.get("metview_interface")
			if metview_type != None:
				newtype = metview_type
			if (newtype in self.types):
				f = self.types[newtype]
				self.newparam(self.param, f(self, attrs), self.default(attrs))
			else:
				self.last = self.last + self.options(attrs)

		if (name == "set"):
			setname = attrs.get("name")
			if setname != "" :
				valuestring = attrs.get("value")
				values = valuestring.split('/')
				for val in values:
					self.addOptionalParam(setname, ((self.param ,val),))
				#print('opt: ', self.optionalparams)
				#print  "%s %s = %s %s " % ('%if', self.param.upper(), attrs.get("value").upper(), '%then')
				#print "\t%s %s " % ('%unset', attrs.get("name").upper())
				pass

		if (name == "option"):
				if (self.debug) :
					print("    option: "+ attrs.get("name") + " - " + attrs.get("fortran"))
				if attrs.get("visible") != 'no'  :
					self.last = self.last + "\t\t%s; %s\n" % (attrs.get("fortran").upper(), attrs.get("fortran").upper())
					self.addRequirementToClass(self.param, attrs.get("fortran"), attrs.get("name"))
					#if attrs.get("doc_inherits") != 'no':
					#	self.classes[attrs.get("name")]["inherits_reqs_from"].add(self.classname)
					#	print "YClass " + attrs.get("name") + " inherits_reqs_from " + self.classname
					if attrs.get("docdive") != 'no' and  attrs.get("doc_inherits") != 'no' :
						if attrs.get("xmlfile") is not None:
							fname = "/%s.xml" % attrs.get("xmlfile")
						else:
							fname = "%s/%s.xml" % (sys.argv[1], attrs.get("name"))
						if not (fname in self.filehistory):  # don't open the same file twice
							try:
								file = open(fname, "r")
								self.filehistory.append(fname)
								if (self.debug):
									print("Opened (start option) " + fname)
								object = ObjectHandler()
								object.myoptions = []
								object.myrules = {}
								object.mydef = []
								object.unset = {}
								object.filehistory = self.filehistory
								object.classes = self.classes
								object.setTopLevel(False)
								parser = make_parser()
								parser.setContentHandler(object)
								parser.parse(file)
								s = object.mydef
								id = "%s_%s" % (attrs.get("name"), attrs.get("fortran"))
								id = "%s" % ( attrs.get("fortran"))
								self.myrules[id] = s
								self.myoptions.append(s)
								self.classes = object.classes
								#print "objlast: " + object.last
								#print "selflast: " + self.last
								#self.inheritedlast = self.inheritedlast + object.last
								#self.last = self.last + object.last
								#print "SL2: " + self.last
							except:
								#print "********EXCEPT 1!!!!!"
								#print "Could not open file " + fname
								#print "Therefore adding to myoptions: "
								#print(self.mydef)
								#self.myoptions.append(self.mydef)
								#self.mydef = []
								self.myoptions.append([])
								pass
					else:
						self.myoptions.append([])
						#self.myoptions.append(self.mydef)
						#self.mydef = []



	def endElement(self, name):

		#if (name == "magics") :
		#	return

		if self.ignore:
			return

		if ((name == "parameter" or name == "metview_parameter") and len(self.myoptions) != 0) :
		#if (name == "parameter") :
			#print(self.myoptions)
			self.last = self.last + "\t} = %s\n" % self.defparam
			self.newparam(self.param, self.last, self.defparam)
			if (self.debug) :
				print("  endparam: " + self.param)
				print("  endparam SL: \n" + self.last)
			self.last = ""
			for option in self.myoptions:
				for p in option:
					#print "    adding newparam from option: " + p[0]
					self.newparam(p[0], p[1], p[2])
			self.myoptions = []
			for rules in self.myrules:
				current = rules
				unsets = []
				for unset in self.myrules:
					if (unset != rules):
						for p in self.myrules[unset]:
							try:
								unsets.remove(p[0])
							except:
								pass
							unsets.append(p[0])

				for unset in self.myrules:
					if (unset == rules):
						for p in self.myrules[unset]:
							try:
								unsets.remove(p[0])
							except:
								continue

				if  len(unsets) != 0 :
						#print  "%s %s != %s %s " % ('%if', self.param.upper(), current.upper(), '%then')
						#for unset in unsets:
						#	print "\t%s %s " % ('%unset', unset.upper())
						pass

			self.myrules = {}


		if (name == "magics") :
			if self.toplevel:
				#print "SL (end class): " + self.last
				#print('opt: ', self.optionalparams)
				definition = open(sys.argv[3], "w")
				definition.write("PARAMSHARE ; ParamShare; PARAMSHARE\n");
				definition.write("{\n\tCOLOUR {\n\t\t%include MagicsColors.h\n\t}\n")
				definition.write("\tSTYLE {\n")
				definition.write("\t\tSOLID; SOLID\n")
				definition.write("\t\tDASH; DASH\n")
				definition.write("\t\tDOT; DOT\n")
				definition.write("\t\tCHAIN_DOT; CHAIN_DOT\n")
				definition.write("\t\tCHAIN_DASH; CHAIN_DASH\n")
				definition.write("\t}\n")
				definition.write("\tFONT {\n")
				definition.write("\t\tARIAL; ARIAL\n")
				definition.write("\t\tCOURIER; COURIER\n")
				definition.write("\t\tHELVETICA; HELVETICA\n")
				definition.write("\t\tTIMES; TIMES\n")
				definition.write("\t\tSERIF; SERIF\n")
				definition.write("\t\tSANSSERIF; SANSSERIF\n")
				definition.write("\t\tSYMBOL; SYMBOL\n")
				definition.write("\t}\n")
				definition.write("}\n\n")

				definition.write("%s; Magics; Automatically generated\n{\n" % sys.argv[4])
				for param in self.mydef:
					#print('PARAM START')
					#print(param[1])
					definition.write("%s" % param[1])
					definition.write("\n")
					#print('PARAM END')
				#definition.write(self.inheritedlast)
				definition.write("}\n")
				definition.close()

				#print "------------------------------------------"
				#for cl in self.classes.keys():
				#	print cl + " : "
				#	print self.classes[cl]
				#print self.classes

				#print "((((((((((((((((((((((((((((((((((((((("
				#print self.optionalparams['contour_shade_technique']

				#print "+++++++++++++++++++++++++++++++++++++++++++"
				#print self.optionalparams
				#print "********************************************"
				self.addInheritedClassParams()
				self.addInheritedClassRequirements()
				self.addClassDetailsToOptionalParameters()
				self.addRelatedOptionalRequirements()
				#print "+++++++++++++++++++++++++++++++++++++"
				#print self.optionalparams
				self.addRelatedOptionalRequirements2()
				#print "********************************************"
				#for o in self.optionalparams:
				#	PRint o
				#	print self.optionalparams[o]
				#print self.optionalparams

				# two different ways to generate the rules
				if False:
					rules = open(sys.argv[5], "w")
					for optparam in self.optionalparams.keys():
						rules.write("\n%if")
						first = True
						prevparam = ""
						#print "conditionzzzz"
						#print self.optionalparams[optparam]
						for condition in self.optionalparams[optparam]:
							if not first:
								if condition[0] == prevparam:
									rules.write(" %and")
								else:
									rules.write(" %or")
							first = False
							prevparam = condition[0]
							rules.write(" " + condition[0].upper() + " != " + condition[1].upper())
						rules.write(" %then")
						rules.write("\n\t%unset " + optparam.upper())
						rules.write("\n")
					rules.close()

				else:
					self.reorganiseOptionalParameters()
					#print "\n\n==============================\n"
					#print self.optionalparams2
					self.ensureOptionalParameters2DependenciesAreInRightOrder()
					#print "\n\n..............................\n"
					#print self.optionalparams3

					rules = open(sys.argv[5], "w")
					#for conditions in self.optionalparams2:
					for clause in self.optionalparams3:
						#print "CLAUSE"
						#print clause
						conditions = clause[0]
						#for conditions in clause[0]:
						#print "CONDS: "
						#print conditions
						lconditions = list(conditions)
						lconditions.sort()
						#print "LCONDS: "
						#print lconditions
						rules.write("\n%if")
						first = True
						prevparam = ""
						#print('conditionsssss :')
						#print conditions
						#print type(conditions)
						#print len(conditions)
						for condition in lconditions:
							#print "condition:"
							#print condition
							#print type(condition)
							#print len(condition)
							if not first:
								if condition[0] == prevparam:
									rules.write(" %and")
								else:
									rules.write(" %or")
							first = False
							prevparam = condition[0]
							#print('condition rules:')
							#print condition
							rules.write(" " + condition[0].upper() + " != " + condition[1].upper())
						rules.write(" %then")
#						for optparam in self.optionalparams2[conditions]:
						for optparam in clause[1]:
							rules.write("\n\t%unset " + optparam.upper())
						rules.write("\n")
					rules.close()


object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

datasource = open(sys.argv[1] +"/" + sys.argv[2], "r")
saxparser.parse(datasource)
