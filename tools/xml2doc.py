
from xml.sax.handler import ContentHandler
from xml.sax import make_parser
import sys





class ObjectHandler(ContentHandler):
	name = ""
	defparam = ""
	params = []
	current = {}
	doc = ""


	def default(self, attrs):
		val = attrs.get("default");
		if (val == "-int_MAX"):
			return "-1.0E21"
		if (val == "int_MAX"):
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
			return "'(automatic)'"
		return val

	def characters(self, data):
		pass
	
	def toggle(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		s = s + "\t\ton; on\n"
		s = s + "\t\toff; off\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s


	def any(self, attrs):
		s = "DOCUMENATION " 
		s = s + "\t} = %s" % self.default(attrs)
		return s


	def number(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		s = s + "\t\t*\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s

	def listofnumbers(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		s = s + "\t\t*\n"
		s = s + "\t\t/\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s

	def listofstrings(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		s = s + "\t\t@\n"
		s = s + "\t\t/\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s

	def colour(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		s = s + "\t\tred; red\n"
		s = s + "\t\t@\n"
		s = s + "\t} = %s\n" % self.default(attrs)
		return s

	def linestyle(self, attrs):
		s = "here we found a line " 
		s = s + "\t} DEFAULT %s\n" % self.default(attrs)
		return s

	def options(self, attrs):
		s = "\t%s\n\t{\n" % attrs.get("name")
		self.defparam = self.default(attrs)
		return s

	def normal(self, param):
		s =  "<tr>\n"
		s = s + "  <td><strong>%s</strong></td><td>%s</td>\n" % (param["name"], param["from"])
		s = s + "  <td>%s</td><td>%s</td>\n" % (param["doc"], param["default"])
		s = s + "</tr>\n"
		return s

	def header(self, param):
		s =  "<tr style='background-color: rgb(204, 204, 255);'>\n"
		s = s + "  <td colspan='4'><strong>%s (%s)</strong></td>\n" % (param["name"], param["value"])
		s = s + "</tr>\n"
		return s

	
	types = { "normal" : normal,
			  "header" : header}

	def newparam(self, param, value, default):
		for p in self.params:
			if p["name"] == param:
				selfcurrent = p
				return
		self.current = {"name" : param,  "type": "normal", "from" : value, "default" : default, "doc":""} 
		self.params.append(self.current)

	def startElement(self, name, attrs):
		if (name == "magics") :
			return

		if (name == "class"):
			self.name = attrs.get("name")
			self.inherits = attrs.get("inherits")
			if self.inherits != '':
				try:
					file = open("xml/%s.xml" % attrs.get("inherits"), "r")
					print " inherits ->%s" % self.inherits
					object = ObjectHandler()
					parser = make_parser()
					object.params = []
					parser.setContentHandler(object)
					parser.parse(file)
					self.params.append(s)
					for s in object.params:
						self.params.append(s)
						
				except:
					pass

		if (name == "documentation"):
			self.doc = ""
		if (name == "parameter"):
			self.param = attrs.get("name")
			print ("name %s"%self.param)
			if (attrs.get("implemented") == 'no'):
				return
			type = attrs.get("to")
			fromtype = attrs.get("from")
			deftype = attrs.get("default")
			self.newparam(self.param, fromtype, deftype)
		if (name == "option"):
				object = ObjectHandler()
				object.params = []
				object.doc = ""
				if (attrs.get("docdive") == 'no'):
					return
				parser = make_parser()
				parser.setContentHandler(object)
				file = open("xml/%s.xml" % attrs.get("name"), "r")
				value = attrs.get("fortran");
				if (attrs.get("fortran") != attrs.get("xml")) :
					value = "%s/%s"%(attrs.get("fortran"), attrs.get("xml"))
					
				p = {"name" : self.param,  "value" : value , "type" : "header"}
				self.params.append(p)
				parser.parse(file)
				for s in object.params:
					self.params.append(s)

	def characters(self, doc):
		self.doc = self.doc + doc

	def endElement(self, name):
		if (name == "magics") :
			return

			
		if (name == "documentation") :
			self.current["doc"] = self.doc
			pass
			
		if (name == "class") :
			filename = "xml/%s.html" % self.name
			# <tr style="background-color: rgb(204, 204, 255);"> 
			definition = open(filename, "w")
			definition.write( "<h3>%s parameters</h3>\n" % self.name)
			definition.write( '<table cellspacing="1" cellpadding="1" border="1" style="width: 100%;">\n')
			definition.write( '<tbody><font face="arial,sans-serif">\n')
			definition.write( "<tr>\n")
			definition.write( "  <td><strong>Parameter</strong></td><td><strong>Type</strong></td>\n")
			definition.write( "  <td><strong>Documentation</strong></td><td><strong>Default</strong></td>\n")
			definition.write( "</tr>\n")
			for param in self.params:	
				s = self.types[param["type"]](self, param)
				definition.write(s)
			definition.write( "</font></tbody>\n")
			definition.write( '</table>\n')


object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

datasource = open(sys.argv[1], "r")
saxparser.parse(datasource)
