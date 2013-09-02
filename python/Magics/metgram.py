import simplejson
import tempfile
import os


magics = {}
magics["info"] = "on"

magics["format"] = "a4"
magics["page"] = []
magics["orientation"] = "portrait"
magics["version"] = "3.0"
last = {}

ps = {}
ps["format"] = "ps"

drivers=[ps]
magics["drivers"] = drivers


definition = []
definition.append( {
	"id" : "haxis",
	"class" : "horizontal_axis",
	"axis_type" : "date",
	"axis_tick_label": "off",
    "axis_tick_colour": "kelly_green" ,
    "axis_line_colour":"kelly_green" ,
    "axis_grid":"on",
    "axis_grid_line_style":"dash",
    "axis_grid_colour":"kelly_green"
})

definition.append({
	"class" : "horizontal_axis",
    "id" : "haxis_last",
    "axis_type" : "date",
    "axis_tick_label": "on",
    "axis_tick_colour": "kelly_green" ,
    "axis_line_colour":"kelly_green" ,
    "axis_grid":"on",
    "axis_grid_line_style":"dash",
    "axis_grid_colour":"kelly_green",
    "axis_date_type":"days",
    "axis_minor_tick":"on",
    "axis_days_label":"both",
    "axis_days_label_colour":"navy",
    "axis_days_label_height":"0.35",
    "axis_months_label_colour":"navy",
    "axis_months_label_height":"0.3",
    "axis_years_label_colour":"navy",
    "axis_years_label_height":"0.3"
})



definition.append({
	"class": "vertical_axis",
	"id" : "vaxis",
	"axis_line":"on", 
    "axis_grid":"on", 
    "axis_line_colour":"kelly_green" ,
    "axis_tick_label_colour":"kelly_green",
    "axis_label_font":"sansserif",
    "axis_grid_line_style":"dash",
    "axis_grid_colour":"kelly_green",
    "axis_tick_colour":"kelly_green"
})
magics["definition"] = definition
pages = []

header = {  "height" : "12%" }
def page():
	page = { "height" : "15%",
	"map" : {
		"cartesian":
        {
          "x_date": { "automatic":"on" },
          "y_regular": { "automatic":"on" }
        },
		"text" : {
			"left": "0%",
			"height": "10%",
			"bottom": "90%",
			"text_justification": "left",
			"text_font_size": "0.4",
			"text_font_style": "bold",
			"text_font": "sansserif",
			"text_colour": "navy",
			"text_border": "off"
		},
		"horizontal_axis": { "use_id" : "haxis" },
		"vertical_axis": { "use_id" : "vaxis" }

		
	}

	}
	return page

class humidity(object):
	
	def execute(self):
		humi =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"13003",
                "epsbufr_parameter_title":"850 hPa Relative Humidity  (%)",
                "use_id":"station"
            },
            "metgraph": { "curve": { } }
        }

		map = page()
		humi["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map["map"]["cartesian"] = {
          "x_date": { "automatic":"on" },
          "y_regular": { "y_min":"0", "y_max":"100" }
		  }
		map["map"]["plot"] = humi
		magics["page"].append(map)
		return map

class msl(object):
	
	def execute(self):
		msl =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"10051",
                "epsbufr_parameter_title":"MSL Pressure (hPa)",
				"epsbufr_parameter_scaling_factor": "0.01",
                "use_id":"station"
            },
            "metgraph": { "curve": { } }
        }

		map = page()
		msl["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map["map"]["plot"] = msl
		magics["page"].append(map)
		return map


class cloud(object):
	
	def execute(self):
		cloud =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"20010",
                "epsbufr_parameter_title":"Cloud Amount (%)",
                "use_id":"station"
            },
            "metgraph": { "bar": { } }
        }

		map = page()
		cloud["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map["map"]["cartesian"] = {
          "x_date": { "automatic":"on" },
          "y_regular": { "y_min":"0", "y_max":"100" }
		  }
		map["map"]["plot"] = cloud
		magics["page"].append(map)
		return map

class precip(object):
	
	def execute(self):
		precip =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"13011",
                "epsbufr_parameter_title":"Precipitation",
                "epsbufr_accumulated_parameter":"on",
                "use_id":"station"
            },
            "metgraph": { "bar": { } }
        }
		precip["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map = page()
		map["map"]["plot"] = precip
		magics["page"].append(map)
		return map

class wind(object):
	
	def execute(self):
		wind =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"11003",
                "epsbufr_parameter_2_descriptor":"11004",
                "epsbufr_parameter_title":"10m Wind (m/s)",
                "use_id":"station"
            },
            "metgraph": { "flags": { } }
        }
		wind["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map = page()
		map["map"]["plot"] = wind
		map["height"] = "10%"
		map["map"]["height"] = "80%"
		map["map"]["text"]["bottom"] = "70%"
		map["map"]["vertical_axis"]["axis_tick_label"] = "off"
		map["map"]["vertical_axis"]["axis_grid"] = "off"
		map["map"]["vertical_axis"]["axis_tick"] = "off"
		map["map"]["cartesian"] = {
          "x_date": { "automatic":"on" },
          "y_regular": { "y_min":"-1", "y_max":"1" }
		}
		magics["page"].append(map)
		return map

class tempe(object):
	
	def execute(self):
		t850 =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"12001",
                "epsbufr_parameter_title":"Temperature (C)",
				"epsbufr_parameter_offset_factor": "-273.15",
                "use_id":"station"
            },
            "metgraph": { 
				"metgram_plot_style":"curve",
				"metgram_curve_colour":"blue"
				}
        }
		t850["epsbufr"]["epsbufr_information"] = magics["info"]
		tempe =  {
            "epsbufr":
            {
                "epsbufr_parameter_descriptor":"12004",
                "epsbufr_short_title":"off",
				"epsbufr_information":"off",
				"epsbufr_parameter_offset_factor": "-273.15",
                "use_id":"station"
            },
            "metgraph": { 
				"metgram_plot_style":"curve",
				"metgram_curve_colour":"red"
				}
        }
		t850["epsbufr"]["epsbufr_information"] = magics["info"]
		magics["info"] = "off"
		map = page()
		map["map"]["plot"] = [ t850, tempe]
		magics["page"].append(map)
		return map





class station(object):
	def __init__(self, args):	
		self.definition = args
		self.definition["id"]= "station"
		self.definition["class"]= "epsbufr"
	def execute(self):
		magics["definition"].append(self.definition)
		return page()

	
class ps(object):
	def __init__(self, args):	
		self.definition = args
		self.definition["format"]= "ps"
	def execute(self):
		if (magics["drivers"] == None) :
			magics["drivers"]=[]
		magics["drivers"].append(self.definition)
		return page()

	




def metgram(*args):

	magics["page"].append(header)
	nb = len(args)
	i = 0
	haxis="haxis"
	for n in args:
		map = n.execute()
		i += 1
		if (i == nb) :
			haxis="haxis_last"
		map["map"]["horizontal_axis"]["use_id"] = haxis
		
			
	s = simplejson.dumps(magics, indent=4 * ' ')
	f = tempfile.NamedTemporaryFile()
	f.write(s)
	f.flush()

	cmd = "magjson %s" % (f.name)

	error = os.system(cmd)
	if (error != 0):
		print "Error found"
	f.close
	

cloud = cloud()
humidity = humidity()
precip = precip()
tempe = tempe()
msl = msl()
wind = wind()

