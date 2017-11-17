# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from . import macro

def substitute(default, user):
    out = default
    if user != None:
        for key in user:
            out[key] = user[key]
    return out


def geoplot(data, contour=None, output=None, background=None, foreground=None, area=None, title=[]):

    default = {
       "area" : {},
       "contour" : {},
       "background" : { "map_coastline_sea_shade"         : 'on',
                        "map_coastline_sea_shade_colour"  : 'rgb(81,81,81)',
                        "map_coastline_land_shade"        : 'on',
                        "map_label"                       : 'off',
                        "map_coastline_land_shade_colour" : 'rgb(113,113,113)',
                    },
        "foreground" : { "map_coastline_thickness" :  2,
                         "map_grid_line_style"     :  'dash',
                         "map_grid_colour"         :  'rgb(143,166,183)',
                         "map_label"               :  'off',
                         "map_coastline_colour"    :  'rgb(143,166,183)'
                    },

    }

    background = macro.mcoast( substitute(default["background"], background) )
    foreground = macro.mcoast( substitute(default["foreground"], foreground) )
    projection = macro.mmap(   substitute(default["area"], area) )
    contour    = macro.mcont(  substitute(default["contour"], contour) )

    #Define the title
    title = macro.mtext(
                  text_lines = title,
                  text_font_size = 0.8,
                  text_justification = "left"
                )
    if output == None :
      return macro.plot(projection, background, data, contour, foreground, title)

    return macro.plot(output, projection, background, data, contour, foreground, title)

def xyplot(data, contour=None, output=None):

    default = {
       "contour" : {}
    }

    #Setting the cartesian view
    projection = macro.mmap(subpage_map_projection = 'cartesian',
                subpage_x_automatic = 'on',
                subpage_y_automatic = 'on',
                     )
    #Vertical axis
    vertical = macro.maxis(axis_orientation = "vertical",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")

    #Horizontal axis
    horizontal = macro.maxis(axis_orientation = "horizontal",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")


    #Define the graph
    contour = macro.mcont( substitute(default["contour"], contour))

    #Define the title
    title = macro.mtext(
                  text_font_size = 0.8,
                  text_justification = "left"
                )
    if output == None:
      return macro.plot(output, projection, vertical, horizontal, data, contour, title)

    return macro.plot(output, projection, vertical, horizontal, data, contour, title)

def graph(x,y, title="", graph = None, colour = "ecmwf_blue") :

    default = {
     "graph" : { "graph_line_colour"  : "ecmwf_blue",
                    "graph_line_thickness" : 2,
     }
    }

    x[0] = x[0]*1.
    y[0] = y[0]*1.

    #Setting the cartesian view
    projection = macro.mmap(subpage_map_projection = 'cartesian',
                subpage_x_automatic = 'on',
                subpage_y_automatic = 'on',
                     )
    #Vertical axis
    vertical = macro.maxis(axis_orientation = "vertical",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")

    #Horizontal axis
    horizontal = macro.maxis(axis_orientation = "horizontal",
                     axis_grid =  "on",
                     axis_grid_colour = "grey",
                     axis_grid_thickness = 1,
                     axis_grid_line_style = "dot")

    #define the input data
    input = macro.minput(input_x_values =  x,
                input_y_values =  y)

    #Define the graph
    graph = macro.mgraph( substitute(default["graph"], graph)
                )
    #Define the title
    title = macro.mtext(text_lines = [title],
                  text_font_size = 0.8,
                  text_justification = "left"
                )
    return macro.plot(projection, vertical, horizontal, input, graph, title)


colour = "ecmwf_blue"
font_size = 0.35
defaults = {
   "epswind" :  {
    "projection" : {
            "subpage_map_projection" : 'cartesian',
            "subpage_x_axis_type" : 'date',
            "subpage_x_automatic" : 'on',
            "subpage_y_axis_type" : 'regular',
            "subpage_y_automatic" : 'off',
            "subpage_y_max" : 43200.,
            "subpage_y_min" : -43200.,
       },
         "vertical_axis" :  {
              "axis_orientation" : "vertical",
              "axis_grid" : "on",
              "axis_grid_colour" : "navy",
              "axis_grid_line_style" :"dash",
              "axis_grid_reference_level" :  0.,
              "axis_grid_reference_thickness" : 1,
              "axis_line" :  "on",
              "axis_line_colour" : "navy",
              "axis_tick": "off",
              "axis_tick_label": "off"

              },
      "epswind" : {
              "eps_rose_wind_border_colour": "Rgb(0.5000, 0.5000, 0.5000)",
              "eps_rose_wind_colour": "greenish_blue"
              }
      },

   "epswave" :  {
    "projection" : {
            "subpage_map_projection" : 'cartesian',
            "subpage_x_axis_type" : 'date',
            "subpage_x_automatic" : 'on',
            "subpage_y_axis_type" : 'regular',
            "subpage_y_automatic" : 'off',
            "subpage_y_max" : 43200.,
            "subpage_y_min" : -43200.,
       },
         "vertical_axis" :  {
              "axis_orientation" : "vertical",
              "axis_grid" : "on",
              "axis_grid_colour" : "navy",
              "axis_grid_line_style" :"dash",
              "axis_grid_reference_level" :  0.,
              "axis_grid_reference_thickness" : 1,
              "axis_line" :  "on",
              "axis_line_colour" : "navy",
              "axis_tick": "off",
              "axis_tick_label": "off"

              },

         "epswave": {
                        "eps_rose_wind_border_colour": "Rgb(0.5000, 0.5000, 0.5000)",
                        "eps_rose_wind_colour": "RGB(0.925,0.609,0.953)",
                        "eps_rose_wind_convention": "oceanographic"
                      },
  },
  "eps" :  {
       "projection" : {
            "subpage_map_projection" : 'cartesian',
            "subpage_x_axis_type" : 'date',
            "subpage_x_automatic" : 'on',
            "subpage_y_axis_type" : 'regular',
              "subpage_y_automatic" : 'on',
       },
         "vertical_axis" :  {
              "axis_orientation" : "vertical",
              "axis_grid" : "on",
              "axis_grid_colour" : "navy",
              "axis_grid_line_style" :"dash",
              "axis_grid_reference_level" :  0.,
              "axis_grid_reference_thickness" : 1,
              "axis_line" :  "on",
              "axis_line_colour" : "navy",
              "axis_tick_colour" : "navy",
              "axis_tick_label_colour" : "navy",
              "axis_tick_label_height":  font_size
              },
          "horizontal_axis" :  {
              "axis_orientation" : "horizontal",
              "axis_date_type" : "days",
              "axis_days_label" : "both",
              "axis_days_label_colour" :  "navy",
              "axis_days_label_height" : font_size,
              "axis_grid" :  "on",
              "axis_grid_colour" : "navy",
              "axis_grid_line_style" : "dash",
              "axis_line_colour" : "navy",
              "axis_minor_tick" : "off",
              "axis_minor_tick_colour" : "navy",
              "axis_months_label" : "off",
              "axis_tick_colour" : "navy",
              "axis_type" :  "date",
              "axis_years_label" : "off"
              },
          "epsgraph" : {
              "eps_box_border_thickness" : 2,
              "eps_box_width" : 1.5,
              "eps_box_colour" : colour,
              "eps_font_colour" :"navy",
              "eps_legend_font_size" :  font_size,
              "eps_grey_legend" : "off",
              "legend" :'off'
          },

          "epsclim" : {
             "eps_shade_colour": colour,
             "eps_shade_line_thickness": 4,
          }
      }
    }


def epswave(parameter, input, **args):
    actions = []

    projection = macro.mmap( substitute(defaults["epswave"]["projection"], args.get("projection", None)) )

    # define horizontal axis
    horizontal = macro.maxis(substitute(defaults["eps"]["horizontal_axis"], args.get("horizontal_axis", None)))
    vertical = macro.maxis(substitute(defaults["epswave"]["vertical_axis"], args.get("vertical_axis", None)))


    data = macro.mwrepjson(
                            wrepjson_family =  "eps",
                            wrepjson_keyword =  "eps",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_parameter_information =  args.get("title", parameter),
                            wrepjson_parameter_scaling_factor = 1.,
                        )

    wave = macro.mepswave(substitute(defaults["epswave"]["epswave"], args.get("epswave", None)) )


    actions.append(projection)
    actions.append(vertical)
    actions.append(horizontal)

    actions.append(data)
    actions.append(wave)


    text = macro.mtext(
                    text_colour =  "navy",
                    text_font_size = font_size*2,
                    text_justification =  "left",
                    text_lines =  ["ENS Meteogram",
                    "<json_info key='station_name'/><json_info key='location'/><json_info key='grid_point'/><json_info key='height'/>",
                    "<json_info key='product_info'/><json_info key='date'/>",
                    "<font size='0.5' colour='white'>.</font>",
                    "<json_info key='parameter_info'/>",]

                )

    actions.append(text)

    if "output" in args != "" :
      #Setting of the output file name
      png = macro.output(output_formats = ['png'],
      output_name_first_page_number = "off",
      output_name = args["output"],
      super_page_y_length = 10.,
      subpage_y_length = 5.,
      subpage_y_position = 1.5)

      return macro._plot(
        png,
            actions
      )

    return macro.plot(
            actions
    )

def epswind(parameter, input, **args):
    actions = []

    projection = macro.mmap( substitute(defaults["epswind"]["projection"], args.get("projection", None)) )

    # define horizontal axis
    horizontal = macro.maxis(substitute(defaults["eps"]["horizontal_axis"], args.get("horizontal_axis", None)))
    vertical = macro.maxis(substitute(defaults["epswind"]["vertical_axis"], args.get("vertical_axis", None)))


    data = macro.mwrepjson(
                            wrepjson_family =  "eps",
                            wrepjson_keyword =  "eps",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_parameter_information =  args.get("title", parameter),
                            wrepjson_parameter_scaling_factor = 1.,
                        )

    wave = macro.mepswind(substitute(defaults["epswind"]["epswind"], args.get("epswind", None)) )


    actions.append(projection)
    actions.append(vertical)
    actions.append(horizontal)

    actions.append(data)
    actions.append(wave)


    text = macro.mtext(
                    text_colour =  "navy",
                    text_font_size = font_size*2,
                    text_justification =  "left",
                    text_lines =  ["ENS Meteogram",
                    "<json_info key='station_name'/><json_info key='location'/><json_info key='grid_point'/><json_info key='height'/>",
                    "<json_info key='product_info'/><json_info key='date'/>",
                    "<font size='0.5' colour='white'>.</font>",
                    "<json_info key='parameter_info'/>",]

                )

    actions.append(text)

    if "output" in args != "" :
      #Setting of the output file name
      png = macro.output(output_formats = ['png'],
      output_name_first_page_number = "off",
      output_name = args["output"],
      super_page_y_length = 10.,
      subpage_y_length = 5.,
      subpage_y_position = 1.5)



      return macro._plot(
        png,
              actions
        )


    return macro.plot(
            actions
    )

def epsgraph(parameter, input, **args):

    actions = []

    projection = macro.mmap( substitute(defaults["eps"]["projection"], args.get("projection", None)) )

    # define horizontal axis
    horizontal = macro.maxis(substitute(defaults["eps"]["horizontal_axis"], args.get("horizontal_axis", None)))
    vertical = macro.maxis(substitute(defaults["eps"]["vertical_axis"], args.get("vertical_axis", None)))


    data = macro.mwrepjson(
                            wrepjson_family =  "eps",
                            wrepjson_keyword =  "eps",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_missing_value = args.get("missing", 9999.),
                            wrepjson_parameter_information =  args.get("title", parameter),
                            wrepjson_parameter_scaling_factor = args.get("scaling", 1.),
                            wrepjson_parameter_offset_factor = args.get("offset", 0.),
                        )

    graph = macro.mepsgraph(substitute(defaults["eps"]["epsgraph"], args.get("epsgraph", None)) )
    actions.append(projection)
    actions.append(vertical)
    actions.append(horizontal)

    print ("Create Axis")

    if "climate" in args  :
      clim = macro.mwrepjson(
                            wrepjson_family =  "eps",
                            wrepjson_keyword =  "clim",
                            wrepjson_input_filename = input,
                            wrepjson_parameter = parameter,
                            wrepjson_parameter_scaling_factor = args.get("scaling", 1.),
                            wrepjson_parameter_offset_factor = args.get("offset", 0.),
                            wrepjson_ignore_keys = ["100"],
                            wrepjson_missing_value = args.get("missing", 9999.),
                            wrepjson_parameter_information = "none",
                            wrepjson_position_information = "off"
                        )
      shade = macro.mepsshading(substitute(defaults["eps"]["epsclim"], args.get("epsclim", None)) )
      actions.append(clim)
      actions.append(shade)

    actions.append(data)
    actions.append(graph)

    text = macro.mtext(
                    text_colour =  "navy",
                    text_font_size = font_size*2,
                    text_justification =  "left",
                    text_lines =  ["ENS Meteogram",
                    "<json_info key='station_name'/><json_info key='location'/><json_info key='grid_point'/><json_info key='height'/>",
                    "<json_info key='product_info'/><json_info key='date'/>",
                    "<font size='0.5' colour='white'>.</font>",
                    "<json_info key='parameter_info'/>",]
                  )

    actions.append(text)

    print (actions)
    print (args)
    '''
    if "output" in args != "" :
      #Setting of the output file name
      png = macro.output(output_formats = ['png'],
        output_name_first_page_number = "off",
        output_name = args["output"],
        super_page_y_length = 10.,
        subpage_y_length = 5.,
        subpage_y_position = 1.,
      )
      
      return macro._plot(
            png,
            actions
      )
    '''
    print (actions)
    print ("Sent to plot")
    return macro.plot(
            actions
      )




def epsclimgram(**kw):

  args = {"clim" :True }
  args.update(kw)

  return epsgraph(**args)

params = {
    "2t": {
        "scaling": 1.0,
        "offset": -273.0,
        "method": epsgraph,
        "title": "2-metre temperature"
    },
    "mcc": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Medium cloud cover"
    },
     "light-index": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Light index"
    },
    "10fg6": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "10 metre wind gust in the last 6 hours"
    },
    "tp": {
        "scaling": 1000.,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Total precipitation (last 6 hours) (m)"
    },
    "tcc": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Total cloud cover"
    },
    "hcc": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "High cloud cover"
    },
    "ws": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Wind speed (m/s)"
    },
    "lcc": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Low cloud cover"
    },
    "sf": {
        "scaling": 1000.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Snowfall (last 6 hours)"
    },
    "swh": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Significant wave height"
    },
    "mwp": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Mean wave period"
    },
    "dwi": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epswind,
        "title": "10 metre wind direction"
    },
    "mwd": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epswave,
        "title": "Mean wave direction"
    },
    "ws24": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Daily mean of 10m wind speed (m/s)"
    },
    "tcc24": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Daily mean of total cloud cover"
    },
    "mn2t24": {
        "scaling": 1.0,
        "offset": -273.15,
        "method": epsgraph,
        "title": "2 metre min temperature (Daily)"
    },
    "tp24": {
        "scaling": 1000.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": "Total precipitation (mm/24hr)"
    },
    "mx2t24": {
        "scaling": 1.,
        "offset": -273.15,
        "method": epsgraph,
        "title": "2 metre max. temperature (Daily)"
    },
    "dwi24": {
        "scaling": 1.0,
        "offset": 0.0,
        "method": epswind,
        "title": "Daily distribution of 10m Wind Direction"
    }

}


params_15days = {   "mx2t24":"2 metre max. temperature (Daily)",
                    "mn2t24":"2 metre min temperature (Daily)",
                    "tcc24":"Daily mean of total cloud cover",
                    "tp24":"Total precipitation (mm/24hr)",
                    "dwi24":"Daily distribution of 10m Wind Direction",
                    "ws24":"Daily mean of 10m wind speed (m/s)",
                }




def epsgram(parameter, input, **args):
  eps = params.get(parameter, { "scaling": 1.0,
        "offset": 0.0,
        "method": epsgraph,
        "title": parameter} )

  args["scaling"] = eps["scaling"]
  args["offset"] = eps["offset"]
  args["title"] = eps["title"]

 return eps["method"](parameter, input, **args)
