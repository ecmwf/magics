import json
import os

import jinja2


with open('../../share/magics/styles/palettes.json') as json_data:
    palettes = json.load(json_data)


import collections



results = {}
colours = {}
keywords = {}
levels =  {}
parameters = {}


def put(file, where, comment):
    cmd = "java -jar /home/graphics/cgs/atlassian-cli-2.5.0/lib/confluence-cli-2.5.0.jar --server http://ussoftware.ecmwf.int:8081/wiki --user cgs --password sjnclEC4 --action addAttachment --space MAGP --file '%s' --comment '%s' --title '%s' " % (file, comment, where)
    print cmd
    os.system(cmd)




for p in palettes :
    origin = palettes[p]["tags"]["origin"]
    print p, origin
    colour = sorted(palettes[p]["tags"]["colours"])
    
    for c in colour:
        colours[c] = c
    keyword = palettes[p]["tags"].get("keywords", [])
    for k in keyword:
        keywords[k] = k
    level = palettes[p]["tags"].get("n_levels", "0")
    print level
    levels[int(level)] = level
    params = palettes[p]["tags"]["parameter"]
    class_params = []
    for param in params:
        short = param.replace(" ", "")
        short = short.replace(":", "")
        short = short.replace("(", "")
        short = short.replace(")", "")
        parameters[param] = "p"+short
        class_params.append( "p"+short)
    print class_params
    where = results.get(origin, [])
    where.append( {
        "title" : p,
        "colours" : ' '.join(colour),
        "parameters" : ' '.join(class_params),
        "labels" : ', '.join(params),
        "level" : 'level_' + level,
        "nb_level" : level,
        "path" : "%s.png" % (p) 
        })
    results[origin] = where
    



template = os.path.join(os.path.dirname(__file__), "palette.html")
with open(template, "rt") as f:
    template = jinja2.Template(f.read())

fname = os.path.join(os.path.dirname(__file__), "table.html")
with open(fname, "wt") as f:
    f.write(template.render(title="Palettes",
                            results=collections.OrderedDict(sorted(results.items())),
                            colours=collections.OrderedDict(sorted(colours.items())),
                            parameters = collections.OrderedDict(sorted(parameters.items())),
                            keywords = keywords,
                            levels = collections.OrderedDict(sorted(levels.items()))
                            ))




put("table.html", "Predefined palettes in Magics",  "html" )
