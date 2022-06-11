import xmltodict

import ipywidgets as widgets

with open("../src/params/BoxPlotVisualiser.xml") as fd:
    l = fd.read()
    definition = xmltodict.parse(l)



print (definition["magics"]["class"])

def boolean(data):
    name = data["@name"]
    return 'widgets.ToggleButtons(description={}, options = ["on", "off"])'.format(name)

def colour(data):
    name = data["@name"]
    return 'widgets.DropDown(description={}, options = ["red", "blue", "green", "navy", "pink"])'.format(name)

def style(data):
    name = data["@name"]
    return 'widgets.DropDown(description={}, options = ["dash", "solid", "dot"])'.format(name)


def choice(data):
    name = data["@name"]
    values = data.get("@values", None)
    if values:
        return 'widgets.DropDown(description={}, options = {})'.format(name, values.split("/"))
    return ""


def ignore(data):
    name = data["@name"]

    return "ignore"

def float(data):
    name = data["@name"]
    return 'widgets.IntSlider(description={}'.format(name)

def int(data):
    name = data["@name"]
    return 'widgets.FloatSlider(description={}'.format(name)

helpers = {
    "bool" : boolean,
    "Colour" : colour,
    "float" : float,
    "int" : int,
    "LineStyle" : style,
    "string" : choice,
}


for e in definition["magics"]["class"]["parameter"]:
    print (e["@name"], e["@to"])
    to = e["@to"]
    print(helpers.get(to, ignore)(e))