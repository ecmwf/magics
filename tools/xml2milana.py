import xmltodict

with open("../src/params/CoastPlotting.xml") as fd:
    definition = xmltodict.parse(fd)

print definition


for params in definition["magics"]["class"]:
    print params
