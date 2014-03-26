
from subprocess import call


xmldir = "../src/params/"
mvdir = "../metview_files/"
icons = [ { "class" : "MTHERMOGRID", "file": "TephiGrid.xml" } ] 
icons = [ { "class" : "MCONT", "file": "Contour.xml" } ] 


for i in icons:
    c = i["class"]
    deffile = mvdir + c + "Def" 
    rulesfile = mvdir + c + "Rules" 
    call(["python", "xml2mv.py", xmldir + i["file"], deffile, c, rulesfile])


