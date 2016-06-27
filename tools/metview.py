# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


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


