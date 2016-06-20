# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


import json
import os

def put(file, where, comment):
	cmd = "java -jar /home/graphics/cgs/atlassian-cli-2.5.0/lib/confluence-cli-2.5.0.jar --server http://ussoftware.ecmwf.int:8081/wiki --user cgs --password loulou --action addAttachment --space MAGP --file '%s' --comment '%s' --title '%s' --replace" % (file, comment, where)
	print cmd
	os.system(cmd)

def load(file, where, data, entry):
  

	#build zip file with all source and data files
	cmd= "zip %s.zip %s.py %s.f90 %s.mv4"%(file,file,file,file)
	for e in data: cmd+= " "+e 
	os.system(cmd)
	
	#upload the files
	extensions = [".py",".png",".f90",".html",".mv4",".zip"]
	for ex in extensions:
	  put(file+ex, where, file)
	  os.system('rm -f '+file+ex)
	
	#restore original python script name
	os.system('mv -f '+file+'.bak '+file+'.py')

def prepare(name, list, page):
	
	examples = { "examples" : []}
	for entry in list:
		cmd = "python " + entry["file"] + ".py"
		print cmd
		os.system(cmd)
		load(entry["file"], page,entry["data"],entry)
		ex = {"name" : entry["file"],"title" : entry["title"]}
		examples["examples"].append(ex)
	file = open(name, "w")
	print >> file, json.dumps(examples, indent=1)







