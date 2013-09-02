
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







