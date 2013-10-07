import sys
from subprocess import call

def upload(filename,destination):
    destination = "deploy@download-admin:test-data/%s"%destination
    e= call(["scp",filename, destination])
    if not e==0:
        sys.stderr.write("ERROR uploading the file %s into %s"%(filename,destination))
