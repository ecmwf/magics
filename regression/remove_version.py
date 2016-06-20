#!/usr/bin/env python
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


"""Run this script when you want to delete a version results from the regression test summary. 
   It removes the version results from file:
   
   http://download.ecmwf.int/test-data/magics/regression_output/data.json
   
   and updates the summary output report:

   http://download.ecmwf.int/test-data/magics/regression_output/summary.html
"""

#Python standard library 
import sys
import os
import json
from subprocess import call
from optparse import OptionParser

#Project modules
from regression_util import upload,getOverview,buildOverview,getBranches,buildBranches,getVersions,buildVersions

__author__  = 'cgs,cgjd'
__date__    = '2014-01-10'
__version__ = '0.1'

REMARK='''
REMARK:
This script does NOT remove any file from download.ecmwf.int server.
This script does NOT modify any test result file to remove any reference to the version.
This script ONLY removes references to the version in "data.json" file, used to build "summary.html" file.
If you actually want to free space on download.ecmwf.int server, please use "remove_branch.py" script instead. 
'''

#####################################################################
#####################################################################
#####################################################################

def buildSummaryReport(data):
    with open('summary_template.html') as f: report= f.read()
    
    #Build Branch/Versions overview 
    oData= getOverview(data)
    overview,results= buildOverview(oData)
    report= report.replace('OVERVIEW',overview)
    report= report.replace('RESULTS',results)
    
    bData= getBranches(data)
    tab_branches= buildBranches(bData)
    report= report.replace('TAB_BRANCHES',tab_branches)
    
    vData= getVersions(data)
    tab_versions= buildVersions(vData)
    report= report.replace('TAB_VERSIONS',tab_versions)
    
    with open('summary.html','w') as f: f.write(report)
    upload('summary.html','magics/regression_output')


#####################################################################
#####################################################################
#####################################################################

def removeVersion(version):
    
    #remove branch from data file 
    ########### REMARK! ideally, this operation should be atomic! ########### 
    command= ' '.join(['scp','deploy@download-admin:test-data/magics/regression_output/data.json','.'])
    call(command,shell=True)
    if os.path.exists('data.json'):
        with open('data.json') as f: olddata= [tuple(x) for x in json.loads(f.read())]
    else:
        olddata= []
    newdata= [x for x in olddata if not x[3]==version]
    with open('data.json','w') as f: f.write(json.dumps(newdata,sort_keys=True,indent=4,separators=(',', ': ')))
    upload('data.json','magics/regression_output')   
    ########### REMARK! ideally, this operation should be atomic! ########### 
    
    if len(newdata)<len(olddata):  
        #build the html file
        buildSummaryReport(newdata)

        n= len(olddata)-len(newdata)
        print n,'test results have been deleted for version "%s" in summary.html'%version
        print REMARK
    else:
        print 'No test results have been deleted for version "%s". Is  name correct?'%version 

#####################################################################
#####################################################################
#####################################################################

if __name__ == "__main__":
    
    cmd_parser = OptionParser(usage="usage: %prog <version>", version='%prog : '+__version__, description = __doc__, prog = 'remove_version.py')

    print sys.argv
    
    _,positional = cmd_parser.parse_args()
    version= None

    if positional:
        version= positional.pop(0) 
        removeVersion(version)
    else:
        print 'Please provide a version name. Nothing done.'
