#!/usr/bin/env python

"""Run this script when you want to delete a branch results from the regression test summary. 
   It removes the server path:
   
   http://download.ecmwf.int/test-data/magics/regression_output/<BRANCH>/...
   
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

def removeBranch(branch):
    
    #remove branch from data file 
    ########### REMARK! ideally, this operation should be atomic! ########### 
    command= ' '.join(['scp','deploy@download-admin:test-data/magics/regression_output/data.json','.'])
    call(command,shell=True)
    if os.path.exists('data.json'):
        with open('data.json') as f: olddata= [tuple(x) for x in json.loads(f.read())]
    else:
        olddata= []
    newdata= [x for x in olddata if not x[0]==branch]
    with open('data.json','w') as f: f.write(json.dumps(newdata,sort_keys=True,indent=4,separators=(',', ': ')))
    upload('data.json','magics/regression_output')   
    ########### REMARK! ideally, this operation should be atomic! ########### 
    
    if len(newdata)<len(olddata):  
        #remove the branch files in data server
        command= 'ssh deploy@download-admin "rm -rf /home/deploy/test-data/magics/regression_output/%s"'%branch
        call(command,shell=True)
    
        #build the html file
        buildSummaryReport(newdata)

        n= len(olddata)-len(newdata)
        print n,'test results have been deleted for branch "%s"'%branch
    else:
        print 'No test results have been deleted for branch "%s". Is branch name correct?'% branch

#####################################################################
#####################################################################
#####################################################################

if __name__ == "__main__":
    
    cmd_parser = OptionParser(usage="usage: %prog <branch>", version='%prog : '+__version__, description = __doc__, prog = 'summary.py')

    print sys.argv
    
    _,positional = cmd_parser.parse_args()
    branch= None

    if positional:
        branch= positional.pop(0) 
        removeBranch(branch)
    else:
        print 'Please provide a branch name. Nothing done.'
