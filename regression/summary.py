#!/usr/bin/env python

"""The script runs should be called after the regression tests have run. 
   It uploads the output reports (HTML pages) to the server path:
   
   http://download.ecmwf.int/test-data/magics/regression_output/<BRANCH>/<TEST>/<TIMESTAMP>
   
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
from regression_util import upload,maxResult,getOverview,buildOverview,getBranches,buildBranches,getVersions,buildVersions

__author__  = 'cgs,cgjd'
__date__    = '2013-11-01'
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

def summary(base_dir):

    ##################################################
    # "Time of testing" is shared for all tests
    # collected in the summary
    #time= datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
    #################################################

    #read test sub-directories
    tests= [f for f in os.listdir(base_dir) if os.path.isdir(base_dir+'/'+f) and os.path.isfile(base_dir+'/'+f+'/'+f+'.par') ]
    print tests

    #reset temporal directory
    tmpdir= base_dir+'/.temporal'
    if os.path.exists(tmpdir): call(['rm','-rf',tmpdir])
    call(['mkdir',tmpdir])
    
    data= []
    #load report files into "download.ecmwf.int/test-data/magics/regression_output/BRANCH/TEST/TIMESTAMP"
    for test in tests:
        parfile= base_dir+'/'+test+'/'+test+'.par'
        with open(parfile) as f: params= json.loads(f.read())

        #create local hierarchy
        tmptestdir= tmpdir+'/'+params['branch_name']+'/'+test+'/'+params['time']
        if not os.path.exists(tmptestdir): call(['mkdir','-p',tmptestdir])
        
        #copy test directory to temporal hierarchy
        testdir= base_dir+'/'+test
        command= ' '.join(['cp',testdir+'/*',tmptestdir])
        call(command,shell=True)
        
        #get the test group from input directory
        group=''
        if params.has_key('input_dir'): group= params['input_dir'].split('/')[-1]+'/'
        
        #keep relevant parameters to database: branch,group+test,time,version,result
        for ver in  params['versions']:
            p_branch= params['branch_name']
            p_test= group+test
            p_time= params['time']
            p_version= ver
            p_mess= params['exit_message']
            p_diff= params['max_difper']
            p_thre= params['threshold'] 
            if p_mess.find('FAILED')>=0:
                p_result= 'Error'
            else:
                try:
                    p_result= maxResult(params['result'][ver].values())
                except:
                    p_result= 'Error'
            data.append((p_branch,p_test,p_time,p_version,p_result,p_mess,p_diff,p_thre))
        
    #upload all tests hierarchy to the server
    upload(tmpdir+'/*','magics/regression_output')

    #clean temporal directory
    if os.path.exists(tmpdir): call(['rm','-rf',tmpdir])
    
    
    #update data file 
    ########### REMARK! ideally, this operation should be atomic! ########### 
    command= ' '.join(['scp','deploy@download-admin:test-data/magics/regression_output/data.json','.'])
    call(command,shell=True)
    if os.path.exists('data.json'):
        with open('data.json') as f: olddata= [tuple(x) for x in json.loads(f.read())]
    else:
        olddata= []
    newdata= list( set(olddata) | set(data) )
    with open('data.json','w') as f: f.write(json.dumps(newdata,sort_keys=True,indent=4,separators=(',', ': ')))
    upload('data.json','magics/regression_output')   
    ########### REMARK! ideally, this operation should be atomic! ########### 
    
    #build the html file
    buildSummaryReport(newdata)
    

#####################################################################
#####################################################################
#####################################################################

if __name__ == "__main__":
    
    cmd_parser = OptionParser(usage="usage: %prog <input-reports-dir>", version='%prog : '+__version__, description = __doc__, prog = 'summary.py')

    print sys.argv
    
    _,positional = cmd_parser.parse_args()
    input_dir= None

    if positional: input_dir= positional.pop(0)     
    
    summary(input_dir)
