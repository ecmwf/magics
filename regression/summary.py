#!/usr/bin/env python

"""The script runs should be called after the regresion tests have run. 
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
import datetime

#Project modules
from regression_util import upload,maxResult,resultLabelStyle,writeTab

__author__  = 'cgs,cgjd'
__date__    = '2013-11-01'
__version__ = '0.1'

#####################################################################
#####################################################################
#####################################################################


def getDateDistance(timestamp,colour='grey'):
    #old code
#     d1= datetime.datetime.now()
#     d0= datetime.datetime.strptime(timestamp,"%Y%m%d_%H%M%S")
#     dd= d1 - d0
#     text= d0.strftime("%Y.%m.%d %H:%M:%S")
#     if dd.days>0:
#         return '%dd'%dd.days,'%d days ago'%dd.days,text
#     else:
#         return '%dh'%(dd.seconds/3600,),'%d hours ago'%(dd.seconds/3600,),text
    d0= datetime.datetime.strptime(timestamp,"%Y%m%d_%H%M%S")
    text= d0.strftime("%Y.%m.%d %H:%M:%S")
    dd= '<span class="timestamp" style="color:%s">%s</span>'%(colour,timestamp)
    return dd,dd,text


def getOverview(data):

    #count only last test run for branch/test/version    
    d={}    
    for row in data:
        bra,tes,tim,ver,res= row
        if d.has_key((bra,tes,ver)):
            dtim,_= d[(bra,tes,ver)]
            if tim>dtim: d[(bra,tes,ver)]= (tim,res) 
        else:
            d[(bra,tes,ver)]= (tim,res)

#   collect data by branch/version 
    odata={}
    for bra,tes,ver in d:
        tim,res= d[(bra,tes,ver)]
        if odata.has_key((bra,ver)):
            #update result counters 
            odata[(bra,ver)][res]+= [(tes,tim)]
            mint,maxt= odata[(bra,ver)]['time']
            odata[(bra,ver)]['time']= (min(mint,tim),max(maxt,tim))
        else:
            #new branch/version
            odata[(bra,ver)]= {'Identical':[],'MinorDiff':[],'Check':[],'Error':[],'time':(tim,tim)}
            odata[(bra,ver)][res]+= [(tes,tim)]
        
    #odata[branch,version]= {Identical:x,MinorDiff:x,Check:x,Error:x,time:(mintime,maxtime)}
    return odata

def buildOverview(oData):
    
    #sorted list of versions
    versions= list(set([k[1] for k in  oData.keys()]))
    versions.sort()
    
    #sorted list of branches
    branches= list(set([k[0] for k in  oData.keys()]))
    branches.sort()

    html= ''
    results= {}
    
    for ver in versions: html+= '<th>%s</th>'%ver
    html= '<tr><th>branches</th>%s<tr>\n'%html
    for bra in branches:
        results[bra]= {}
        row='<th>%s</th>'%bra
        for ver in versions:
            results[bra][ver]= {}
            row+='<td>'
            for res in ['Error','Check','MinorDiff','Identical']:
                results[bra][ver][res]={}
                num= len(oData[(bra,ver)][res])
                click= "setActiveResult('%s','%s','%s');"%(bra,ver,res)
                row+= ' <a href="#" data-toggle="tooltip" style="text-decoration:none;" title="%d %s tests" onclick="%s"><span class="badge badge-%s">%s</span></a> '%(num,res,click,resultLabelStyle(res),num)
                result=''
                
                #sort tests by name and show 
                last_tests= oData[(bra,ver)][res]
                last_tests.sort()
                for tes,tim in last_tests:
                    _,_,dat= getDateDistance(tim)
                    tesgrp= tes
                    teslnk= tes.split('/')[-1]
                    result+='''<tr>
                                 <td><a href="/test-data/magics/regression_output/%s/%s/%s/%s.html">%s</a></td>
                                 <td><small>%s</small></td>
                               <tr>'''%(bra,teslnk,tim,teslnk,tesgrp,dat)
                result='''
                <h4> %s  / %s  / <span class="badge badge-%s" style="font-size: 20px; height: 24px;line-height: 20px">%s</span></h4>
                <table class="table table-bordered">
                <tr><th>test</th><th>time</th></tr>
                %s
                </table>'''%(bra,ver,resultLabelStyle(res),res,result)
                results[bra][ver][res]= result 
            maxt= oData[(bra,ver)]['time'][1]
            _,tip,dat= getDateDistance(maxt)
            row+='<a href="#" data-toggle="tooltip" style="text-decoration:none;" title="Last test run: %s"><small>%s</small></a></td>\n'%(dat,tip)
        html+='<tr>%s</tr>\n'%row

    results= json.dumps(results) 
    html='''
        <table class="table table-bordered">
        %s
        </table>
    '''%html
    return html,results

def getBranches(data):
    #count only last test run for branch/test/version    
    d={}    
    for row in data:
        bra,tes,tim,ver,res= row
        if d.has_key((bra,tes,ver)):
            dtim,_= d[(bra,tes,ver)]
            if tim>dtim: d[(bra,tes,ver)]= (tim,res) 
        else:
            d[(bra,tes,ver)]= (tim,res)
    
    #build data dictionaries hierarchy
    bdata= {}
    for bra,tes,ver in d:
        tim,res= d[(bra,tes,ver)]
        if not bdata.has_key(bra): bdata[bra]= {}
        if not bdata[bra].has_key(tes): bdata[bra][tes]= {}
        bdata[bra][tes][ver]= (tim,res)
    tests= list(set([k[1] for k in  d.keys()]))
    tests.sort()
    versions= list(set([k[2] for k in  d.keys()]))
    versions.sort()
    return bdata,tests,versions

def buildBranches(bData):
    data,tests,versions= bData
    tabs= []
    content=''
    
    #for each branch
    for bra in data:
        
        #write a header row
        for ver in versions: content+= '<th>%s</th>'%ver
        content= '<tr><th>tests</th>%s<tr>\n'%content
        
        #for each test
        for tes in tests:
            #skip if test has not run for the branch
            if not data[bra].has_key(tes): continue

            tesgrp= tes
            teslnk= tes.split('/')[-1]
            
            row='<th>%s</th>'%tesgrp

            #for each version
            for ver in versions:
                if not data[bra][tes].has_key(ver):
                    #blank if test has not run for the version
                    row+='<td></td>'
                else:
                    tim,res= data[bra][tes][ver]
                    row+='<td>'
                    row+= '<a href="/test-data/magics/regression_output/%s/%s/%s/%s.html" data-toggle="tooltip" style="text-decoration:none;"><span class="label label-%s">%s</span></a>'%(bra,teslnk,tim,teslnk,resultLabelStyle(res),res)
                    short,_,dat= getDateDistance(tim)
                    row+=' <a href="#" data-toggle="tooltip" style="text-decoration:none;" title="Last test run: %s"><small>%s</small></a></td>\n'%(dat,short)
            content+='<tr>%s</tr>\n'%row

        content='''
        <table class="table table-bordered">
        %s
        </table>
        '''%content
        
        #add a branch tab
        tab= {'title': '<h4>%s</h4>'%bra,
                 'id':  'btab-%s'%bra,
            'content':  content}
        tabs.append(tab)
    
    html= writeTab(tabs)
    return html 

def getVersions(data):
    
    #build data dictionaries hierarchy
    vdata= {}
    times= set()
    for (bra,tes,tim,ver,res) in data:
        if not vdata.has_key(bra): vdata[bra]= {}
        if not vdata[bra].has_key(ver): vdata[bra][ver]= {}
        if not vdata[bra][ver].has_key(tes): vdata[bra][ver][tes]= {}
        vdata[bra][ver][tes][tim]= res
        times|= set([tim])
    #sort timestamps from newest to oldest
    times= list(times)
    times.sort()
    times.reverse()
    return vdata,times
    
def buildVersions(vData):
    data,times= vData
    btabs= []
    
    #for each branch
    branches= data.keys()
    branches.sort()
    for bra in branches:
        bcontent= ''
        vtabs=[]
        
        #for each version
        versions= data[bra].keys()
        versions.sort()
        for ver in versions:
            vcontent= ''

            #write a header row
            for tim in times:
                _,tip,dat= getDateDistance(tim,colour="black")
                vcontent+= '<th><a href="#" data-toggle="tooltip" style="text-decoration:none;" title="run time: %s"><small>%s</small></a></th>'%(dat,tip)
                #vcontent+= '<th>%s<br><small>%s</small></th>'%(tip,dat)
            vcontent= '<tr><th>tests</th>%s<tr>\n'%vcontent

            #for each test
            tests= data[bra][ver].keys()
            tests.sort()
            for tes in tests:
                
                tesgrp= tes
                teslnk= tes.split('/')[-1]
                                
                row='<th>%s</th>'%tesgrp
    
                #for each run time
                for tim in times:
                    if data[bra][ver][tes].has_key(tim):
                        res= data[bra][ver][tes][tim]
                        row+='<td>'
                        row+= '<a href="/test-data/magics/regression_output/%s/%s/%s/%s.html" data-toggle="tooltip" style="text-decoration:none;"><span class="label label-%s">%s</span></a>'%(bra,teslnk,tim,teslnk,resultLabelStyle(res),res)
                        row+='</td>\n'
                    else:
                        row+='<td></td>\n'
                vcontent+='<tr>%s</tr>\n'%row
    
            vcontent='''
            <table class="table table-bordered">
            %s
            </table>
            '''%vcontent

            #add a version tab
            tab= {'title': '<h4>%s</h4>'%ver,
                     'id':  'vtab-%s-%s'%(bra,ver),
                'content':  vcontent}
            vtabs.append(tab)
            
        #add a branch tab
        bcontent= writeTab(vtabs)
        tab= {'title': '<h4>%s</h4>'%bra,
                 'id':  'vtab-%s'%bra,
            'content':  bcontent}
        btabs.append(tab)
    
    html= writeTab(btabs)
    return html 

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
            p_result= maxResult(params['result'][ver].values())
            data.append((p_branch,p_test,p_time,p_version,p_result))
        
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
