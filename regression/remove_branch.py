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
import datetime
from BeautifulSoup import BeautifulSoup 

#Project modules
from regression_util import upload,maxResult,resultLabelStyle,writeTab,resultLabelColour

__author__  = 'cgs,cgjd'
__date__    = '2014-01-10'
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
        bra,tes,tim,ver,res,mes,dif,thr= row
        if d.has_key((bra,tes,ver)):
            dtim,_,_,_,_= d[(bra,tes,ver)]
            if tim>dtim: d[(bra,tes,ver)]= (tim,res,mes,dif,thr) 
        else:
            d[(bra,tes,ver)]= (tim,res,mes,dif,thr)

#   collect data by branch/version 
    odata={}
    for bra,tes,ver in d:
        tim,res,mes,dif,thr= d[(bra,tes,ver)]
        if odata.has_key((bra,ver)):
            #update result counters 
            odata[(bra,ver)][res]+= [(tes,tim,mes,dif,thr)]
            mint,maxt= odata[(bra,ver)]['time']
            odata[(bra,ver)]['time']= (min(mint,tim),max(maxt,tim))
        else:
            #new branch/version
            odata[(bra,ver)]= {'Identical':[],'MinorDiff':[],'Check':[],'Error':[],'time':(tim,tim)}
            odata[(bra,ver)][res]+= [(tes,tim,mes,dif,thr)]
        
    #odata[branch,version]= {Identical:x,MinorDiff:x,Check:x,Error:x,time:(mintime,maxtime)}
    return odata

def buildOverview(oData):
    
    #sorted list of versions
    versions= list(set([k[1] for k in  oData.keys()]))
    versions.sort()
    versions.reverse()
    
    #sorted list of branches
    branches= list(set([k[0] for k in  oData.keys()]))
    branches.sort()
    branches.reverse()

    html= ''
    results= {}
    
    for ver in versions: html+= '<th>%s</th>'%ver
    html= '<tr><th>branches\\versions</th>%s<tr>\n'%html
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
                #row+= ' <a href="#" onclick="%s">%s</a> '%(click,num)
                result=''
                
                #sort tests by name and show 
                last_tests= oData[(bra,ver)][res]
                last_tests.sort()
                for tes,tim,mes,dif,thr in last_tests:
                    _,_,dat= getDateDistance(tim)
                    tesgrp= tes
                    teslnk= tes.split('/')[-1]
                    result+='''<tr>
                                 <td><a href="/test-data/magics/regression_output/%s/%s/%s/%s.html"><span style="color:%s;">%s</span></a></td>
                                 <td><small>%s</small></td>
                                 <td><small>%.1f%%</small></td>
                                 <td><small>%.1f%%</small></td>
                                 <td><small>%s</small></td>
                               <tr>'''%(bra,teslnk,tim,teslnk,resultLabelColour(res),tesgrp,dat,dif,thr,mes)
                result='''
                <h4> %s  / %s  / <span class="badge badge-%s" style="font-size: 20px; height: 24px;line-height: 20px">%s</span></h4>
                <table class="table table-bordered">
                <tr><th>test</th><th>compilation time</th><th>dif.</th><th>thres.</th><th>exit message</th></tr>
                %s
                </table>'''%(bra,ver,resultLabelStyle(res),res,result)
                results[bra][ver][res]= result
                 
            maxt= oData[(bra,ver)]['time'][1]
            _,tip,dat= getDateDistance(maxt)
            row+='<a href="#" data-toggle="tooltip" style="text-decoration:none;" title="Last compilation: %s"><small>%s</small></a></td>\n'%(dat,tip)
        html+='<tr>%s</tr>\n'%row

    results= json.dumps(results,indent=4) 
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
        bra,tes,tim,ver,res,_,_,_= row
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
    versions.reverse()
    return bdata,tests,versions

def buildBranches(bData):
    data,tests,versions= bData
    tabs= []
    
    #for each branch
    i= 0
    branches= data.keys()
    branches.sort()
    branches.reverse()
    for bra in branches:

        #write a header row
        content=''
        for ver in versions: content+= '<th>%s</th>'%ver
        content= '<tr><th>tests\\versions</th>%s<tr>\n'%content
        
        #for each test
        for tes in tests:
            #skip if test has not run for the branch
            if not data[bra].has_key(tes): continue

            tesgrp= tes
            teslnk= tes.split('/')[-1]
            
            row='<td>%s</td>'%tesgrp

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
                    row+=' <a href="#" data-toggle="tooltip" style="text-decoration:none;" title="Last compilation: %s"><small>%s</small></a></td>\n'%(dat,short)
            content+='<tr>%s</tr>\n'%row

        content='''
        <table class="table table-bordered">
        %s
        </table>
        '''%content
    
        #add a branch tab
        tab= {'title': '<h4>%s</h4>'%bra,
                 'id': 'btab-%d'%i,
            'content': content}
        i+= 1
        tabs.append(tab)
    
    html= writeTab(tabs)
    return html 

def getVersions(data):
    
    #build data dictionaries hierarchy
    vdata= {}
    times= set()
    for (bra,tes,tim,ver,res,_,_,_) in data:
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
    branches.reverse()
    i=0
    for bra in branches:
        bcontent= ''
        vtabs=[]
        
        #for each version
        versions= data[bra].keys()
        versions.sort()
        versions.reverse()
        j=0
        for ver in versions:
            vcontent= ''

            #get timestamps for current version
            vtimes= set()
            for tim in times:
                for tes in data[bra][ver]:
                    if data[bra][ver][tes].has_key(tim): vtimes|= set([tim])
            
            #write version header row
            vtimes= list(vtimes)
            vtimes.sort()
            vtimes.reverse()
            for tim in vtimes:
                _,tip,dat= getDateDistance(tim,colour="black")
                vcontent+= '<th><a href="#" data-toggle="tooltip" style="text-decoration:none;" title="compilation time: %s"><small>%s</small></a></th>'%(dat,tip)
                #vcontent+= '<th>%s<br><small>%s</small></th>'%(tip,dat)
            vcontent= '<tr><th>tests\\times</th>%s<tr>\n'%vcontent

            #for each test
            tests= data[bra][ver].keys()
            tests.sort()
            for tes in tests:


                
                tesgrp= tes
                teslnk= tes.split('/')[-1]
                                
                row='<td>%s</td>'%tesgrp
    
                #for each run time
                for tim in vtimes:
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
                     'id': 'vtab-%d-%d'%(i,j),
                'content': vcontent}
            j+=1
            vtabs.append(tab)
            
        #add a branch tab
        bcontent= writeTab(vtabs)
        tab= {'title': '<h4>%s</h4>'%bra,
                 'id': 'vtab-%i'%i,
            'content': bcontent}
        i+=1
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
        command= 'ssh deploy@download-admin "rm -rf /home/deploy/test-data/magics/regression_output/%s"'%branch)
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
