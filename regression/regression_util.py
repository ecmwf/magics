# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#Python standard library 
import sys
import re
import os
import json
import datetime
from subprocess import call,Popen,PIPE

#Third party modules
from BeautifulSoup import BeautifulSoup


#resources usage
usage= {0: 'time in user mode',    # (float)
        1: 'time in system mode',  # (float)
        2: 'maximum resident set size',
        3: 'shared memory size',
        4: 'unshared memory size',
        5: 'unshared stack size',
        6: 'page faults not requiring I/O',
        7: 'page faults requiring I/O',
        8: 'number of swap outs',
        9: 'block input operations',
       10: 'block output operations',
       11: 'messages sent',
       12: 'messages received',
       13: 'signals received',
       14: 'voluntary context switches',
       15: 'involuntary context switches'}

def usage2Dict(u_struc):
    res= {}
    i= 0
    for att in u_struc:
        res[usage[i]]= att
        i+=1
    return res

def prefix(filename,pre):
    aux= filename.split('/')
    aux[-1]= pre + aux[-1]
    return '/'.join(aux)

def suffix(filename,suf):
    aux= filename.split('.')
    if len(aux)>1:
        aux[-2]= aux[-2] + suf
        return '.'.join(aux)
    else:
        return filename + suf

def extension(filename,newExtension):
    aux= filename.split('.')
    aux[-1]= newExtension
    return '.'.join(aux)

def upload(filename,destination):
    #create destination directory path if it does not exist
    dir_path= '/download/data/test-data/' + '/'.join(destination.split('/')[:-1])
    command= 'ssh deploy@download-int.ecmwf.int "mkdir -p %s"'%dir_path
    e= call(command,shell=True)
    if not e==0:
        sys.stderr.write("ERROR uploading the file '%s': Server directory path '%s' cannot be created'"%(filename,dir_path))
       
    #upload using build scp destination path
    destination = "deploy@download-int.ecmwf.int:/download/data/test-data/%s"%destination
    command= ' '.join(["scp","-r",filename, destination])
    e= call(command,shell=True)
    if not e==0:
        sys.stderr.write("ERROR uploading the file '%s' into '%s'"%(filename,destination))
    return e
    
        
def ImageMagick_compare(reference,ver_ref,ver_dif):
    #compare with test's output and return number of different pixels
    diff= 0
    try:
        command='compare -metric AE -dissimilarity-threshold 1 "%s" "%s" %s'%(reference,ver_ref,ver_dif)
        p= Popen(command,stdout=PIPE,stderr=PIPE,shell=True)
        _,stderr= p.communicate()
        diff= int(stderr)
    except:
        sys.stderr.write("ERROR comparing '%s' and '%s' with ImageMagick_compare"%(reference,ver_ref))
    #print 'ImageMagick_compare',(reference,ver_ref,ver_dif),diff
    return diff

def PerceptualDiff_compare(reference,ver_ref,ver_dif,pix_thres=100):
    #compare with test's output and return number of different pixels
    diff= 0
    try:
        command='/usr/local/apps/perceptualdiff/1.1.2/bin/perceptualdiff -verbose -fov 90 -threshold %d %s %s -output %s'%(pix_thres,reference,ver_ref,ver_dif)
        p= Popen(command,stdout=PIPE,stderr=PIPE,shell=True)
        stdout,_ = p.communicate()
        p= re.compile('([0-9]+) pixels are different')
        m= p.search(stdout)
        diff= int(m.group(1))
    except:
        sys.stderr.write("ERROR comparing '%s' and '%s' with PerceptualDiff"%(reference,ver_ref))
    #print 'PerceptualDiff_compare',(reference,ver_ref,ver_dif),diff
    return diff

def resultLabelStyle(lab):
    c= {'Identical': 'success',
        'MinorDiff': 'info',
        'Check':     'warning',
        'Error':     'important'}
    return c[lab]

def resultLabelColour(lab):
    c= {'Identical':  'green',
        'MinorDiff': 'blue',
        'Check':     'goldenrod',
        'Error':     'red'}
    return c[lab]

def maxResult(labels):
    for lab in ['Error','Check','MinorDiff']:
        if lab in labels: return lab
    return labels[0]#should be Identical

def result(thres,diff,pdiff,pixels):
    d,pd= 100.0*diff/pixels,100.0*pdiff/pixels
    val= max(d,pd)
    text,colour,style= ['']*3
    if diff==pdiff==0:         text,colour,style= 'Identical', 'green',     'success'
    elif val<0.5*thres:        text,colour,style= 'MinorDiff', 'blue',      'info' 
    elif 0.5*thres<=val<thres: text,colour,style= 'Check',     'goldenrod', 'warning'
    elif thres<=val:           text,colour,style= 'Error',     'red',       'error'
    return text,colour,style,val

def resultLabel(thres,diff,pdiff,pixels):
    return result(thres,diff,pdiff,pixels)[0]

def resultColour(thres,diff,pdiff,pixels):
    return result(thres,diff,pdiff,pixels)[1]

def resultStyle(thres,val):    
    return result(thres,val,val,100)[2]

def resultText(thres,diff,pdiff,pixels):
    lab,col,_,val= result(thres,diff,pdiff,pixels)
    return '<td><b style="color:%s">%s</b> <small>(%.1f%%)</small></td>'%(col,lab,val)


def TextDiff(reference,ver_ref):
    #compares 2 text files and return an HTML table (an string as well) with the diff results
    diff= ''
    try:
    #if True:
        #call the difference tool
        ver_dif= suffix(ver_ref,'_tdif')
        command='python /home/graphics/cgjd/development/git/coderev/codediff.py -y -o %s %s %s'%(ver_dif,reference,ver_ref)
        p= Popen(command,stdout=PIPE,stderr=PIPE,shell=True)
        stdout,stderr = p.communicate()
        
        #extract the 1st table of the HTML output (it is the only relevant part)
        with open(ver_dif) as f: html= f.read()
        s= BeautifulSoup(html)
        diff= str(s.find('table'))  
    except:
        sys.stderr.write("ERROR comparing '%s' and '%s' with codediff.py\n"%(reference,ver_ref))
        sys.stderr.write(stderr)
        sys.stdout.write(stdout)
        
    return diff

def splitOutput(reference):
    #splits a pdf or ps reference output document into a set of png files (one for each document page)
    resolution= '50'#dpi
    png_files= []
    try:
        print reference
        if reference[-2:]=='ps':
            command='pstopnm -dpi=%s %s'%(resolution,reference)
            assert(0==call(command,stdout=PIPE,stderr=PIPE,shell=True))
            #output filename: reference999.ppm
            ppm_pat= re.compile(reference[:-3]+'[0-9]+.ppm')
            ppm_files= [f for f in os.listdir('.') if ppm_pat.match(f)]
            for ppm_file in ppm_files:
                png_file= ppm_file.replace('.ppm','.png')
                command='pnmtopng %s > %s; rm -f %s'%(ppm_file,png_file,ppm_file)
                assert(0==call(command,shell=True))
                #final filename: reference999.png
                png_files.append(png_file) 
                
        elif reference[-3:]=='pdf':
            command='pdftoppm -png -r %s %s %s'%(resolution,reference,reference[:-4])
            #output filename: reference-9.png 
            assert(0==call(command,shell=True))
            tmp_pat= re.compile(reference[:-4]+'-[0-9]+.png')
            tmp_files= [f for f in os.listdir('.') if tmp_pat.match(f)]
            for tmp_file in tmp_files:
                pag_num= int((tmp_file.split('-')[1]).split('.')[0])
                png_file= '%s%03d.png'%(reference[:-4],pag_num)
                command='mv %s %s'%(tmp_file,png_file)
                assert(0==call(command,shell=True))
                #final filename: reference999.png
                png_files.append(png_file) 
    except:
        sys.stderr.write("ERROR splitting the file '%s' into png images"%(reference))
    png_files.sort()
    return png_files

def writeTab(tabs,tab_class='nav-tabs'):
    tab_lines=''
    tab_contents=''
    first= True
    for tab in tabs:
        if first:
            tab_lines+= '<li class="active"><a data-toggle="tab" href="#%s">%s</a></li>'%(tab['id'],tab['title'])
            tab_contents+='<div id="%s" class="tab-pane active">%s</div>'%(tab['id'],tab['content'])
            first= False
        else:
            tab_lines+= '\n\t<li><a data-toggle="tab" href="#%s">%s</a></li>'%(tab['id'],tab['title'])
            tab_contents+='<div id="%s" class="tab-pane">%s</div>'%(tab['id'],tab['content'])
    return '''
    <div class="tabbable">
        <ul class="nav %s">%s</ul>
        <div class="tab-content">%s</div><!-- /.tab-content -->
    </div><!-- /.tabbable -->
    '''%(tab_class,tab_lines,tab_contents)

#####################################################################
#####################################################################
#####################################################################
    
def writeHtmlReport(params,usage,stdout,stderr,ref_pages,ref_ver_pages):

    with open('report_template.html','r') as f: report= f.read()

    #test name
    test_name= params['reference'].split('.')[0]
    report= report.replace('TEST_NAME',test_name)
    
    #branch name
    report= report.replace('BRANCH_NAME',params['branch_name'])

    #results section
    thre= params['threshold']
    diff= params['diff'] 
    pdif= params['pdiff']
    pixe= params['pixels']
    mess= params['exit_message']
    results= ''
    for i in range(len(pixe)): results+= '<th>page %d</th>'%(i+1,)
    results= '<tr><th>test threshold: <span style="color:red">%d%%</span></th>%s<tr>\n'%(int(params['threshold']),results)
    try:
        valss= []
        for ver in params['versions']:
            vals= []
            pages=''
            for i in range(len(pixe)):
                _,_,_,val= result(thre,diff[ver][i],pdif[ver][i],pixe[i])
                pages+= resultText(thre,diff[ver][i],pdif[ver][i],pixe[i])
                vals+= [val]
                sty= resultStyle(thre,max(vals))
            results+='<tr class="%s"><th>%s</th>%s</tr>\n'%(sty,ver,pages)
            valss+= vals
    except:
        #SET ERROR VALUE 
        valss= [100]
    results='''
        <br>
        <table class="table table-bordered">
            <tr class="%s"><th>exit message</th><td><b>%s</b></td></tr>
        </table>
        <br>
        <table class="table table-bordered">
        %s
        </table>
    '''%(resultStyle(thre,max(valss)),mess,results)
    report= report.replace('TEST_RESULTS',results)


    #parameters  section
    for name in params: report= report.replace(name.upper(),str(params[name]))
                    
    #usage tabs
    usa_ver= {}
    for ver in params['versions']:
        ref= prefix(extension(params['reference'],'usa'),ver+'_')
        try:
            with open(ref) as f: usa_ver[ver]= json.loads(f.read())
        except:
            pass
        
    def run_info_line_colour(name,test_value,ver_values):
        def colour(v1,v0):
            CF= 0.5 #chage factor: 0.5 ~ 50%
            r,g,b= (0,0,0)
            try:
                r= max(0,min(255,int(255.0*(v1-v0)/(v0*CF))))
                g= max(0,min(255,int(255.0*(v0-v1)/(v0*CF))))
            except:
                if v1>v0: r=255
            return 'rgb(%d,%d,%d)'%(r,g,b)
        def precision(v):
            if   v==0:  return '0'
            elif v>=10: return '%d'%int(v)
            else:       return '%.2f'%v
        res= '<tr style="text-align:right"><td>%s</td><td style="color:%s">%s</td>'%(name,colour(test_value,ver_values[0]),precision(test_value))
        for i in range(len(ver_values)):
            if i+1<len(ver_values):
                res+= '<td style="color:%s">%s</td>'%(colour(ver_values[i],ver_values[i+1]),precision(ver_values[i]))
            else:
                res+= '<td style="color:%s">%s</td>'%('rgb(0,0,0)',precision(ver_values[i]))
        res+= '</tr>\n'
        return res
    
    usa_names= usage.keys()
    usa_names.sort() 

###################################

    tabs= []
    pixel_difs= {}
    for ver in params['versions']:
        ver_id= 'ver'+ver.replace('.','-')
        tab= {'title': '<h4>with %s</h4>'%ver,
              'id':    ver_id}
        
        #stdout section
        def stdout_TextDiff(stdout,ver,reference):
            ref= extension(reference,'out')
            with open(ref,'w') as f: f.write(stdout)
            ref_ver= prefix(ref,ver+'_')
            return TextDiff(ref,ref_ver)
        stdout_difs= stdout_TextDiff(stdout,ver,params['reference'])

        #stderr section    
        def stderr_TextDiff(stderr,ver,reference):
            ref= extension(reference,'err')
            with open(ref,'w') as f: f.write(stderr)
            ref_ver= prefix(ref,ver+'_')
            return TextDiff(ref,ref_ver)
        stderr_difs= stderr_TextDiff(stderr,ver,params['reference'])

        #run info section
        run_info='<tr><th>resource</th><th>%s</th><th>%s</th></tr>\n'%(params['branch_name'],ver)
        for name in usa_names:
            try:
                run_info+= run_info_line_colour(name,usage[name],[usa_ver[ver][name]])
            except:
                pass
        run_info= '<table class="table table-bordered">'+run_info+'</table>'
       
        #output plots
        def plots(div_id,page,npix,ndif,npdif,ref,ref_ver):
            ref_dif  = suffix(ref_ver,'_diff')
            def size(fn): return int(os.stat(fn).st_size/1024)
            
            comparison='''
            <div style="text-align:center;">
                <span id="%s_pixel_diff">Difference: %d (%.1f%%) pixels</span>
            </div>'''%(div_id,ndif,100.0*ndif/npix)
            
            return '''
            <div class="row" style="">
                <div class="span4 left" style=" ">
                %s - %dK
                </div>
                <div class="span4 center" style=" ">
                %s
                </div>
                <div class="span4 right" style=" ">
                %s - %dK
                </div>
            </div>            
            <div class="row" style="margin:0;">
                <div id="%s_left" class="image left span4" style="margin:0;width:33.1%%;border:1px solid #DA0000" onscroll="onScrollDiv(this);">
                    <image class="plot" style="max-width:none;width:100%%" src="%s">
                </div>
                <div id="%s_diff" class="image center span4" style="margin:0;width:33.1%%;border:1px solid #DA0000" onscroll="onScrollDiv(this);">
                    <image class="plot plot_diff" style="max-width:none;width:100%%" src="%s">
                </div>
                <div id="%s_right" class="image right span4" style="margin:0;width:33.1%%;border:1px solid #AAAAAA" onscroll="onScrollDiv(this);">
                    <image class="plot" style="max-width:none;width:100%%" src="%s">
                </div>
            </div>
            '''%(ref,size(ref),comparison,ref_ver,size(ref_ver),div_id,ref,div_id,ref_dif,div_id,ref_ver)

        page_tabs= []
        try:
            for ipage in range(len(ref_pages)):
                ref_page= ref_pages[ipage]
                ref_ver_page= ref_ver_pages[ver][ipage]
                npix= params['pixels'][ipage]
                ndif= params['diff'][ver][ipage]
                npdif= params['pdiff'][ver][ipage]
                ver_id= ver + '-' + str(ipage)
                pixel_difs[ver_id]= {'pdiff':[npdif,100.0*npdif/npix],'mdiff':[ndif,100.0*ndif/npix]}
                page_title= '<span style="color:'+resultColour(params['threshold'],ndif,npdif,npix)+'">Page '+str(ipage+1)+'</span>' 
                page_tabs+= [{'id':tab['id']+'-page'+str(ipage),'title': page_title,'content': plots(ver_id,ipage,npix,ndif,npdif,ref_page,ref_ver_page)}]
        except:
            pass
        output_plots= writeTab(page_tabs)
        output_plots+= '''
        <div style="text-align:center;">
            <br>
            <form id="zoom_form" name="zoom_form">
                    Zoom: 
                    <input type="radio" name="zoomgroup" value="100" checked onclick="onClickZoom(this.value);"> 100%&nbsp;
                    <input type="radio" name="zoomgroup" value="200" onclick="onClickZoom(this.value);">         200%&nbsp;
                    <input type="radio" name="zoomgroup" value="400" onclick="onClickZoom(this.value);">         400%&nbsp;
                    <input type="radio" name="zoomgroup" value="800" onclick="onClickZoom(this.value);">         800%&nbsp;
            </form>
            <form id="diff_form" name="diff_form">
                    <input type="radio" name="diffgroup" value="mdiff" checked onclick="onClickDiff(this.value);">ImageMagick&nbsp;
                    <input type="radio" name="diffgroup" value="pdiff" onclick="onClickDiff(this.value);">Perceptual Diff&nbsp;
                    <br>
            </form>
        </div>'''


        #tab content
        tab_content= [{'id':tab['id']+'-plot',    'title':'Output Plot',      'content': output_plots},
                      {'id':tab['id']+'-stdout',  'title':'Standard Output',  'content': stdout_difs},
                      {'id':tab['id']+'-stderr',  'title':'Standard Error',   'content': stderr_difs},
                      {'id':tab['id']+'-runinfo', 'title':'Run Info',         'content': run_info}]
        tab['content']= writeTab(tab_content)
        
        tabs.append(tab)
   
    tab_vers= writeTab(tabs)#,tab_class='nav-pills')
    report= report.replace('TAB_VERS',tab_vers)

    report= report.replace('PIXEL_DIFS',json.dumps(pixel_difs))
    
                #last step: nicely indent  the HTML code
    return report#BeautifulSoup(report).prettify()

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
            if not oData.has_key((bra,ver)): continue
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
    versions= {}
    for bra,tes,ver in d:
        tim,res= d[(bra,tes,ver)]
        if not bdata.has_key(bra): bdata[bra]= {}
        if not bdata[bra].has_key(tes): bdata[bra][tes]= {}
        bdata[bra][tes][ver]= (tim,res)
        if not versions.has_key(bra): versions[bra]= set([])
        versions[bra]|= set([ver])
    tests= list(set([k[1] for k in  d.keys()]))
    tests.sort()
    #versions= list(set([k[2] for k in  d.keys()]))
    #versions.sort()
    #versions.reverse()
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
        vs= list(versions[bra])
        vs.sort()
        vs.reverse()
        for ver in vs: content+= '<th>%s</th>'%ver
        content= '<tr><th>tests\\versions</th>%s<tr>\n'%content
        
        #for each test
        for tes in tests:
            #skip if test has not run for the branch
            if not data[bra].has_key(tes): continue

            tesgrp= tes
            teslnk= tes.split('/')[-1]
            
            row='<td>%s</td>'%tesgrp

            #for each version
            for ver in vs:
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
