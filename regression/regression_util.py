#Python standard library 
import sys
import re
import os
import json
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
    print 'scp',filename,destination
    destination = "deploy@download-admin:test-data/%s"%destination
    e= call(["scp",filename, destination])
    if not e==0:
        sys.stderr.write("ERROR uploading the file '%s' into '%s'"%(filename,destination))
        
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

    
def writeHtmlReport(params,usage,stdout,stderr,ref_pages,ref_ver_pages):

    with open('report_template.html','r') as f: report= f.read()

    #test name
    test_name= params['reference'].split('.')[0]
    report= report.replace('TEST_NAME',test_name)
    
    #branch name
    report= report.replace('BRANCH_NAME',params['branch_name'])

    #parameters and result section
    for name in params: report= report.replace(name.upper(),str(params[name]))

    usa_ver= {}
    for ver in params['versions']:
        ref= prefix(extension(params['reference'],'usa'),ver+'_')
        with open(ref) as f: usa_ver[ver]= json.loads(f.read())

    #run info section
    run_info='<tr><td></td><td><b>test</b></td>'
    for ver in params['versions']: run_info+= '<td><b>'+ver+'</b></td>'
    run_info+='</tr>\n'
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
    for name in usa_names:
        run_info+= run_info_line_colour(name,usage[name],[usa_ver[ver][name] for ver in params['versions']])
#        run_info+= '<tr><td>'+name+':</td><td>'+str(usage[name])+'</td>'
#        for ver in params['versions']:
#            run_info+= '<td>'+str(usa_ver[ver][name])+'</td>'
#        run_info+='</tr>'
    report= report.replace('RUN_INFO',run_info) 

###################################

    tabs= []
    for ver in params['versions']:
       
        tab= {'title': 'VERSION %s'%ver,
              'id':    'ver'+ver.replace('.','-')}
        
        #stdout section
        def stdout_TextDiff(stdout,ver,reference):
            ref= extension(reference,'out')
            with open(ref,'w') as f: f.write(stdout)
            ref_ver= prefix(ref,ver+'_')
            return TextDiff(ref,ref_ver)
        stdout_difs= ''
        stdout_difs+= '<h3> Comparing output with version '+ver+'</h3>'+stdout_TextDiff(stdout,ver,params['reference'])

        #stderr section    
        def stderr_TextDiff(stderr,ver,reference):
            ref= extension(reference,'err')
            with open(ref,'w') as f: f.write(stderr)
            ref_ver= prefix(ref,ver+'_')
            return TextDiff(ref,ref_ver)
        stderr_difs= ''
        stderr_difs+= '<h3> Comparing error with version '+ver+'</h3>'+stderr_TextDiff(stderr,ver,params['reference'])
       
        #output plots
        def plots(ref,ref_ver):
            ref_dif  = suffix(ref_ver,'_diff')
            #ref_pdif = suffix(ref_ver,'_pdif')

            return '''
                <div id="left" class="image left" style="border:0px solid #DA0000" onscroll="onScrollDiv(this);">
                    %s<br>
                    <image src="%s">
                </div>
                <div id="diff" class="image center" style="border:0px solid #DA0000" onscroll="onScrollDiv(this);">
                    %s<br>
                    <image src="%s">
                </div>
                <div id="right" class="image right" style="border:0px solid #AAAAAA" onscroll="onScrollDiv(this);">
                    %s<br>
                    <image src="%s">
                </div>
            '''%(ref,ref,ref_dif,ref_dif,ref_ver,ref_ver)
           
#             return '''
#             <table>
#                 <tr><td>%s</td></tr>
#                 <tr><td><image src="%s"></td></tr>
#                 <tr><td>%s</td></tr>
#                 <tr><td><image src="%s"></td></tr>
#                 <tr><td>%s</td></tr>
#                 <tr><td><image src="%s"></td></tr>
#                 <tr><td>%s</td></tr>
#                 <tr><td><image src="%s"></td></tr>
#             </tr></table>
#             '''%(ref,ref,ref_dif,ref_dif,ref_pdif,ref_pdif,ref_ver,ref_ver)

        output_plots= ''
        page_tabs= []
        for ipage in range(len(ref_pages)):
            ref_page= ref_pages[ipage]
            ref_ver_page= ref_ver_pages[ver][ipage]
            page_tabs+= [{'id':tab['id']+'-page'+str(ipage),'title':'Page '+str(ipage+1),'content': plots(ref_page,ref_ver_page)}]
        output_plots+= writeTab(page_tabs)

        #tab content
        tab_content= [{'id':tab['id']+'-plot',   'title':'Output Plot',      'content': output_plots},
                      {'id':tab['id']+'-stdout', 'title':'Standard Output',  'content': stdout_difs},
                      {'id':tab['id']+'-stderr',  'title':'Standard Error',  'content': stderr_difs}]
        tab['content']= writeTab(tab_content)
        
        tabs.append(tab)
   
    tab_vers= writeTab(tabs,tab_class='nav-pills')
    report= report.replace('TAB_VERS',tab_vers)
    
    #last step: nicely indent  the HTML code
    return BeautifulSoup(report).prettify()
    
    
#     n=0
#     tab_nav=''
#     for ver in params['versions']:
#         if n==0: tab_nav+= '<li class="active"><a href="#'+ver+'" data-toggle="tab"> version '+ver+'</a></li>\n'
#         else:    tab_nav+= '<li><a href="#'+ver+'"data-toggle="tab"> version '+ver+'</a></li>\n'
#         n+=1
#     tab_nav= '<ul class="nav nav-tabs">'+tab_nav+'<ul>\n'
# 
#     n=0
#     tab_con=''
#     for ver in params['versions']:
#         if n==0: tab_con+= '<div id="'+ver+'" class="tab-pane active">'+plots(ver,params['reference'])+'</div>'
#         else:    tab_con+= '<div id="'+ver+'" class="tab-pane">'+plots(ver,params['reference'])+'</div>'
#         n+=1
#     tab_con= '<div class="tab-content">'+tab_con+'</div>'
#     
#     output_plots= '<div>'+tab_nav+tab_con+'</div>'
    
    
    #1 calculate image diff
    
    #2 calculate stdout diff
    
    #3 calculate stderr diff
    
    #4 calculate usage diff
