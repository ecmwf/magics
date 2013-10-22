import sys
import re
from subprocess import call,Popen,PIPE,check_output
from BeautifulSoup import BeautifulSoup
import os

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
    return diff

def PerceptualDiff_compare(reference,ver_ref,ver_dif,pix_thres=100):
    #compare with test's output and return number of different pixels
    diff= 0
    try:
        command='/var/tmp/cgjd/perforce/magics/tools/versioncmp/pdiff/perceptualdiff/perceptualdiff -verbose -fov 90 -threshold %d %s %s -output %s'%(pix_thres,reference,ver_ref,ver_dif)
        p= Popen(command,stdout=PIPE,stderr=PIPE,shell=True)
        stdout,_ = p.communicate()
        p= re.compile('([0-9]+) pixels are different')
        m= p.search(stdout)
        diff= int(m.group(1))
    except:
        sys.stderr.write("ERROR comparing '%s' and '%s' with PerceptualDiff"%(reference,ver_ref))
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
    resolution= '150'#dpi
    png_files= [] 
    try:
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
    
def writeHtmlReport(params,usage,stdout,stderr,ref_pages,ref_ver_pages):

    with open('report_template.html','r') as f: report= f.read()

    #test name
    test_name= params['reference'].split('.')[0]
    report= report.replace('TEST_NAME',test_name)

    #parameters and result section
    for name in params: report= report.replace(name.upper(),str(params[name]))

    #run info section
    run_info=''
    for name in usage: run_info+= '<tr><td>'+name+':</td><td>'+str(usage[name])+'</td></tr>'
    report= report.replace('RUN_INFO',run_info) 

    #stdout section
    def stdout_TextDiff(stdout,ver,reference):
        ref= extension(reference,'out')
        with open(ref,'w') as f: f.write(stdout)
        ref_ver= prefix(ref,ver+'_')
        return TextDiff(ref,ref_ver)
    stdout_difs= ''
    for ver in params['versions']: stdout_difs+= '<h3> Comparing output with version '+ver+'</h3>'+stdout_TextDiff(stdout,ver,params['reference'])
    report= report.replace('STDOUT_DIFS',stdout_difs) 

    #stderr section    def stdout_TextDiff(stdout,ver,reference):
    def stderr_TextDiff(stderr,ver,reference):
        ref= extension(reference,'err')
        with open(ref,'w') as f: f.write(stderr)
        ref_ver= prefix(ref,ver+'_')
        return TextDiff(ref,ref_ver)
    stderr_difs= ''
    for ver in params['versions']: stderr_difs+= '<h3> Comparing output with version '+ver+'</h3>'+stderr_TextDiff(stderr,ver,params['reference'])
    report= report.replace('STDERR_DIFS',stderr_difs) 
    
    
    #report= report.replace('STDERR',stderr) 
    
    #output plots
    def plots(ref,ref_ver):
        ref_dif  = suffix(ref_ver,'_diff')
        ref_pdif = suffix(ref_ver,'_pdif')
       
        return '''
        <table>
            <tr><td>%s</td></tr>
            <tr><td><image src="%s"></td></tr>
            <tr><td>%s</td></tr>
            <tr><td><image src="%s"></td></tr>
            <tr><td>%s</td></tr>
            <tr><td><image src="%s"></td></tr>
            <tr><td>%s</td></tr>
            <tr><td><image src="%s"></td></tr>
        </tr></table>
        '''%(ref,ref,ref_dif,ref_dif,ref_pdif,ref_pdif,ref_ver,ref_ver)
    output_plots= ''
    for ver in params['versions']:
        for ipage in range(len(ref_pages)):
            ref_page= ref_pages[ipage]
            ref_ver_page= ref_ver_pages[ipage]
            output_plots+= '<h3> Comparing Page '+str(ipage+1)+' with version '+ver+'</h3>'+plots(ref_page,ref_ver_page)
    report= report.replace('OUTPUT_PLOTS',output_plots) 
    
    return report
   
   
    
    
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
