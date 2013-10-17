import sys
import re
from subprocess import call,Popen,PIPE,check_output
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
        #call the difference tool
        ver_dif= suffix(ver_ref,'_tdif')
        command='/home/graphics/cgjd/development/git/coderev/codediff.py -o %s %s %s'%(ver_dif,reference,ver_ref)
        assert(call(command,stdout=PIPE,stderr=PIPE,shell=True)==0)
        
        #extract the 1st table of the HTML output (it is the only relevant part)
        with open(ver_dif) as f: html= f.read()
        s= BeautifulSoup(html)
        diff= s.find('table')  
    except:
        sys.stderr.write("ERROR comparing '%s' and '%s' with PerceptualDiff"%(reference,ver_ref))
    return diff

        
def writeHtmlReport(params,usage,stdout,stderr):

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
    def stdout_diff(stdout,ver,reference):
        ref= extension(reference,'out')
        with open(ref,'w') as f: f.write(stdout)
        ref_ver= prefix(ref,ver+'_')
        return TextDiff(ref,ref_ver)
    stdout_diffs= ''
    for ver in params['versions']: stdout_diffs+= '<h3> Comparing output with version '+ver+'</h3>'+stdout_diff(stdout,ver,params['reference'])
    report= report.replace('STDOUT_DIFS',stdout_diffs) 

    #stderr section
    report= report.replace('STDERR',stderr) 
    
    #output plots
    def plots(ver,ref):
        ref_ver  = prefix(ref,ver+'_')
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
    for ver in params['versions']: output_plots+= '<h3> Comparing with version '+ver+'</h3>'+plots(ver,params['reference'])
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
