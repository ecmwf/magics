'''
Created on 14 May 2012

@author: cgjd
'''

from os import system
import csv
import json

if __name__ == '__main__':
    pass


def buildSinglePage(parid,parti,title):
    f= file('./attachments/%s/%s'%(parid,title))
    s= f.read()
    f.close()
    j= json.loads(s)

    #get single page template
    f= file('./example_template.html')
    htemplate= f.read()
    f.close()
    
    
    data= []
    
    ######################################
    # no count the 1 single definition json files
    if len(j['examples'])==1: return data
    ######################################
    
    
    #for each example
    for e in j['examples']:
        
        name= e['name']

        #build the json definition
        je={'examples':[e]}
        se= json.dumps(je)
        f= file('./attachments/%s/%s.json'%(parid,name),'w')
        f.write(se)
        f.close()
        
        #build the page
        html0= htemplate.replace('%PARENT_ID%',parid)
        html=  html0.replace('%SINGLE_EXAMPLE%',name)
        f= file('./attachments/%s/%s.html'%(parid,name),'w')
        f.write(html)
        f.close()

#####################################################
# only generate files for this example page
#        if parid== '14157255':
	if True:
#####################################################

            #upload the page
            res = system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action addPage --space "MAGP" --title "%s %s" --file "./attachments/%s/%s.html"  --parent "Gallery"  --noConvert --replace' % (parti, name, parid, name))
            res = system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action addAttachment --space "MAGP" --title "%s" --file "./attachments/%s/%s.json"' % (parti, parid, name))
            print e, res
        
        #save the relevant info
        pagetit0= "%s %s"%(parti,name)
        pagetit= pagetit0.replace(' ','+')
        data+= [(pagetit,parid,name)]
    return data
        

#1: define list of pages with attachments
parids={
        '14157333':'Graph examples',
        '14157255':'Axis examples',
        '14156352':'Subpage examples',
        '14156190':'Symbol examples',
        '14158405':'Contour examples',
        '14975221':'Coastlines examples',
        '14975310':'Legend examples',
        '15466537':'Wind examples'
}

system('rm -fr ./attachments/*')
[system('mkdir ./attachments/%s'%i) for i in parids.keys()]


#1.1 get pages info
res= system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action getPageList --space "MAGP" --file "pages.csv"')



#
##2: download the lists of elements form from confluence
#
system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action getAttachmentList --space "MAGP" --file "list.csv"')
f= file('list.csv','rb')
r= csv.reader(f,delimiter=',')
data= []
for row in r:
    title= row[0]
    parid= row[2]
    filen= row[3]
    if parid in parids.keys() and filen[-4:]=='json':
        print parid,title
        data.append(row)
f.close()
print '%d attachments selected'%len(data)

#2.1 download the relevant pages
jsonpages= []
for row in data:
    filen= row[3]
    title= row[0]
    parid= row[2]
    print 'Downloading', title, 
    res= system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action getAttachment --space "MAGP" --id "%s" --file "%s" > out'%(parid,filen))
    if res==0: res=system('mv %s ./attachments/%s'%(filen,parid))
    if res==0: print 'OK'
    else:      print 'KO'

#3: build the examples pages
examples= []
for row in data:
    filen= row[3]
    title= row[0]
    parid= row[2]
    parti= parids[parid]
    examples+= buildSinglePage(parid,parti,title)  

#5: build the gallery page
f= file('./gallery_template.html')
htemplate= f.read()
f.close()
lines=''
num= len(examples)
print 'Num of examples',num
for i in range(0,num):
    pagetit,parid,name= examples[i]
    if i%3==0: lines+='<tr>\n'
    lines+='\n\t<td><a href="/wiki/display/MAGP/%s">\n\t<img width="100%%" src="/wiki/download/attachments/%s/%s.png" alt="%s"/></a></td>'%(pagetit,parid,name,name) 
    if i%3==2: lines+='</tr>\n'
html= htemplate.replace('%TABLE_ROWS%',lines)
f= file('./gallery.html','w')
f.write(html)
f.close()

#6: update the gallery page
res= system('/home/graphics/cgjd/confluence-cli-2.5.0/confluence.sh --action addPage --space "MAGP" --title "Gallery" --file "./gallery.html"  --parent "Home"  --noConvert --replace')
if res==0:
    print 'Gallery updated OK'
else:
    print 'Error occurred while updating Gallery'

