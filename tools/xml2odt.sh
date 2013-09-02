#!/bin/bash
set -x
#
# to be run from src/xml/Makefile
#
input=${1}
to=../${2}
dir=`dirname $0`
odt_template=${dir}/ParaTemplate.odt
script=${dir}/xml2odt.py
tmp_dir=odt_tmp
from=./

i=`basename ${input} .xml`
new_name="${to}/${i}_Parameter.odt"

echo "Generate from ${input} the table in ${new_name}"

#
#  UnZip the template
#
unzip ${odt_template} -d ${tmp_dir}

#
#
#
rm -f ${tmp_dir}/content.xml
python ${script} ${input} ${tmp_dir}/content.xml

#
#  Zip up the new parameter file
#
cd ${tmp_dir}
zip -0 -X ${new_name} mimetype && zip -r ${new_name} * -x mimetype
  
#
#  Clean-up
#
cd ..
rm -rf ${tmp_dir}
