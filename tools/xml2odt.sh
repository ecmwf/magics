#!/bin/bash
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

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
