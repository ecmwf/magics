#!/bin/bash
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#scriptdir=`dirname ${0}`
#scriptdir=`(cd ${scriptdir}; pwd)`
#scriptname=`basename ${0}`
tostripdir=`dirname "$1"`
tostripfile=`basename "$1"`

set -e

if [ -z ${tostripfile} ] ; then
#  echo "USAGE ${scriptname} <tostrip>"
  echo "to-be-striped file must be specified"
  exit 1
fi

cd "${tostripdir}"

debugfile="${tostripfile}.debug"

echo "stripping ${tostripfile}, putting debug info into ${debugfile}"
#objcopy --only-keep-debug "${tostripfile}" "${debugfile}"
cp "${tostripfile}" "${debugfile}"
#strip --strip-debug --strip-unneeded "${tostripfile}"
objcopy --strip-debug "${tostripfile}"
objcopy --add-gnu-debuglink="${debugfile}" "${tostripfile}"
chmod -x "${debugfile}"
