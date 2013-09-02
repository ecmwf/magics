#!/bin/bash

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
