#!/bin/bash
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


logfile=$0_.log
echo "Log is written in ${logfile}"
exec > $logfile 2>&1
set -x

EMOS_VERSION=000380

rpmspec=EmosLib.spec

#
#  generate SPEC file with emos version number
#
sed "s/000xxx/${EMOS_VERSION}/g" ${rpmspec}.in > ${rpmspec}

#
#   Prepare tarball in right format
#
tar -xzf emos_${EMOS_VERSION}.tar.gz
\mv emos_${EMOS_VERSION} emos-${EMOS_VERSION}
# correct link options for 64bit
olds="DEBUG ="
news="DEBUG = -fPIC "
chmod +w emos-${EMOS_VERSION}/config/config.linux_gfortranR64A64.in
sed "s/${olds}/${news}/g" emos-${EMOS_VERSION}/config/config.linux_gfortranR64A64.in > temp.in
\mv -f temp.in emos-${EMOS_VERSION}/config/config.linux_gfortranR64A64.in
tar -cjf emos-${EMOS_VERSION}.tar.bz emos-${EMOS_VERSION}
\rm -rf emos-${EMOS_VERSION}

#
#  SRC RPM
#
rpmbuild --nodeps --buildroot=${PWD}/_rpm -bs \
	--define="_rpmdir ${PWD}"  --define="_srcrpmdir ${PWD}" --define="_sourcedir ${PWD}" \
	--define="_specdir ${PWD}" --define="_builddir ${PWD}" \
	${rpmmacros} ${rpmspec}

#
#  BIN RPM
#
rpmbuild --nodeps --buildroot=${PWD}/_rpm -ba \
	--define="_rpmdir ${PWD}"  --define="_sourcedir ${PWD}" \
	--define="_specdir ${PWD}" --define="_builddir ${PWD}" \
	${rpmmacros} ${rpmspec}

\rm -rf emos-000380.tar.bz emos-000380 ${rpmspec}
