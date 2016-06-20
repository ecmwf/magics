# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

 if [[ $1 = '' || $2 = '' ]]; then
	echo "ERROR. Arguments missing: please specify the folders to compare"
	exit 1
 fi 

export compfolder1=$1
export compfolder2=$2

#version values "daily" and "new++" are conventional, they have nothing to do with the folder comparison
./versioncmp.sh daily new++ ./examples/folders_compare/dorun.sh -output ./output

