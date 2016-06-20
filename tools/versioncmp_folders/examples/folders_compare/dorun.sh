# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

version=$1 
echo "dorun.sh running version $version"

#version value is "instrumental", it has nothing to do with magics version
if [[ $version = "daily" ]]; then
  cp $compfolder1/*.ps .
  touch *.ps
else
  cp $compfolder2/*.ps .
  touch *.ps
fi

