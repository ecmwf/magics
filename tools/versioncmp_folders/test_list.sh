# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#begin of modification - cgjd 26.07.2012
#new - search output images by name (ensure images are correctly compared)
# Utility to search an element in a list
search_list() {
echo "All Arguments to function: $*"
echo "First argument $1"
echo "Second argument $2"
echo "Third argument $3"
    elem= $1
    list= $2
    while [[ $iter -lt ${#list[*]} ]]; do
      if [[ elem = list[$iter] ]]; then return $iter; fi
      iter= $iter + 1
    done
    return -1
}
#end of modification - cgjd 26.07.2012

l[0]=uno
l[1]=dos
l[2]=tres

e=tres

res= search_list $e $l

echo $res, $l[$res]

