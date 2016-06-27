#!/bin/bash
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


where_magics_is="/usr/local/apps/Magics"

mag_version[0]=${1:-"new++"}
mag_version[1]=${2:-"develop++"}   # newer version

#
#  CLEAN-UP
#
rm -rf ps png svg

current_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
current_MAGPLUS_HOME=$MAGPLUS_HOME

mkdir -p logs

##############################################################
#
#    FUNCTIONS to run tests
#
##############################################################
function generate_plot_fortran {
  echo Do FORTRAN: ${2}
  ${where_magics_is}/${1}/bin/magics-config --compile=fortran/${2}.f
  ./${2} DEVICE PS_PNG_SVG
  \rm -f ${2} ${2}.o
  mv ${2}.ps ps/${1}/
  mv ${2}*.png png/${1}/
  mv ${2}*.svg svg/${1}/
}

function generate_plot_json {
  echo Do JSON: ${2}
  ${where_magics_is}/${1}/bin/magjson -timeout=0 -legend=nolegend -width=1423 -height=591 json/${2}.json
  mv ${2}.png png/${2}_${1}.png
}


##############################################################
#
#    RUNNING the tests
#
##############################################################
mkdir ps png svg

for versions in "${mag_version[@]}"; do
  echo ""
  echo "Start ${versions}"
  echo ""
  export MAGPLUS_HOME=${where_magics_is}/${versions}
  export LD_LIBRARY_PATH=${MAGPLUS_HOME}/lib:$LD_LIBRARY_PATH

  mkdir ps/${versions} png/${versions} svg/${versions}
  #
  # FORTRAN
  #
  echo " Start Fortran"
  for i in $( ls fortran/); do
	echo "    $i"
	ext="f"
	generate_plot_fortran ${versions} ${i%.${ext}} &> logs/fortran_${i}_${versions}.log
  done

  #
  # JSON
  #
  echo " Start JSON"
  if [ -f ${where_magics_is}/${versions}/bin/magjson ]; then
    for i in $( ls json/); do
	  echo "    $i"
	  ext="json"
	  generate_plot_json ${versions} ${i%.${ext}} &> logs/json_${i}_${versions}.log
    done
  else
    echo ""
    echo "           JSON support not available in version ${versions} ${where_magics_is}/${1}/bin/magjson"
    echo ""
  fi

done

#
#  CLEAN-UP
#
export LD_LIBRARY_PATH=$current_LD_LIBRARY_PATH
export MAGPLUS_HOME=$current_MAGPLUS_HOME
