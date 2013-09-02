#!/bin/sh

echo ""
echo " Please only use this script if you know what you are doing!!!"
echo ""
echo " This script can change operational use scripts!"
echo ""

for use_script in use.* sh.*
do
   diff -q ${use_script} /usr/local/share/ecmwf/use/
   if (($?!=0)) ; then
   
    ls -lrS ${use_script} /usr/local/share/ecmwf/use/${use_script}
    echo ""

    echo -n "You want to replace? [NO/yes/diff] "
    read answer
    
    if test "x${answer}" = xdiff ; then
      diff ${use_script} /usr/local/share/ecmwf/use/
      echo ""
      echo -n "You want to replace? [NO/yes] "
      read answer
    fi
    
    if test "x${answer}" = xyes ; then
      cp ${use_script} /usr/local/share/ecmwf/use/
      echo "File /usr/local/share/ecmwf/use/${use_script} has been replaced!"
    fi
    echo ""
   fi
done
