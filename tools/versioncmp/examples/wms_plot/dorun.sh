# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

set -A names wind wave image template wms_base_layer wms_data_layer wms_image_layer 
for name in ${names[*]}; do
  echo "processing $name"
  $MAGPLUS_HOME/bin/magjson $name.json
  mv map.png $name.png
done

