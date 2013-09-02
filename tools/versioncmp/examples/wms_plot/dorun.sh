set -A names wind wave image template wms_base_layer wms_data_layer wms_image_layer 
for name in ${names[*]}; do
  echo "processing $name"
  $MAGPLUS_HOME/bin/magjson $name.json
  mv map.png $name.png
done

