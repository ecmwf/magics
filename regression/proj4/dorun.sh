set -A names polar_north bonne  collignon geos  lambert  lambert_subarea  mollweide  mercator sub_mercator
for name in ${names[*]}; do
  echo "processing $name"
  $MAGPLUS_HOME/bin/magjson $name.json
done

