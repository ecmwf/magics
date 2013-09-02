set -A names  traj
for name in ${names[*]}; do
  echo "processing $name"
 magmlx $name.magml
done

