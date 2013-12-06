set -A names  rotated
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

