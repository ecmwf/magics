set -A names  example1
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

