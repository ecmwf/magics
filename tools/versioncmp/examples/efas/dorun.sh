set -A names  efas_shading tempe efas  probRgt50
for name in ${names[*]}; do
  echo "processing $name"
  python $name.py
done

python precip.py EUEupsPr.txt
python precip.py EUEupsSnowmelt.txt
