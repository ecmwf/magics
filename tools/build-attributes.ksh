
#!/bin/ksh
src="../src/params"
target="../src/attributes"
list=${src}/*.xml



while getopts "h:i:a" arg; do
  case $arg in
    h)
      echo "usage" 
      ;;
    i)
      list=${src}/$OPTARG.xml
      echo ${list}
      ;;
    a)
      list=${src}/*.xml
      echo ${list}
      ;;
  esac
done

for i in `ls ${list}`
do
echo  $i
python3 xml2cc_mv.py .  $i ${target}
python3 xml2cc.py .  $i ${target}
done
