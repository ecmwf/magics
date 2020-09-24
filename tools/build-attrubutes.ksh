
#!/bin/ksh
src="../src/params"
target="../src/attributes"
for i in `ls ${src}/*.xml`
do
echo  $i
python xml2cc_mv.py .  $i ${target}
python xml2cc.py .  $i ${target}
done
