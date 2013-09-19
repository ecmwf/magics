
#!/usr/bin/ksh
here=`pwd`
print `basename $here`
dir=`basename $here`

for f in `ls`;do
   get="/tmp/cgs/naturalearth/${dir}_physical/ne_${f}"
   if [ ! -f $get ];
   then
     echo "Error $get does not exists!"
    else
     echo "$get found!"
     cp $get $f
   fi
done                  
