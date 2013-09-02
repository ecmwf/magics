src_folder="/scratch/graphics/cgi/mpptestsuite/html/test/fortran"
data_folder="/scratch/graphics/cgi/mpp_os_tests"
cd $src_folder
files=`ls *.F`
cd - > /dev/null
for file in ${files[@]}; do
  folder=${file%\.*}
  mkdir -p $folder
  cp -f $src_folder/$file $folder/.
  script_file=$folder/dorun.sh
  cp -f dorun_template.sh $script_file
  sed -i "s/%FORTRAN_FILE%/$file/g" $script_file
  # do we need to copy over any data
  lines=`cat $src_folder/$file | grep -e INPUT_FILE_NAME -e data`
  for line in ${lines[@]}; do
    # strip out any unwanted characters
    line=`echo $line | sed -e "s/[\,\'()*]//g"`
    if [[ -f $data_folder/$line ]]; then
      mkdir -p $folder/data
      cp -f $data_folder/$line $folder/data/.
    fi

  done


done
