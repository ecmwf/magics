#!/bin/ksh
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


######################################################
#
# Usage:
#
# versioncmp.sh <version1> <version2> <script.sh> 
#
# NB the script should not set any env variables or
# make any "use <program>" calls.
#
# Script should produce a single image output which
# will then be compared across versions.
#
# If an invalid version is input, the range of available
# versions is shown to the user.
#
######################################################

# Error codes for various scenarios
CODE_OKAY=0
CODE_FAIL=1
CODE_MISSING=2
CODE_ERRORS=3
CODE_MAJOR_DIFFERENCE=4
CODE_MINOR_DIFFERENCE=5
CODE_BAD_REQUEST=6

set -A return_strings
return_strings[$CODE_OKAY]='OK'
return_strings[$CODE_FAIL]='Script_error'  
return_strings[$CODE_MISSING]='Missing output image(s)'
return_strings[$CODE_ERRORS]='Check_error_output'
return_strings[$CODE_MAJOR_DIFFERENCE]='Significant_image_difference'  
return_strings[$CODE_MINOR_DIFFERENCE]='Minor_image_difference'  
return_strings[$CODE_BAD_REQUEST]='Requested_version(s)_did_not_exist'  

WARN_CODE_OKAY=0
WARN_CODE_DIFFERENT=1
WARN_CODE_CHECK=2
WARN_CODE_ERROR=3

set -A warning_tags
warning_tags[$WARN_CODE_OKAY]="okay"
warning_tags[$WARN_CODE_DIFFERENT]="different"
warning_tags[$WARN_CODE_CHECK]="check"
warning_tags[$WARN_CODE_ERROR]="error"

#The path to the current script
SCRIPTDIR=`dirname ${0%/*}`
#SCRIPTDIR=/var/tmp/cgjd/perforce/magics/tools/versioncmp/
verbose=0
force_overwrite=0
output_folder="$SCRIPTDIR/output"          

# The location of imagemagick
dummy=`which compare`
image_magick_loc=${dummy%/*}
pdiff_loc="$SCRIPTDIR/pdiff/perceptualdiff"

image_stub="image"
diff_stub="diff"
image_ext="png"

errorcode=$CODE_OKAY

# we will work out whether a significant difference
# has been detected and report if so
report_difference="none"
#difference_threshold=50   # in pixels
difference_threshold_factor=0.05  # in percent

LOG() {
  if [[ $verbose -eq 1 ]]; then
    echo $1
  fi
}

get_warning_code() {
  typeset warning_code
  warning_level=0
  case $1 in
    $CODE_OKAY)
      warning_level=$WARN_CODE_OKAY;;
    $CODE_MINOR_DIFFERENCE)
      warning_level=$WARN_CODE_DIFFERENT;;
    $CODE_MAJOR_DIFFERENCE|$CODE_ERRORS)
      warning_level=$WARN_CODE_CHECK;;
    $CODE_FAIL|$CODE_MISSING|$CODE_BAD_REQUEST)
      warning_level=$WARN_CODE_ERROR;;
    *)
      # I don't know what happened if we're here so assign to max severity
      warning_level=$WARN_CODE_ERROR;;
  esac
  return $warning_level
}


RETURN() {
  # exits normally and writes error status information
  errorcode=$1
  # check for out of bounds
  if [[ $errorcode -gt ${#return_strings[*]} ]]; then
    errorcode=1  # a script error
  fi
  rtntext=`echo ${return_strings[$errorcode]} | tr "_" " "` 
  LOG "\"$rtntext\""

  echo $rtntext > $output_folder/report.txt
  get_warning_code $errorcode
  warning_level=$?

  # TODO return a severity code according to what the problem was
  echo $warning_level > $output_folder/warning.txt
  exit 0
}

# Utility to report errors and close down with message
ERROR() {
    echo "ERROR: $1"
}

FAIL() {
   if [[ ! $1 = '' ]]; then
     ERROR "$1"
   fi
   RETURN $CODE_FAIL
}

RUN() {
  # runs a command in verbose mode or hides output
  if [[ $verbose -eq 1 ]]; then
     $1
   else
     $1 > /dev/null 2> /dev/null
  fi
}

# Checks whether a subfolder exists within a root folder
# check_subfolder <rootfolder> <subfolder>
check_subfolder() {
  if [[ $1 = '' || $2 = '' ]]; then
    FAIL "Internal error. Exiting."
  fi

  test_folder=$1/$2

  if [[ ! -d $test_folder ]]; then
    ERROR "Required folder $test_folder does not exist."
    ERROR "Existing folders are:"
    for i in `ls $1`; do
      echo "$i"
    done
    RETURN $CODE_BAD_REQUEST
  fi
}

# Test whether a filename is an image
is_image() {
  set -A extensions png jpg jpeg ps png tif gif
  filename=$1
  ext=${filename##*.}
  LOG "Testing $filename"
  LOG "has extension $ext"
  for e in ${extensions[@]}; do
      if [[ $ext = $e ]]; then
          LOG "$filename is an image file"
          return 0
      fi
  done
  LOG "$filename is not an image file."
  return 1
}

#
# Begin main script here
#

if [[ $1 = '' || $2 = '' ]]; then
    FAIL "Need to specify two version numbers.";  
fi

if [[ $3 = '' ]]; then
  FAIL "No input script specified."
fi

version1=$1
version2=$2

# look for optional arguments
arg_count=$#
if [[ arg_count -gt 3 ]]; then
  count=3
  set -A argvec $*
  while [[ $count -lt $arg_count ]]; do
    case ${argvec[$count]} in
      "-v"|"--verbose"|"-verbose")
        verbose=1;;
      "-force"|"--force"|"-f")
        force_overwrite=1;;
      "-threshold-factor"|"--threshold-factor"|"-t")
        difference_threshold_factor=${argvec[(($count+1))]};
        count=$count+1;;
#      "-threshold"|"--threshold"|"-t")
#        difference_threshold=${argvec[(($count+1))]};
#        count=$count+1;;
      "-output"|"--output"|"-o")
        output_folder=${argvec[(($count+1))]};
        count=$count+1;;
      *)
      FAIL "Unknown option '${argvec[$count]}'";;
    esac
    count=$count+1
  done
fi


# Check these versions exist in the root folder
magics_root=`cd $MAGPLUS_HOME;cd ..;pwd`

check_subfolder $magics_root $version1
check_subfolder $magics_root $version2

the_script=$3
script_path=${the_script%/*}
script_file=${the_script##*/}

#begin of modification - cgjd 29.03.2012
#new - switch on/off automatic text in fortran programs
if [[ $MAGICS_TEST_AUTOTEXT = off ]];then
  mkdir ./temp
  cp $script_path/*.F ./temp 
  sed -i "s/\(.*\)CALL  *POPEN.*/&\n\1CALL PSETC ('PAGE_ID_LINE','OFF')/g" $script_path/*.F
fi
#end of modification - cgjd 29.03.2012


if [[ ! -f $the_script ]]; then
  FAIL "Script $the_script does not exist."
fi
LOG "---------------------------------------------------------------"
LOG "Running $the_script with Magics versions $version1 and $version2 "
LOG "The script is running from the folder $SCRIPTDIR"
LOG "Output folder $output_folder"
LOG "Script folder $script_folder"
LOG "---------------------------------------------------------------"

set -A version_array $version1 $version2

#original_path=/usr/local/bin:/usr/bin:/bin

#begin of modification - cgjd 03.12.2012
#new - add odb library path (ODB version is harcoded, this may be changed in future!!!!)
odb_lib_path=/usr/local/apps/odb_api/0.9.24/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$odb_lib_path
#end of modification - cgjd 03.12.2012


original_lib_path=$LD_LIBRARY_PATH
original_path=$PATH
original_magplus_home=$MAGPLUS_HOME
original_magpluslib_shared=$MAGPLUSLIB_SHARED
original_magpluslib_shared_double=$MAGPLUSLIB_SHARED_DOUBLE
original_magpluslib_static=$MAGPLUSLIB_STATIC
original_magpluslib_static_double=$MAGPLUSLIB_STATIC_DOUBLE
#begin of modification - cgjd 10.04.2012
#new - save Python packages path
original_pythonpath=$PYTHONPATH
#end of modification - cgjd 10.04.2012

#begin of modification - cgjd 27.07.2012
#new
last_output_path=None
#end of modification - cgjd 27.07.2012

# Check for the presence of an output folder
if [[ -d $output_folder && $force_overwrite -ne 1 ]]; then
  print -n "Output folder $output_folder/ already exists. Overwrite? (y/N): "; read var
  if [[ $var != 'y' ]]; then
    RETURN $CODE_FAIL
  fi
  echo NB adding argument '-force' overwrites automatically.
fi


rm -rf $output_folder 
mkdir $output_folder 
cd $output_folder 
RUN "cd -" 

# sort out the html comparison file
output_URL=$output_folder/index.html
cp $SCRIPTDIR/config/codes.css $output_folder/.
cp $SCRIPTDIR/config/comp.html $output_URL

set -A images
set -A imagesizes
set -A runtimes

count=0

#
# Here is the loop over versions:
# 

while [ $count -lt ${#version_array[*]} ]; do

  # create an array of images for each version
  set -A images[$count]
  set -A imagesizes[$count]

  version=${version_array[$count]}
  LOG "Running version $version"

  magics_lib_path=`$magics_root/$version/bin/magics-config --libdir`
  #export LD_LIBRARY_PATH=$magics_lib_path:$original_lib_path
  magics_libs=`$magics_root/$version/bin/magics-config --libs`
  LOG "Adding Magics lib path to LD_LIBRARY_PATH: $magics_lib_path"
  LOG "Adding Magics libs to LD_LIBRARY_PATH: $magics_libs"
  export LD_LIBRARY_PATH=$magics_libs:$magics_lib_path:$original_lib_path
  LOG "LD_LIBRARY_PATH now $LD_LIBRARY_PATH"
  LOG "Adding Magics bin folder to PATH: $magics_root/$version/bin"
  export PATH=$magics_root/$version/bin:$original_path
  LOG "PATH now $PATH"

  export MAGPLUSLIB_SHARED=`$magics_root/$version/bin/magics-config --f90shared`
  export MAGPLUSLIB_SHARED_DOUBLE=`$magics_root/$version/bin/magics-config --f90shared --double`
  export MAGPLUSLIB_STATIC=`$magics_root/$version/bin/magics-config --f90static`
  export MAGPLUSLIB_STATIC_DOUBLE=`$magics_root/$version/bin/magics-config --f90static --double`
  export MAGPLUS_HOME=$magics_root/$version
  LOG "Set MAGPLUS_HOME=$MAGPLUS_HOME"
  LOG "Set MAGPLUSLIB_SHARED=$MAGPLUSLIB_SHARED"
  LOG "Set MAGPLUSLIB_SHARED_DOUBLE=$MAGPLUSLIB_SHARED_DOUBLE"
  LOG "Set MAGPLUSLIB_STATIC=$MAGPLUSLIB_STATIC"
  LOG "Set MAGPLUSLIB_STATIC_DOUBLE=$MAGPLUSLIB_STATIC_DOUBLE"

#begin of modification - cgjd 10.04.2012
#new - set Python packages to Magics version
  export PYTHONPATH=/usr/local/apps/Magics/$version/lib/python2.7/site-packages
  LOG "Set PYTHONPATH=$PYTHONPATH"
#end of modification - cgjd 10.04.2012

  # TODO set the linker and compile options here
  rm -rf $output_folder/$version
  mkdir -p $output_folder/$version

  LOG "Changing folder to $output_folder/$version"
  cd $output_folder/$version
  output_path=`pwd`
  LOG "Output folder $output_path"

  RUN "cd -" 
  cd $script_path

  touch ".tmp"

  LOG "Running script now"
  start_ns=`date +%s%N`
  ksh $script_file $version > $output_path/output.txt 2> $output_path/errors.txt
  end_ns=`date +%s%N`

  (( time_ms=(end_ns-start_ns)/1000000 ))
  LOG "Script execution time: $time_ms ms"
  runtimes[$count]=$time_ms

  script_error_out=`cat $output_path/errors.txt`
  if [[ ${#script_error_out} -gt 0 ]]; then
    LOG "Script error output: $version:"
    if [[ $verbose -eq 1 ]]; then
      cat $output_path/errors.txt
    fi
  fi

#begin of modification cgjd 2012.04.30
#new - save grib files
#  mv output $image_stub/output.grib
#end of modification cgjd 2012.04.30
  
  psfiles=`find . -newer .tmp -name "*.ps" -o -name "*.eps"`
  LOG "Converting the following PostScript files"
  LOG ${psfiles}
  pscount=0
  for f in ${psfiles}; do
#begin of modification cgjd 2012.04.26
#old
#   pstopnm $f 
#new - set image resolution to higher value
   pstopnm -dpi=100 $f 
#end of modification cgjd 2012.04.26
    psstub=${f##.*}
    ppmcount=0
    for ppm in `ls ${psstub}*.ppm`; do
      countstr=`printf %02d-%02d $pscount $ppmcount`
#begin of modification cgjd 2012.04.26
#old
     pnmtopng $ppm > $image_stub$countstr.$image_ext
#new - set image resolution to higher value
#     pnmtopng -force $ppm > $image_stub$countstr.$image_ext
#end of modification cgjd 2012.04.26
      ((ppmcount=ppmcount+1))
    done
    rm -f *.ppm
   ((pscount=pscount+1))
  done

  # pdfs now
  pdffiles=`find . -newer .tmp -name "*.pdf"`
  LOG "Converting the following PDF files"
  LOG ${pdffiles}
  pdfcount=0
  for f in ${pdffiles}; do
     pdfstub=${f##.*}
     pdftoppm -png $f 
     pngcount=0
     for png in `ls ${psstub}*.png`; do
        countstr=`printf %02d-%02d $pdfcount $pngcount`
        echo  "PDF countstr" $countstr
        mv $png $image_stub$countstr.$image_ext
        ((pngcount=pngcount+1))
     done
     ((pdfcount=pdfcount+1))
  done

  # now the files have been converted in-place, we can copy them over

  files=`find . -type f -newer .tmp | sort`
  LOG "The following files were modified by the script:"
  LOG ${files}

  rm -f .tmp
  image_count=0

  for f in ${files}; do

    number_string=`printf %02d $image_count`

    # extract path
    file=${f##*/}
    ext=${file##*.}
    case $ext in
      "")
      LOG "Do nothing" > /dev/null;;
      $file)
      LOG "Do nothing" > /dev/null;;
      "o")
      LOG "Do nothing" > /dev/null;;
      "ps")
      # These have already been converted
      LOG "Do nothing" > /dev/null;;
      *)
      is_image $f
      if [[ $?  -eq 0 ]]; then;
	  ##begin ccgjd 27.07.2012
          #cp $f $output_path/$image_stub$number_string.$ext;
          #output_file=$output_path/$image_stub$number_string.$ext
	  output_file=$output_path/$file
          cp $f $output_file
	  ##end ccgjd 27.07.2012
          images[$count][${#images[$count][*]}]=$output_file
          imagesizes[$count][${#imagesizes[$count][*]}]=`ls -sh $output_file | awk '{ print $1 }'`
          ((image_count=image_count+1))
      else;
          # we take a copy for completeness
          cp $f $output_path/$file
      fi;;

    esac

    # now delete the file (keeps things clean)
    LOG "Removing file $f"
    rm -f $f
  done
  LOG "Done version $version"
  RUN "cd -" 

  last_output_path=$output_path

  ((count=count+1))

done

# Do an image diff between files in both folders with the same name
iter=0
max_image_height=0
# collect pixel difference info and timer info
# to write a Javascript map object in out html output file
pixel_map_str=""
page_codes_str=""
summary_file_str=""


diff_types_str="\"diff\": \"ImageMagick\", \"pdiff\": \"Perceptual Diff\""
# construct Javascript maps for the sizes of the files
l_size_map_str=""
r_size_map_str=""

typeset -i image_diff
typeset -i image_pdiff
image_diff=0
image_pdiff=0

LOG "length of images 0 is ${#images[0][*]}"
LOG "length of images 1 is ${#images[1][*]}"


#begin of modification - cgjd 26.07.2012
#old
#while [ $iter -lt ${#images[0][*]} ]; do

#new - search output images by name (ensure images are correctly compared)
iter=0
iter0=0
while [[ $iter0 -lt ${#images[0][*]} ]]; do
  image0=`basename ${images[0][$iter0]}`
  path0=`dirname ${images[0][$iter0]}`
  path1=$path0
  iter1=0
  found="0"
  
while [[ $iter1 -lt ${#images[1][*]} ]]; do
  image1=`basename ${images[1][$iter1]}`
  path1=`dirname  ${images[1][$iter1]}`

if [[ $image0 = $image1 ]]; then
    found="1"
    echo image $iter: $image0

    number_string=`printf %02d $iter`
    cp ${images[0][$iter0]} $path0/$image_stub$number_string.$image_ext;
    cp ${images[1][$iter1]} $path1/$image_stub$number_string.$image_ext;

#end of modification - cgjd 26.07.2012

    $image_magick_loc/compare -metric AE -dissimilarity-threshold 1 ${images[0][$iter0]} ${images[1][$iter1]} $output_folder/$diff_stub$number_string.$image_ext 2> $output_folder/$diff_stub$number_string.txt
    
    this_page_errorcode=$CODE_OKAY

    typeset -i this_width
    typeset -i this_height
    this_width=1
    this_height=1

    if [[ -f $output_folder/$diff_stub$number_string.$image_ext ]]; then

      image_diff=`cat $output_folder/$diff_stub$number_string.txt`
      LOG "Absolute difference between images=$image_diff pixels" 

      # prepare a string to add html radio button
      diff_types_str="\"diff\": \"ImageMagick\""


      # ONLY if non-zero difference do a perceptual diff:
      if [[ $image_diff -gt 0 ]]; then

        # now do perceptual diff (90 degree fov for "face-in-the-screen"
        # perceieved difference)
        $pdiff_loc/perceptualdiff -verbose -fov 90 ${images[0][$iter0]} ${images[1][$iter1]} -output $output_folder/p$diff_stub$number_string.$image_ext | awk '{ print $1 }' | tail -2 > $output_folder/p$diff_stub$number_string.txt
	
        image_pdiff=`cat $output_folder/p$diff_stub$number_string.txt`
      
        # add to the string that will put a radio button on the html output
        diff_types_str+=", \"pdiff\": \"Perceptual Diff\""

      else
        image_pdiff=0
      fi
      
      LOG "Perceived difference between images=$image_pdiff pixels" 

      # get image height using imagemagick
      set -A image_info `identify $output_folder/$diff_stub$number_string.$image_ext`
      dimensions=${image_info[2]}
      this_width=${dimensions%x*}
      this_height=${dimensions##*x}
      if [[ $this_height -gt $max_image_height ]]; then
        max_image_height=$this_height
      fi

      # now look for whether this image difference is above the threshold in
      # terms of percentage of different pixels
      typeset -i total_pixels
      total_pixels=$this_width*$this_height
      tolerance=$difference_threshold_factor*$total_pixels

      if [[ $image_diff -gt $tolerance ]]; then
        report_difference="major"
        this_page_errorcode=$CODE_MAJOR_DIFFERENCE
      elif [[ $image_diff -gt 0  ]]; then
        this_page_errorcode=$CODE_MINOR_DIFFERENCE
        if [[ $report_difference != "major" ]]; then
            # we report the worst case in the event of more than one image
            report_difference="minor"
          fi
      fi

    else
      this_page_errorcode=$CODE_MISSING
    fi

    # construct the pixelmap string
    if [[ $iter -ne "0" ]]; then
        #pixel_map_str="$pixel_map_str, ";
        pixel_map_str+=", ";
        l_size_map_str+=", ";
        r_size_map_str+=", ";
        page_codes_str+=", ";
        summary_file_str+="\n";
    fi

    #pixel_map_str="$pixel_map_str\"$number_string\": $image_diff";
    typeset -i diff_pc
    typeset -i pdiff_pc
    typeset -i total_pixels
    total_pixels=$this_width*$this_height

    diff_pc=100*$image_diff/$total_pixels
    pdiff_pc=100*$image_pdiff/$total_pixels

    if [[ $image_diff -gt 0 ]]; then
      pixel_map_str+="\"$number_string\": \"$image_diff ($diff_pc\%), $image_pdiff ($pdiff_pc\%) discernable\""
    else
      pixel_map_str+="\"$number_string\": \"$image_diff ($diff_pc\%)\""
    fi
    l_size_map_str+="\"$number_string\": \"${imagesizes[0][$iter]}\"";
    r_size_map_str+="\"$number_string\": \"${imagesizes[1][$iter]}\"";
    
    if [[ $image_diff -eq 0 ]]; then
      summary_file_str+="$number_string: Identical"
    else
      summary_file_str+="$number_string: Difference $image_diff ($diff_pc\%), $image_pdiff ($pdiff_pc\%) discernable pixels"
    fi

    # get the warning code according to this error code
    get_warning_code $this_page_errorcode
    this_page_warning_level=$?
    page_codes_str+="\"${warning_tags[$this_page_warning_level]}\""
   ((iter=iter+1))

#begin of modification - cgjd 26.07.2012
#old
#done
#new
fi
((iter1=iter1+1))
done #for each each images[1]

if [[ $found -eq "0" ]]; then
    number_string=`printf %02d $iter`;
    cp ${images[0][$iter0]} $path0/$image_stub$number_string.$image_ext;
    cp $SCRIPTDIR/config/missing.png $last_output_path/$image_stub$number_string.$image_ext;
    cp $SCRIPTDIR/config/missing.png $last_output_path/$image_stub$number_string.$image_ext;
    cp $SCRIPTDIR/config/missing.png $output_folder/p$diff_stub$number_string.$image_ext
    cp $SCRIPTDIR/config/missing.png $output_folder/$diff_stub$number_string.$image_ext

   ((iter=iter+1))
fi

((iter0=iter0+1))
done #for each each images[0]
#end of modification - cgjd 26.07.2012



#begin of modification cgjd 2012.06.21
#new - remove "uncompressed" ppm images
#filesppm=`find $output_folder -type f -name "*.ppm"`
#for f in ${filesppm}; do
#  rm -f $f
#done
#end of modification cgjd 2012.06.21

# write the summary text to file
echo $summary_file_str > $output_folder/summary.txt

set -A text_file_type "output" "errors"

for file_type in ${text_file_type[*]}; do

  # Do a text diff between the output with side-by-side view
  typeset -i col_w1
  typeset -i col_w2
  # the column widths should be the same as the widths of the input files
  col_w1=`wc -L $output_folder/$version1/$file_type.txt | awk '{ print $1 }'`
  col_w2=`wc -L $output_folder/$version2/$file_type.txt | awk '{ print $1 }'`

  if [[ $col_w1 -gt $col_w2 ]]; then
    let width=$col_w1+$col_w1+10
  else
    let width=$col_w2+$col_w2+10
  fi

  diff -W$width -y $output_folder/$version1/$file_type.txt $output_folder/$version2/$file_type.txt | expand > $output_folder/${file_type}_${diff_stub}.txt

  typeset -i diff_width
  diff_width=`wc -L $output_folder/${file_type}_${diff_stub}.txt | awk '{ print $1 }'`
  let col2_start=$diff_width-$col_w2+1

  echo "doing cut on $output_folder/$version1/$file_type.txt  $output_folder/$version2/$file_type.txt"
  diff -W$width -y $output_folder/$version1/$file_type.txt $output_folder/$version2/$file_type.txt | expand | cut -c1-$col_w1 > $output_folder/${file_type}_${diff_stub}_left.txt
  echo "done"
  echo "doing cut on $output_folder/$version1/$file_type.txt $output_folder/$version2/$file_type.txt"
  diff -W$width -y $output_folder/$version1/$file_type.txt $output_folder/$version2/$file_type.txt | expand | cut -c${col2_start}-${diff_width} > $output_folder/${file_type}_${diff_stub}_right.txt
  echo "done"

done

#begin of modification cgjd 2012.04.02
#new - highlight differences in output and error output
diff -awB $output_folder/output_${diff_stub}_left.txt $output_folder/output_${diff_stub}_right.txt > $output_folder/output_${diff_stub}_diff.txt
diff -awB $output_folder/errors_${diff_stub}_left.txt $output_folder/errors_${diff_stub}_right.txt > $output_folder/errors_${diff_stub}_diff.txt
python $SCRIPTDIR/difftext.py $output_folder/output_${diff_stub}_left.txt $output_folder/output_${diff_stub}_right.txt $output_folder/output_${diff_stub}_diff.txt
python $SCRIPTDIR/difftext.py $output_folder/errors_${diff_stub}_left.txt $output_folder/errors_${diff_stub}_right.txt $output_folder/errors_${diff_stub}_diff.txt
rm -f $output_folder/output_${diff_stub}_diff.txt
rm -f $output_folder/errors_${diff_stub}_diff.txt
#end of modification cgjd 2012.04.02

LOG "Writing html file..."

#begin of modification cgjd 2012.12.05
#old
#sed -i "s/%VERSION1%/$version1/g" $output_URL
#sed -i "s/%VERSION2%/$version2/g" $output_URL
#new - variables defined for the compared folders
folder1=`basename $compfolder1`
folder2=`basename $compfolder2`
sed -i "s/%TITLE%/Folder $folder1 vs $folder2/g" $output_URL
sed -i "s/%VERSION1%/$folder1/g" $output_URL
sed -i "s/%VERSION2%/$folder2/g" $output_URL
#end of modification cgjd 2012.12.05
sed -i "s/%PATHIMAGE1%/$version1/g" $output_URL
sed -i "s/%PATHIMAGE2%/$version2/g" $output_URL
sed -i "s/%PATHIMAGEDIFF%/./g" $output_URL
sed -i "s/%IMAGEEXT%/$image_ext/g" $output_URL
sed -i "s/%IMAGESTUB%/$image_stub/g" $output_URL
sed -i "s/%IMAGEDIFFSTUB%/$diff_stub/g" $output_URL
sed -i "s/%PAGE_COUNT%/$iter/g" $output_URL
sed -i "s/%PAGE_CODES%/$page_codes_str/g" $output_URL
sed -i "s/%PIXELMAP%/$pixel_map_str/g" $output_URL
sed -i "s/%TIMEIMAGE1%/${runtimes[0]}/g" $output_URL
sed -i "s/%TIMEIMAGE2%/${runtimes[1]}/g" $output_URL
 
sed -i "s/%LIMAGESIZEMAP%/$l_size_map_str/g" $output_URL
sed -i "s/%RIMAGESIZEMAP%/$r_size_map_str/g" $output_URL

sed -e "/%OUTPUTDIFF1%/r $output_folder/output_${diff_stub}_left.txt" -e "/%OUTPUTDIFF1%/d" $output_URL > $output_folder/.tmp.html; mv -f $output_folder/.tmp.html $output_URL
sed -e "/%OUTPUTDIFF2%/r $output_folder/output_${diff_stub}_right.txt" -e "/%OUTPUTDIFF2%/d" $output_URL > $output_folder/.tmp.html; mv -f $output_folder/.tmp.html $output_URL

sed -e "/%ERRORDIFF1%/r $output_folder/errors_${diff_stub}_left.txt" -e "/%ERRORDIFF1%/d" $output_URL > $output_folder/.tmp.html; mv -f $output_folder/.tmp.html $output_URL
sed -e "/%ERRORDIFF2%/r $output_folder/errors_${diff_stub}_right.txt" -e "/%ERRORDIFF2%/d" $output_URL > $output_folder/.tmp.html; mv -f $output_folder/.tmp.html $output_URL

# write the diffs that are available
sed -i "s/%DIFF_TYPES%/$diff_types_str/g" $output_URL

# we don't need these files anymore
rm -f $output_folder/output_${diff_stub}_left.txt
rm -f $output_folder/output_${diff_stub}_right.txt
rm -f $output_folder/errors_${diff_stub}_left.txt
rm -f $output_folder/errors_${diff_stub}_right.txt
rm -f $output_folder/output_${diff_stub}.txt
rm -f $output_folder/errors_${diff_stub}.txt

image_height=$max_image_height

# we have limits...
if [[ $image_height -gt 500 ]]; then
  image_height=500
fi

sed -i "s/%IMAGEHEIGHT%/$image_height/g" $output_URL
# also set the height of the text area so it fits the screen
# again within a limit so casual inspection of the results are possible
text_height=$(( 750-$image_height ))
if [[ $text_height -lt 250 ]]; then
  text_height=250
fi

sed -i "s/%TEXTHEIGHT%/$text_height/g" $output_URL

# Reset original environment variables
LOG "Resetting original values for modified environment variables"

export LD_LIBRARY_PATH=$original_lib_path
export PATH=$original_path
export MAGPLUS_HOME=$original_magplus_home
export MAGPLUSLIB_SHARED=$original_magpluslib_shared
export MAGPLUSLIB_SHARED_DOUBLE=$original_magpluslib_shared_double
export MAGPLUSLIB_STATIC=$original_magpluslib_static
export MAGPLUSLIB_STATIC_DOUBLE=$original_magpluslib_static_double
#begin of modification - cgjd 10.04.2012
#new - reset Python packages path
export PYTHONPATH=$original_pythonpath
#end of modification - cgjd 10.04.2012

# Report failure if some images were not produced
if [[ ${#images[0][*]} -eq 0 || ${#images[1][*]} -eq 0 ]]; then
  ERROR "Some images were not produced in the test"
  RETURN $CODE_MISSING
fi

if [[ $report_difference = "major" ]]; then
  echo "Significant difference found between versions (>$difference_threshold_factor factor)." 
  cd $output_folder  
  output_full_path=`pwd`
  RUN "cd -"
  echo "Inspect results: file://$output_full_path/index.html"
  errorcode=$CODE_MAJOR_DIFFERENCE
elif [[ $report_difference = "minor" ]]; then
  errorcode=$CODE_MINOR_DIFFERENCE
fi

#begin of modification - cgjd 29.03.2012
#new - switch on/off automatic text in fortran programs
if [[ $MAGICS_TEST_AUTOTEXT = off ]];then
  mv -f ./temp/* ./*
  rmdir ./temp 
fi
#end of modification - cgjd 29.03.2012

LOG "Finished"

RETURN $errorcode

