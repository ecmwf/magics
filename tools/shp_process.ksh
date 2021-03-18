#!/bin/ksh

#set -x
# exit on error
set -e

####################################################################
#                                                                  #
# * NB This utility assumes an installation of GDAL is available * #
# *  also that perforce is available and access to the internet  * #
#                                                                  #
####################################################################

# CONFIGURATION

# The command to do the shp processing
#extract='ogr2ogr'
script_location=${0%/*}
geo_loc="$script_location/geoscript"
geo_cmd="$geo_loc/run-geoscript-groovy"
extract="$geo_cmd $geo_loc/extract.groovy"
merge="$geo_cmd $geo_loc/merge.groovy"

# Whether to checkout dst files from perforce
p4_checkout=1

# Whether to download latest files from naturalEarth
download_files=0

# whether to process cities files
process_cities=1

# whether to merge 10m landmasses
merge_10m_land=0

src_folder='.'            # NB not used if re-downloading
tmp_folder='./tmp'              # temporary path
dst_folder="$script_location/../share/magics"   # relative path to output folder

# These are the resolutions and the features we process here
set -A resolutions 10m 50m 110m
set -A featuretypes land lakes rivers_lake_centerlines

# re-establish tmp folder
# fail if exists
if [[ -d $tmp_folder ]]; then
  print -n "Temp folder $tmp_folder/ already exists. Overwrite? (y/N): "; read var
  if [[ $var != 'y' ]]; then
    exit
  fi
fi

# We are clear to proceed
rm -rf $tmp_folder
mkdir $tmp_folder
#mkdir -p $dst_folder


# Checkout files if 
if [[ $p4_checkout -eq 1 ]]; then

    # checking out files from perforce
    for res in ${resolutions[@]}
    do
          p4 sync $dst_folder/$res/$res'_'* 
          p4 edit $dst_folder/$res/$res'_'* 
    done
    p4 sync $dst_folder/10m_full/10m'_'* 
    p4 edit $dst_folder/10m_full/10m'_'* 
fi


for i in ${resolutions[@]}
do
    echo ""
    echo "  ======== ${i} resolution ======== "
    echo ""

    if [[ $download_files -eq 1 ]]; then 
       wget http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/$i/physical/$i-physical.zip -P $tmp_folder
    else
       # get the files locally      
       cp $src_folder/geoscript/$i-physical.zip $tmp_folder
    fi
 
    src_file=$tmp_folder/$i-physical.zip
    output_folder=$tmp_folder/$i'-physical_files'

    # echo Creating folder $output_folder ... 
    rm -rf $output_folder
    mkdir $output_folder

    # echo Extracting $src_file to $output_folder
    unzip -q $src_file -d $output_folder
     
    # there is now a single entry in the output folder
    # and that is where the files are
    subfolder=`ls $output_folder`

    data_folder=$output_folder/$subfolder

    for j in ${featuretypes[@]}
    do
       filestub_nopath=$i'_'$j
       filestub=$data_folder/$filestub_nopath
       echo "renaming file $data_folder/ne_$i_$j.* to $data_folder/$filestub_nopath.*"
       rename $data_folder/'ne_'$i'_'$j $data_folder/$filestub_nopath $data_folder/*.*
       echo ""
       echo "        ***  ${i} ${j}  ***"
       echo ""
       echo "        Processing $filestub'.*'"

       # test all required files exist
       set -A exts dbf shp prj shx
       for ext in ${exts[@]}
       do
           testfile=$filestub'.'$ext
           if [[ -f $testfile ]]; then
              echo "     Found $testfile" 
           else
              echo "     ERROR: $testfile missing!"
           fi

       done 

       # Do landmass feature merging here
       if [[ $i = "10m" && $j = "land" && $merge_10m_land -eq 1 ]]; then

           echo "merging 10m_land file"

           # pre-process the raw downloaded file to merge
           # any adjacent land features with scalerank=0
           # and let all other features pass into the output file
           timestamp=`epoch`
           output_name=tmp_$timestamp
           $merge $data_folder $filestub_nopath.shp $output_name
           # Once done, we copy back over the original
           ls -l $data_folder
           rename $output_name $filestub_nopath $data_folder/*.*
           ls -l $data_folder
       fi

       # Resolution changes here

       ogrout=$filestub'/output'
       echo "    Creating temporary folder $filestub"
       mkdir $filestub       
       
       # set the scale limit according to the feature type
        
       scale_limit=6
       if [[ $j = "land" ]]; then
          # a little more drastic on the land
          scale_limit=5
       fi

       if [[ $i = "10m" ]]; then 
    
           echo "    Reducing resolution to $scale_limit for feature type $j at scale $i"          
           $extract -where "ScaleRank<=$scale_limit" $ogrout $filestub'.shp' 

           set -A excluded_regions
           #NB an initial space \|/ appears compulsory when specifying -ve numbers :-/
           excluded_regions[0]=" 12.0W 32.0N 48.0E 72.0N"  		# EUROPE

           # Now append the previously excluded data for these regions only
           # only preseve data up to an absolute ceiling so we don't get
           # ridiculously small features anywhere
           scale_ceiling=6

           index=0
           while [ $index -lt ${#excluded_regions[@]} ]
           do
               echo "    Preserving high-resolution data in region ${excluded_regions[$index]}"
               $extract -append -spat ${excluded_regions[$index]} -where "ScaleRank > $scale_limit AND ScaleRank <= $scale_ceiling" $ogrout $filestub'.shp' 
                
               let index=$index+1
           done
           
           # also copy the original files directly to a "10m_full" folder
           echo "    Copying full-resolution files to $data_folder/$i'_full_'$j'_output'"
           for ext in ${exts[@]}
           do
               mkdir -p $data_folder/$i'_full_'$j'_output'
               cp $filestub'.'$ext $data_folder/$i'_full_'$j'_output'
           done

       else
           #simple copy
           mkdir -p $ogrout
           echo "    Copying files at full resolution"
           for ext in ${exts[@]}
           do 
                cp $filestub'.'$ext $ogrout
           done
       fi


       # if statements to process according to file type
       if [[ $j = "lakes" ]]; then              


          # IF we are processing 10m files we need to take account of the fact
          # that we need 10m_full resolution too.
          # NB in later versions of naturalearth Name1 and Name2 might be e.g.
          # Name and Name_alt or some other combination...
          # 
          if [[ $i = "10m" ]]; then
             echo "    Removing Great Lakes duplicates and Caspian sea for $j feature type $i resolution"
              $extract -overwrite -where "Name1 != 'Great Lakes' AND Name1 != 'Great  Lakes' AND Name1 != 'Caspian Sea' AND (Name2 != 'Great Lakes' OR ScaleRank=1 )" $dst_folder/$i'_full' $data_folder/$i'_full_'$j'_output'/$filestub_nopath'.shp' 
              $extract -overwrite -where "Name1 != 'Great Lakes' AND Name1 != 'Great  Lakes' AND Name1 != 'Caspian Sea' AND (Name2 != 'Great Lakes' OR ScaleRank=1 )" $dst_folder/$i $ogrout/$filestub_nopath'.shp' 
           else
             # remove great lakes and caspian sea
             # with output folder the target folder
             echo ""
             echo "    Removing Great Lakes duplicates and Caspian sea for $j feature type"
             $extract -overwrite -where "Name1 != 'GREAT  LAKES' AND Name1 != 'Great Lakes' AND Name1 != 'Great  Lakes' AND Name1 != 'Caspian Sea' AND (Name2 != 'Great Lakes' OR ScaleRank=1 )" $dst_folder/$i $ogrout/$filestub_nopath'.shp'
              echo "    Files written to target folder $dst_folder"

          fi
         

       else
         # a simple copy to the target folder
         echo ""
         echo "    Copying files to destination folder $dst_folder/$i"
         mkdir -p $dst_folder/$i
         for f in `ls $ogrout`
         do
           cp $ogrout/$f $dst_folder/$i/
           echo "     Copied $ogrout/$f to target folder $dst_folder/$i"
         done
        

        if [[ $i = "10m" ]]; then
             # also take a copy of our full-resn files to 10m_full
             mkdir -p $dst_folder/$i'_full/'
             for f in `ls $data_folder/$i'_full_'$j'_output'/`
             do
               cp  $data_folder/$i'_full_'$j'_output'/$f $dst_folder/$i'_full/'
               echo "     Copied $data_folder/$i'_full_'$j'_output'/$f to target folder $dst_folder/$i'_full'"
              done
        fi
      fi

      
    done

    rm -f $dst_folder/*/*.qix $dst_folder/*/*.fix
done


if [[ $process_cities -eq 1 ]]; then

  ################################################################################################
  #
  #       C I T I E S
  #
  echo ""
  echo "  Clean-up cities ..."
  POP_NAME=10m-populated-places-simple
  mkdir -p $tmp_folder/cultural/output
  if [[ $download_files -eq 1 ]]; then 
      wget http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/${POP_NAME}.zip -P $tmp_folder
  else
      # get the files locally
      cp $src_folder/geoscript/${POP_NAME}.zip $tmp_folder
  fi
  unzip -q $tmp_folder/${POP_NAME}.zip -d $tmp_folder/cultural/

  rename $tmp_folder/cultural/ne_10m_populated_places_simple $tmp_folder/cultural/10m_populated_places_simple $tmp_folder/cultural/*.* 

  $extract -where "NAME != 'Vatican City' AND NAME != 'Vaduz' AND NAME != 'San Marino' AND NAME != 'Nicosia'" $tmp_folder/cultural/output $tmp_folder/cultural/10m_populated_places_simple.shp
  cp -f $tmp_folder/cultural/output/* $dst_folder/10m/

fi

################################################################################################
#
# clean up - remove tmp files
#
if [[ $p4_checkout -eq 1 ]]; then
  echo "  Revert unchanged files"
  p4 revert -a ../share/magics/*0m*/*.*
fi
echo ""
#echo "  Removing temporary folder $tmp_folder"
#rm -rf $tmp_folder



echo Done

