#!/bin/ksh
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


ecfs_folder=ec:/maf/magics_tests

#########################################
function backup_folder {
##########################################

  folder=`echo $1 | sed -e "s/\/*$//"`
  echo "Input folder is $folder"

  # strip any trailing slash
  if [[ -d $1 ]]; then

    tarfile=$folder.tar.gz
    echo Archiving folder $folder to local file $tarfile...
    sleep 1
    #time tar cvfz $tarfile $folder
    time tar cfz $tarfile $folder

    # tar filename might reside in a different folder
    # and be of the form /path/to/folder/tarname.tar.gz
    tarfilename=`basename $tarfile`

    echo Moving file $tarfilename to ${ecfs_folder}/.
    sleep 1
    time emv -o $tarfile ${ecfs_folder}/$tarfilename

    # if local file has gone then it worked
    if [[ ! -f $tarfile ]]; then
      echo "Removing local folder $folder"
      sleep 1
      echo rm -rf $folder
      time rm -rf $folder
      return 0
    else
      echo "Problem moving file to ECFS. Investigate."
      return 1
    fi

  else
    echo echo \'$folder\' is not a valid local folder
    return 1
  fi
}

if [[ $# -eq 0 ]]; then
  echo Usage: archive.sh RESULTSFOLDER
  exit 1
fi
MAXFOLDERS=5
output_folder=`echo $1 | sed -e "s/\/*$//"`

# get list of files ordered by reverse name
# which means that the earlier ones will be the
# latest backups. Keep the first MAXFOLDERS of
# the resultant array

# get array of folders
set -A folders `ls -rd ${output_folder}/output*`

# if too many, backup the excess folders
if [[ ${#folders[@]} -gt $MAXFOLDERS ]]; then
  for i in $(seq ${#folders[@]} -1 $MAXFOLDERS); do
    folder=${folders[$i]}
    if [[ -d $folder ]];then

      echo Backing up $folder
      backup_folder $folder
    fi
  done



fi

