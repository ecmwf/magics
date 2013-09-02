 if [[ $1 = '' || $2 = '' ]]; then
	echo "ERROR. Arguments missing: please specify the folders to compare"
	exit 1
 fi 

export compfolder1=$1
export compfolder2=$2

#version values "daily" and "new++" are conventional, they have nothing to do with the folder comparison
./versioncmp.sh daily new++ ./examples/folders_compare/dorun.sh -output ./output

