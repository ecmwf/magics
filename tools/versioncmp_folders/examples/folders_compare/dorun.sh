version=$1 
echo "dorun.sh running version $version"

#version value is "instrumental", it has nothing to do with magics version
if [[ $version = "daily" ]]; then
  cp $compfolder1/*.ps .
  touch *.ps
else
  cp $compfolder2/*.ps .
  touch *.ps
fi

