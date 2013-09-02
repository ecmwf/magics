#!/bin/ksh
echo $MAGPLUS_VERSION

if [[ $MAGPLUS_VERSION = "new++" ]]; then
touch /tmp/cgs/perforce/dev/magics/tools/versioncmp/examples/obstat/ps.ps
cp /tmp/cgs/perforce/dev/magics/tools/versioncmp/examples/obstat/ps.ps .
fi


if [[ $MAGPLUS_VERSION = "daily" ]]; then
touch /tmp/cgs/perforce/dev/magics/tools/versioncmp/examples/obstat/old.ps
cp /tmp/cgs/perforce/dev/magics/tools/versioncmp/examples/obstat/old.ps .
fi

