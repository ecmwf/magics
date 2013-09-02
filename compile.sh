#!/bin/ksh


. ./version.sh
version="${MAGICS_PACKAGE_VERSION}"
subdir="."

metview=off
if test "x$1" = x"mv4" -o "x$1" = x"mv4d"; then
  compile="--enable-metview "
  metview=on
fi


debug=off
if test "x$1" = x"debug" -o "x$1" = x"mv4d"; then
  compile="${compile}--enable-debug "
#  export CXXFLAGS="${CXXFLAGS} -g -DOBS_DEBUG_ -DDRIVERS_DEBUG_"
  export CXXFLAGS="${CXXFLAGS} -g"
  debug=on
  subdir="debug"
fi

export CXX=g++ 
export CC=gcc 
export F77=pgf90
export FC=pgf90

if test "x$1" = x"in" -o "x$1" = x"ip" -o "x$1" = x"id"; then
  compile="--enable-metview "
  metview=on
  export CXX=icpc 
  export CC=icc
  export F77=ifort
  export FC=ifort
  intel_common="-O3 -xhost -opt-report -vec-report3"
  if test "x$1" = x"ip"; then
    intel_common="-O3 -xhost -opt-report -vec-report3 -parallel -par-report3"
  fi
  if test "x$1" = x"id"; then
    intel_common="-guide -parallel"
  fi
  export CFLAGS="${intel_common}"
  export CXXFLAGS="${intel_common}"
fi

if test x"$OS_VERSION" = "xrhel63"; then
  export F77=gfortran
  export FC=gfortran
fi


echo ""
echo " ++++++++++++++++++++++++++++++++++++++++++++++++++"
echo ""
started="`date`"
echo "  BUILDING  Magics version ${version}  starting ${started}"
echo ""
echo " ++++++++++++++++++++++++++++++++++++++++++++++++++"
echo ""
echo " CC =${CC}"
echo " CXX=${CXX}"
echo " F77=${F77}"
echo " FC =${FC}"
echo ""

./bootstrap

apps_path="/usr/local/apps/Magics/${version}"

exp="--disable-exception "
static_common=" ${exp}--enable-static --disable-shared"
shared_common=" --disable-static --enable-shared"

case "${OS_VERSION}" in
   # 32bit Linux
   suse91)
	if test "x$debug" = x"off"; then
	   export CXXFLAGS="${CXXFLAGS} -mtune=native -O2"
	fi
	# STATIC
	./configure.ecmwf --prefix=${apps_path} --disable-cairo --enable-bufr --enable-spot ${static_common}
	make clean
	make quiet

	# SHARED
	./configure.ecmwf --prefix=${apps_path} --disable-cairo --enable-json --enable-bufr --enable-spot ${shared_common}
	make clean
	make quiet
   ;;
   sles11)
        if test "x$CC" = "x"; then
	   export CC=gcc
	fi
	if test "x$debug" = "xoff" -a "x$CC" = "xgcc"; then
	   export CXXFLAGS="${CXXFLAGS} -mtune=native -O2"
	fi
	export PATH=/usr/local/apps/qt/4.6.2\-64/bin:${PATH}
	export GNU_MODE=64
	export PGI_MODE=64
	export GRIB_API_MODE=64
	export OBJECT_MODE=64
	use pgi
	use grib_api

	if test "x$CC" = "xicc"; then            # intel
	  . /usr/local/apps/intel/current/bin/ifortvars.sh intel64
	  . /usr/local/apps/intel/current/bin/iccvars.sh intel64
	  export INTEL_LICENSE_FILE='28518@papercup'
	  bash ./configure.ecmwf --prefix=${apps_path}_intel --enable-bufr --enable-spot --enable-json --enable-odb ${compile}${shared_common} --enable-python && \
	  make clean && \
	  make -j 4
	else   # gcc
	  # STATIC
	  ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-spot ${static_common} && \
	  make clean && \
	  make quiet && \
	  ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-spot --enable-json --enable-odb ${compile}${shared_common} --enable-python && \
	  make clean && \
	  make quiet
	fi
   ;;
   rhel6)
           ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-odb ${static_common} && \
           make clean && make quiet
           ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-odb ${compile}${shared_common} --enable-python --enable-json && \
           make clean && make quiet
   ;;
   opensuse113)
	export PATH=/usr/local/apps/python/current/bin/:$PATH
	export GNU_MODE=64
	export PGI_MODE=64
	export GRIB_API_MODE=64
	export OBJECT_MODE=64
	use pgi
	use grib_api

	if test "x$debug" = "xoff"; then
	   export CXXFLAGS="${CXXFLAGS} -mtune=native -O3"
#	   subdir="release"
#	   rm -rf ${subdir}
#	   mkdir ${subdir}
#	   cd ${subdir}
#	   mkdir -p src/drivers/MgQ
#	   ../
	   ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-odb --enable-spot ${static_common} && \
	   make clean && \
	   make quiet
	   ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-odb --enable-spot ${compile}${shared_common} --enable-python && \
	   make clean && \
	   make quiet
#	   cd -
	else
	   export CXXFLAGS="${CXXFLAGS} -O0 -g"
#	   subdir="debug"
#	   rm -rf ${subdir}
#	   mkdir ${subdir}
#	   cd ${subdir}
#	   mkdir -p src/drivers/MgQ
#	   ../
	   ./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-odb --enable-spot ${compile}--enable-static --enable-python && \
	   make clean && \
	   make quiet
#	   cd -
	fi

	#
	#   M E T V I E W
	#
	if test "x$metview" = "xon" ; then
	    cd ../metview && ./bootstrap
	    if test "x$debug" = x"off"; then
	      ./configure.ecmwf --enable-ecregrid --enable-experimental --with-magics-home=${apps_path} && make clean && make
	    else
	      rm -rf debug
	      mkdir debug
	      cd debug
	      ../configure.ecmwf --enable-debug --enable-qtdebug --enable-ecregrid --enable-experimental --with-magics-home=${apps_path} && make clean && make
	    fi
	fi
   ;;

   opensuse103)
	if test "x$debug" = x"off"; then
	   export CXXFLAGS="${CXXFLAGS} -mtune=native -O2"
	fi
	#
	# STATIC
	#
	./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-spot ${compile}${static_common} \
	&& make clean && make quiet

	#
	# SHARED
	#
	export CXXFLAGS="-g ${CXXFLAGS}"
	./configure.ecmwf --prefix=${apps_path} --enable-bufr --enable-spot ${compile}${shared_common} \
	   --enable-json --enable-odb --enable-python \
	&& make clean && make quiet
   ;;
   
   #########################################################
   #
   # IBM Server
   #
   #	*** C1B ***
   #	ssh -X c1b
   #	module unload c
   #	module load c++/vacpp/12.0.0.0_b01
   #	PROJ4 --> bash configure --disable-shared CC=xlC_r --prefix=/perm/graphics/cgm/proj4
   #	./configure CPPFLAGS="-I/perm/graphics/cgm/boost_1_49_0/" LDFLAGS=-L/usr/local/lib/metaps/lib/grib_api/jasper/lib --with-grib-api=/usr/local/lib/metaps/lib/grib_api/1.9.16 --disable-netcdf --with-proj4=/perm/graphics/cgm/proj4 CXX=xlC_r LD=xlC_r

   #	Base=/usr/local/apps/vacpp/vacpp12.eval/
   #	export PATH=$Base/usr/vacpp/bin:$PATH
   #	export LIBPATH=$Base/usr/lib:/usr/lib:/usr/local/lib
   aix)
    use vacpp12100

    export AR=/usr/bin/ar 
    export LD=/usr/bin/ld
    export NM='/usr/bin/nm -B'
    export CC=xlc_r
    export CXX=xlC_r
    export F77=xlf
    export FC=xlf90
#    export CXXFLAGS="-g -O2"
#    export CFLAGS="-g -O2"

    /bin/bash ./configure.ecmwf --prefix=${apps_path} --disable-python --enable-raster --disable-cairo --enable-bufr ${compile} --enable-static --disable-shared ; gmake clean ;  gmake -j 4 && gmake install
###    /bin/bash ./configure.ecmwf --prefix=${apps_path} --disable-raster --disable-cairo --enable-bufr ${compile} --enable-static --enable-python ; gmake clean ;  gmake -j 4 && gmake install

#    export GRIB_API_MODE=64
#    export OBJECT_MODE=64
###    /bin/bash ./configure.ecmwf --prefix=${apps_path}_${OBJECT_MODE} --enable-metview --disable-raster --disable-cairo --enable-bufr --enable-static --disable-shared ${compile} ; gmake clean ;  gmake -j 4 && gmake install
#    /bin/bash ./configure.ecmwf --prefix=${apps_path}_${OBJECT_MODE} --enable-metview --disable-raster --disable-cairo --enable-bufr --enable-static ${compile} ; gmake clean ;  gmake -j 4 && gmake install
   ;;

   hpux11)
	./configure.ecmwf CXX=aC++ CC=aCC F77=f90 GREP=egrep LD=ld
   ;;

	#######################################################
    #
    #  MAC OS X 
    #
    #  sudo port install cairo pango pkgconfig swig swig-python py-numpy p5-xml-simple curl boost netcdf gcc46 qt4-mac openmotif grib_api pdksh gv proj
    #  export PKG_CONFIG_PATH="/usr/lib/pkgconfig:/usr/X11R6/lib/pkgconfig:/local/lib/pkgconfig:/opt/local/lib/pkgconfig"
    #  export RSYNC_PROXY=http://proxy.ecmwf.int:2222
    #  export OS_VERSION=mac

   mac)
    export MACOSX_DEPLOYMENT_TARGET=10.8
    ./configure ${compile} CXX="g++-mp-4.6" CC="gcc-mp-4.6" F77="gfortran-mp-4.6" \
    && make clean  \
    && make
#      --enable-json --enable-python \
#   cd ../metview
#   ./configure --with-magics-home=/usr/local --with-grib-api=/opt/local/ LDFLAGS=-L/opt/local/lib LIBS=-lpng CXX=g++-mp-4.6 CC=gcc-mp-4.6 F77=gfortran-mp-4.6 --disable-ui
   ;;


   *)
      if test "x${1}" = "xintel"; then
	echo ""
	echo "    I N T E L "
	echo ""
	compile=" --enable-metview "
	./configure CXX="icpc" CC="icc" F77="ifort" ${compile} CXXFLAGS="${CXXFLAGS} -Wall -DOBS_DEBUG_ -DDRIVERS_DEBUG_" LIBS="-lgfortran" --libdir=/usr/local/lib64 \
	    --enable-json --enable-bufr --enable-python
#	&& make clean \
#	&& make && sudo make install

#	cd ../metview &&./bootstrap && \
#	./configure CXX="icpc" CC="icc" F77="ifort" --enable-experimental --enable-ecregrid --enable-qtdebug --with-grib-api=/usr && make clean && make

      else

	echo ""
	echo "SETUP choosen for OUTSIDE the centre!!!"
	echo ""
	export F77=gfortran
	export FC=gfortran

	if test "x$debug" = x"off"; then
	   export CXXFLAGS="${CXXFLAGS} -mtune=native -O2 -ftree-vectorizer-verbose=1" #  -Wdisabled-optimization"
	else
	   export CXXFLAGS="${CXXFLAGS} -g -O0 -Wextra -Wdisabled-optimization"  # -Wfloat-equal -Wshadow
	fi 
	compile=" --enable-metview "
	./configure ${compile} CXXFLAGS="${CXXFLAGS} -Wall -DOBS_DEBUG_ -DDRIVERS_DEBUG_" --libdir=/usr/local/lib64 --enable-bufr --enable-python \
	&& make clean  \
	&& make && sudo make install

	if test -f apps/MagMLInterpretor/magmlx ; then
	  cd ../metview &&./bootstrap && \
	   ./configure --enable-experimental --enable-ecregrid --enable-qtdebug --with-grib-api=/usr && make clean && make
	else
	   echo ""
	   echo "    Magics++ did NOT build!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	   echo ""
	fi
      fi
   ;;
esac


echo ""
echo " ++++++++++++++++++++++++++++++++++++++++++++++++++"
echo ""
echo "  FINISHED BUILDING  Magics version ${version}" 
if test "x$debug" = x"on"; then
echo ""
echo "            D E B U G  version" 
fi
echo ""
echo "  Started  ${started}"
echo ""
echo "  Finished `date`"
echo ""
echo " ++++++++++++++++++++++++++++++++++++++++++++++++++"
echo ""
