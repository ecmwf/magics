Name: emos
Version: 000382
Release: 1.1
Summary: Library and tools to handle Grib 1 and BUFR files
URL: http://www.ecmwf.int/products/data/software/interpolation.html
BuildRoot:    %{_tmppath}/emos-%{version}-build
Source0: emos-%{version}.tar.bz
License: LGPLv3
Group: Productivity/Scientific/Other
# Copyright 2011 ECMWF

BuildRequires: gcc
BuildRequires: grib_api  
BuildRequires: grib_api-devel

%if 0%{?suse_version}
BuildRequires: gcc-fortran
%else
BuildRequires: gcc-gfortran
%endif

%if 0%{?suse_version} > 1100
BuildRequires: libjasper-devel
%endif


#!BuildIgnore: post-build-checks

Requires: grib_api

%description
Runtime files of the ECMWF Emoslib, an application program interface accessible developed to do meteorological interpolation and for the encoding of WMO FM-92 GRIB edition 1 and BUFR files.

Please be aware that this library is build in DOUBLE PRECISION for the use in Magics++ and Metview.



%prep

%setup -q

%build

export R64=R64
export CNAME=_gfortran
export target=linux
export GRIB_API=grib_api_merging
export GRIB_API_DIR=/usr
export JASPER_DIR="-ljasper"

%ifarch x86_64
 export A64=A64
%endif

echo $R64 > .r64
echo %_libdir > .emos

# correct link options for 64bit
%ifarch x86_64
olds="DEBUG ="
news="DEBUG = -fPIC "
chmod +w config/config.$target$CNAME$R64$A64.in
sed "s/${olds}/${news}/g" config/config.$target$CNAME$R64$A64.in > temp.in
\mv -f temp.in config/config.$target$CNAME$R64$A64.in
%endif

#cat Makefile.in | sed s:reals:$R64: > Makefile
cat Makefile.in | sed s:reals:$R64: | sed s:glue:grib_api_merging: > Makefile

for subdirs in gribex pbio bufrdc bufrtables crexdc interpolation fft
do
cat $subdirs/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:depl:emos: | sed s:comp:$CNAME: > $subdirs/Makefile
done

for subdirs in examples/gribex examples/bufr examples/crex examples/interpolation examples/fft
do
cat $subdirs/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:comp:$CNAME: > $subdirs/Makefile
done

#grib_api
cat grib_api_merging/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:comp:$CNAME: | sed s:depl:emos: | sed s:glue:$GRIB_API_DIR: > grib_api_merging/Makefile

cat examples/interpolation_grib_api/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:comp:$CNAME: | sed s:glue:$GRIB_API_DIR: | sed s:jasp:$JASPER_DIR: > examples/interpolation_grib_api/Makefile

cat config/config.$target$CNAME$R64$A64.in | sed s:emos:$INSTALL_DIR: > config/config.$target$CNAME$R64$A64


make
ls bufrtables/*000* > .list/bufrtables



%install
# install all files into the BuildRoot
#./install
echo $PWD
echo $RPM_BUILD_ROOT
export R64=R64
export INSTALL_DIR=$RPM_BUILD_ROOT/%_libdir

mkdir -p $RPM_BUILD_ROOT/%_libdir

echo "Installing Tables, land-sea mask and EMOS library into $INSTALL_DIR ... "


mkdir -p $INSTALL_DIR/gribtables
mkdir -p $INSTALL_DIR/bufrtables
mkdir -p $INSTALL_DIR/crextables
mkdir -p $INSTALL_DIR/land_sea_mask
mkdir -p $INSTALL_DIR/gribtemplates

for i in `cat .list/gribtables` ; do
    cp $i $INSTALL_DIR/gribtables
done
for i in `cat .list/bufrtables` ; do
    cp $i $INSTALL_DIR/bufrtables
done
for i in `cat .list/crextables` ; do
    cp $i $INSTALL_DIR/crextables
done
for i in `cat .list/land_sea_mask` ; do
    cp $i $INSTALL_DIR/land_sea_mask
done
for i in `cat .list/gribtemplates` ; do
    cp $i $INSTALL_DIR/gribtemplates
done

cp libemos$R64.a $INSTALL_DIR/

%clean
# clean up the hard disc after build
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%_libdir/libemosR64.a
%_libdir/bufrtables
%_libdir/crextables
%_libdir/gribtables
%_libdir/gribtemplates
%_libdir/land_sea_mask

%changelog

* Tue Mar 22 2011 - magics@ecmwf.int
- Full support for GRIB 2 files for interpolation
- Links with GribAPI 1.9.9

* Sun Feb 21 2011 - magics@ecmwf.int
- update Emos version to 377 
- Better GRIB 2 support

* Sun Dec 19 2010 - magics@ecmwf.int
- update Emos version to 376

