Name: emos
Version: 000370
Release: 17.1
Summary: Library and tools to handle Grib 1 and BUFR files
URL: http://www.ecmwf.int/products/data/software/interpolation.html
BuildRoot:    %{_tmppath}/emos-%{version}-build
Source0: emos-%{version}.tar.bz
License: LGPLv3
Group: Productivity/Scientific/Other

BuildRequires: gcc

%if 0%{?suse_version}
BuildRequires: gcc-fortran
%else
BuildRequires: gcc-gfortran
%endif

# Copyright 2009 ECMWF

#!BuildIgnore: post-build-checks

%description
Runtime files of the ECMWF Emoslib, an application program interface accessible developed to do meteorological interpolation and for the encoding of WMO FM-92 GRIB edition 1 and BUFR files.

Please be aware that this library is build in DOUBLE PRECISION for the use in Magics++ and Metview.



%prep
#cd /usr/src/packages/BUILD
#tar -xzvf /usr/src/packages/SOURCES/emos_%{version}.tar.gz
#echo $PWD
%setup -q

%build

#cd emos_%{version}
echo $PWD

export R64=R64
export CNAME=_gfortran
export target=linux

%ifarch x86_64
 export A64=A64
%endif

echo $R64 > .r64
echo %_libdir > .emos

cat Makefile.in | sed s:reals:$R64: > Makefile

for subdirs in gribex pbio bufrdc bufrtables crexdc interpolation fft
do
cat $subdirs/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:depl:emos: | sed s:comp:$CNAME: > $subdirs/Makefile
done

for subdirs in examples/gribex examples/bufr examples/crex examples/interpolation examples/fft
do
cat $subdirs/Makefile.in | sed s:reals:$R64: | sed s:arch:$target: | sed s:plat:$A64: | sed s:comp:$CNAME: > $subdirs/Makefile
done

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

