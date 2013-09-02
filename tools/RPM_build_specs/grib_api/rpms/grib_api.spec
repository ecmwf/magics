Name: grib_api
Version: 1.9.9
Release: 2.1
Summary: Library and tools to handle Grib 1 and 2 files
URL:http://www.ecmwf.int/products/data/software/grib_api.html
BuildRoot:    %{_tmppath}/%{name}-%{version}-build
Source0: %{name}-%{version}.tar.gz
License: LGPLv3
Group: Productivity/Scientific/Other

#
# Build requirements
#
BuildRequires: gcc

%if 0%{?suse_version} >= 1130
BuildRequires: python-numpy-devel python-devel swig
Requires: python python-numpy
%endif

%if 0%{?suse_version}  
BuildRequires: gcc-fortran  
%else  
BuildRequires: gcc-gfortran  
%endif

#
#  SLES 9 & 10 do NOT have jasper!!!
#
%if 0%{?sles_version} != 10 && 0%{?sles_version} != 9 && 0%{?centos_version} < 100 
PreReq: libjasper-devel
%endif


# Copyright 2010 ECMWF

#!BuildIgnore: post-build-checks

%description
Runtime files of the ECMWF GRIB API is an application program interface accessible from C and FORTRAN programs developed for encoding and decoding WMO FM-92 GRIB edition 1 and edition 2 messages. A useful set of command line tools is also provided to give quick access to grib messages.

%package devel
Summary: Developing package for grib_api
Group: Development/Libraries
%description devel
Header files and library of the ECMWF GRIB API is an application program interface accessible from C and FORTRAN programs developed for encoding and decoding WMO FM-92 GRIB edition 1 and edition 2 messages. A useful set of command line tools is also provided to give quick access to grib messages.


%prep
%setup -q
%build
%if 0%{?sles_version} == 10 || 0%{?sles_version} == 9 || 0%{?centos_version} > 500 
  ./configure --prefix=/usr --libdir=%_libdir CFLAGS="$CFLAGS -fPIC" --disable-jpeg
%else
%if 0%{?suse_version} >= 1130
  ./configure --prefix=/usr --libdir=%_libdir CFLAGS="$CFLAGS -fPIC" --enable-python
%else
  ./configure --prefix=/usr --libdir=%_libdir CFLAGS="$CFLAGS -fPIC"
%endif
%endif

make
%install
# install all files into the BuildRoot
make DESTDIR=$RPM_BUILD_ROOT install

%clean
# clean up the hard disc after build
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/bin/grib*
/usr/bin/tigge*
/usr/bin/big2gribex
/usr/bin/parser
/usr/bin/points
/usr/bin/gg_sub_area_check
/usr/share/definitions
%if 0%{?suse_version} == 1130
%_libdir/python2.6/site-packages/grib_api
%endif
%if 0%{?suse_version} > 1130
%_libdir/python2.7/site-packages/grib_api
%endif

%files devel
%defattr(-,root,root,-)
/usr/include/grib_api.h
/usr/include/grib_api.mod
/usr/include/grib_api_f77.h
%_libdir/libgrib_api.a
%_libdir/libgrib_api_f77.a
%_libdir/libgrib_api_f90.a
/usr/ifs_samples
/usr/share/samples
