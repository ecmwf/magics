Name: Magics
Version: 2.14.8
Release: 1.1
Summary: Library and tools to visualize meteorological data and statistics
URL: http://www.ecmwf.int/products/data/software/magics++.html
BuildRoot: %{_tmppath}/%{name}-%{version}-build
Source0: %{name}-%{version}.tar.bz2
License: Apache V2.0
# Copyright 2011 ECMWF
Group: Productivity/Scientific/Other
Obsoletes: %{name}++

Requires: emos
Requires: grib_api >= 1.9.8
Requires: cairo    >= 1.4.0

##################################################

#!BuildIgnore: post-build-checks

BuildRequires: gcc-c++
BuildRequires: swig
BuildRequires: perl-XML-Parser
BuildRequires: cairo-devel
BuildRequires: pango-devel
BuildRequires: grib_api-devel
BuildRequires: libqt4-devel
BuildRequires: emos


%if 0%{?suse_version} > 1120
BuildRequires: libQtWebKit-devel
BuildRequires: libqt4-devel
Requires: netcdf libnetcdf4
Requires: python
BuildRequires: python-devel python-numpy-devel swig
BuildRequires: netcdf libnetcdf4 libnetcdf-devel
%endif

%if 0%{?fedora} || 0%{?centos_version} || 0%{?rhel_version}

BuildRequires: pkgconfig gcc-gfortran
BuildRequires: expat expat-devel

%else
BuildRequires: gd gd-devel
Requires: gd gd-devel
BuildRequires: gcc-fortran libexpat-devel
%if 0%{?suse_version} > 1100
BuildRequires: libjasper-devel
%endif
%endif


################################################

%description
Runtime files for Magics - The library and tools to visualize meteorological data and statistics

%package devel
Summary: Developing package for Magics
Group: Development/Libraries/C and C++
Requires: %{name} grib_api-devel
Obsoletes: %{name}++-devel
%description devel
Header and library files for Magics - The library and tools to visualize meteorological data and statistics

%if 0%{?suse_version} > 1100

%package metview-devel
Summary: Magics package for Metview 4
Group: Development/Libraries/C and C++
Requires: %{name}-devel
%description metview-devel
Magics header and library to compile Metview

%endif

Authors:
--------
  (ECMWF Meteorological Visualisation Section)
  Sylvie Lamy-Thepaut
  Stephan Siemen
  Fernando Ii
  Iain Russell
  Sandor Kertesz

##############################################

%prep
%setup -q
%build
%if 0%{?fedora} || 0%{?centos_version} || 0%{?rhel_version}
  ./configure --prefix=/usr --libdir=%_libdir --disable-netcdf --enable-bufr --enable-json --disable-jasper --disable-raster
%else
%if 0%{?suse_version} > 1120
  ./configure --prefix=/usr --libdir=%_libdir --enable-netcdf --enable-bufr --enable-json --enable-metview --enable-python CXXFLAGS="-O2 -mtune=generic"
%else
  ./configure --prefix=/usr --libdir=%_libdir --disable-netcdf --enable-bufr --enable-json
%endif
%endif

make
%install
# install all files into the BuildRoot
make DESTDIR=$RPM_BUILD_ROOT install

%clean
# clean up the hard disc after build
rm -rf $RPM_BUILD_ROOT


%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

###############################################

%files
%defattr(-,root,root)
%doc AUTHORS ChangeLog NOTICE LICENSE
/usr/bin/magmlx
/usr/bin/magjson
/usr/bin/magics-config
/usr/bin/magicsCompatibilityChecker
/usr/share/magics
%_libdir/libMagPlus.so.*
%if 0%{?suse_version} == 1130
%_libdir/python2.6/site-packages
%endif
%if 0%{?suse_version} > 1130
%_libdir/python2.7/site-packages
%endif


%files devel
%defattr(-,root,root)
/usr/include/magics
/usr/share/aclocal/magics.m4
%_libdir/pkgconfig/magics.pc
%_libdir/libMagPlusSingle.a
%_libdir/libMagPlusDouble.a
%_libdir/libMagPlus.so
%_libdir/libMagPlus.la

%if 0%{?suse_version} > 1120
%files metview-devel
%defattr(-,root,root)
%_libdir/libMagPlusQt.la
%_libdir/libMagPlusQt.so*
%_libdir/libMagWrapper.a
%endif

################################################

%changelog
* Mon Dec 12 2011 - magics@ecmwf.int
- update to version 2.14.8 (bug fixe for python interface and borders)

* Mon Nov 22 2011 - magics@ecmwf.int
- update to version 2.14.5 (bug fixe for graph plotting with symbols)

* Mon Nov 21 2011 - magics@ecmwf.int
- update to version 2.14.4 (bug fixes, optimisations for SVG)

* Mon Nov 14 2011 - magics@ecmwf.int
- update to version 2.14.2 (bug fixes, fixes for python interface)

* Thu Oct 26 2011 - magics@ecmwf.int
- update to version 2.14.1 (new coastlines, new python interface)

* Thu Jun 16 2011 - magics@ecmwf.int
- update to version 2.12.9 (bug fixes in layout and PostScript output)

* Thu Apr 15 2011 - magics@ecmwf.int
- update to version 2.12.7 (remove unnecessary warning messages)

* Thu Apr 14 2011 - magics@ecmwf.int
- update to version 2.12.6 (add JSON and Metview 4.0.4 support)

* Thu Dec 02 2010 - magics@ecmwf.int
- update to version 2.12.0

* Fri Nov 20 2010 - magics@ecmwf.int
- update to version 2.11.5  (threaded contouring, Metview support)

* Fri Jun 25 2010 - magics@ecmwf.int
- update to version 2.10.2  (improve hatch shading + SVG output)

* Mon May 03 2010 - magics@ecmwf.int
- update to version 2.10  (add synop plotting)

* Tue Oct 20 2009 - magics@ecmwf.int
- initial package
