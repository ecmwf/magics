# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

SET(ENABLE_CAIRO   ON  CACHE BOOL "Enable PNG")
SET(ENABLE_PYTHON  ON  CACHE BOOL "Enable Python")
SET(ENABLE_ODB     OFF  CACHE BOOL "Enable ODB")
SET(ENABLE_METVIEW ON  CACHE BOOL "Enable Metview")
# SET(WITH_PGI_FORTRAN ON  CACHE BOOL "Enable pgi Fortran")
# SET(PGI_PATH /usr/local/apps/pgi/pgi-10.8/linux86-64/10.8/libso  CACHE STRING "PGI library path")
SET(CMAKE_CXX_FLAGS "-std=c++1y" CACHE STRING "CXX FLAGS")
