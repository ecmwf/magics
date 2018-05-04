# (C) Copyright 2018 - ECMWF
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
# Find the Jinja2 module
# This module finds if Jinja2 is installed, and sets the following variables
# indicating where it is.
#
#  JINJA2_FOUND   - was Jinja2 found
#

# Finding Jinja2 requires calling the Python interpreter
if(Jinja2_FIND_REQUIRED)
    find_package(PythonInterp REQUIRED)
else()
    find_package(PythonInterp)
endif()

if(NOT PYTHONINTERP_FOUND)
    set(JINJA2_FOUND FALSE)
    return()
endif()

execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
    "import jinja2 as j; print(j.__version__);"
    RESULT_VARIABLE _JINJA2_SEARCH_SUCCESS
    OUTPUT_VARIABLE _JINJA2_VALUES_OUTPUT
    ERROR_VARIABLE _JINJA2_ERROR_VALUE
    OUTPUT_STRIP_TRAILING_WHITESPACE)

if(NOT _JINJA2_SEARCH_SUCCESS MATCHES 0)
    if(Jinja2_FIND_REQUIRED)
        message(FATAL_ERROR "Jinja2 import failure:\n${_JINJA2_ERROR_VALUE}")
    endif()
    message(FATAL_ERROR "Please install Python module Jinja2 (e.g. with pip or conda)")
    set(JINJA2_FOUND FALSE)
    return()
endif()

find_package_message(JINJA2 "Found Jinja2" "Using ${PYTHON_EXECUTABLE}")

set(JINJA2_FOUND TRUE)
