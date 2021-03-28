/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsCalls.cc
    \brief Implementation of Fortran and C interface

 To use the C interface "magics_api.h" must be included.

 Changes: 13-06-2006 Added C interface (Stephan)

 \sa magics_api.h
*/

#include "MagicsCalls.h"
#include "magics.h"
// #include "MagLog.h"
// #include "MagicsGlobal.h"
// #include "MagicsParameter.h"
// #include "WebFormat.h"
// #include "py_calls.h"

// extern "C" {
// #include "magics_api.h"
// }

using namespace magics;

static std::string _last_error;

static void last_error(const std::string& error) {
    _last_error = error;
}

static void last_error(std::exception& e) {
    last_error(e.what());
}

static void clear_error() {
    last_error("");
}

static const char* last_error() {
    return _last_error.size() ? _last_error.c_str() : nullptr;
}

template <class T>
const char* python_void(const char* name, T proc) {
    clear_error();
    try {
        proc();
    }
    catch (std::exception& e) {
        last_error(e);
        std::cout << "EXCEPTION in py_" << name << "(): " << e.what() << std::endl;
    }
    catch (...) {
        std::cout << "EXCEPTION in py_" << name << "(): unknown" << std::endl;
        last_error("Unknown exception");
    }
    return last_error();
}

template <class T>
const char* python_char(const char* name, T proc) {
    clear_error();

    try {
        return proc();
    }
    catch (std::exception& e) {
        last_error(e);
        std::cout << "EXCEPTION in py_" << name << "(): " << e.what() << std::endl;
    }
    catch (...) {
        std::cout << "EXCEPTION in py_" << name << "(): unknown" << std::endl;
        last_error("Unknown exception");
    }

    return nullptr;
}


extern "C" {


MAGICS_EXPORT const char* mag_error() {
    return last_error();
}

MAGICS_EXPORT const char* mag_home() {
    return MagicsCalls::home();
}

MAGICS_EXPORT const char* home() {
    return MagicsCalls::home();
}

#define PYTHON_VOID(NAME) \
    MAGICS_EXPORT const char* py_##NAME() { return python_void(#NAME, MagicsCalls::NAME); }

#define PYTHON_CHAR(NAME) \
    MAGICS_EXPORT const char* py_##NAME() { return python_char(#NAME, MagicsCalls::NAME); }


PYTHON_VOID(axis)
PYTHON_VOID(boxplot)
PYTHON_VOID(close)
PYTHON_VOID(coast)
PYTHON_VOID(cont)
PYTHON_VOID(eps)
PYTHON_VOID(epsbar)
PYTHON_VOID(epscloud)
PYTHON_VOID(epsgraph)
PYTHON_VOID(epsinput)
PYTHON_VOID(epslight)
PYTHON_VOID(epsplumes)
PYTHON_VOID(epsshading)
PYTHON_VOID(epswave)
PYTHON_VOID(epswind)
PYTHON_VOID(geo)
PYTHON_VOID(geojson)
PYTHON_VOID(graph)
PYTHON_VOID(grib)
PYTHON_VOID(image)
PYTHON_VOID(import)
PYTHON_VOID(info)
PYTHON_VOID(input)
PYTHON_VOID(keep_compatibility)  // TODO: review name
PYTHON_VOID(legend)
PYTHON_VOID(line)
PYTHON_VOID(mapgen)
PYTHON_VOID(metbufr)
PYTHON_VOID(metgraph)
PYTHON_VOID(mute)
PYTHON_VOID(netcdf)
PYTHON_VOID(obs)
PYTHON_VOID(odb)
PYTHON_VOID(open)
PYTHON_VOID(overlay)
PYTHON_VOID(plot)
PYTHON_VOID(print)
PYTHON_VOID(raw)
PYTHON_VOID(set_python)  // TODO: review name
PYTHON_VOID(strict_mode)  // TODO: review name
PYTHON_VOID(symb)
PYTHON_VOID(table)
PYTHON_VOID(taylor)
PYTHON_VOID(tephi)
PYTHON_VOID(text)
PYTHON_VOID(tile)
PYTHON_VOID(unmute)  // TODO: review name
PYTHON_VOID(wind)
PYTHON_VOID(wrepjson)

PYTHON_CHAR(knowndrivers)  // TODO: review name
PYTHON_CHAR(metagrib)      // TODO: review name
PYTHON_CHAR(metainput)     // TODO: review name
PYTHON_CHAR(metanetcdf)    // TODO: review name


MAGICS_EXPORT const char* py_detect(const char* json, const char* dim) {
    return python_char("detect", [json, dim] { return MagicsCalls::detect(json, dim); });
}

MAGICS_EXPORT const char* py_new(const char* page) {
    return python_void("new", [page] { MagicsCalls::page(page); });
}

MAGICS_EXPORT const char* py_reset(const char* name) {
    return python_void("reset", [name] { MagicsCalls::reset(name); });
}

MAGICS_EXPORT const char* py_setc(const char* name, const char* value) {
    return python_void("setc", [name, value] { MagicsCalls::setc(name, value); });
}

MAGICS_EXPORT const char* py_setr(const char* name, const double value) {
    return python_void("setr", [name, value] { MagicsCalls::setr(name, value); });
}

MAGICS_EXPORT const char* py_seti(const char* name, const int value) {
    return python_void("seti", [name, value] { MagicsCalls::seti(name, value); });
}

MAGICS_EXPORT const char* py_set1r(const char* name, const double* data, const int dim1) {
    return python_void("set1r", [name, data, dim1] { MagicsCalls::set1r(name, data, dim1); });
}

MAGICS_EXPORT const char* py_set2r(const char* name, const double* data, const int dim1, const int dim2) {
    return python_void("set2r", [name, data, dim1, dim2] { MagicsCalls::set2r(name, data, dim1, dim2); });
}

MAGICS_EXPORT const char* py_set1i(const char* name, const int* data, const int dim1) {
    return python_void("set1i", [name, data, dim1] { MagicsCalls::set1i(name, data, dim1); });
}

MAGICS_EXPORT const char* py_set2i(const char* name, const int* data, const int dim1, const int dim2) {
    return python_void("set2i", [name, data, dim1, dim2] { MagicsCalls::set2i(name, data, dim1, dim2); });
}

MAGICS_EXPORT const char* py_set1c(const char* name, const char** data, const int dim1) {
    return python_void("set1c", [name, data, dim1] { MagicsCalls::set1c(name, data, dim1); });
}


}  // end of extern "C"
