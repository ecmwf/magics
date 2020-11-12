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

#include "CompatibilityHelper.h"
#include "FortranMagics.h"
#include "MagLog.h"
#include "MagicsCalls.h"
#include "MagicsParameter.h"
#include "MagicsSettings.h"
#include "WebFormat.h"
#include "magics.h"
#include "magics_api.h"

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


// #include "MagLog.h"
// #include "MagicsSettings.h"
// #include "MagicsParameter.h"
// #include "WebFormat.h"
// #include "py_calls.h"

// extern "C" {
// #include "magics_api.h"
// }

using namespace magics;

template <class T>
void c_void(const char* name, T proc) {
    try {
        proc();
    }
    catch (std::exception& e) {
        MagLog::error() << "EXCEPTION in mag_" << name << "(): " << e.what() << std::endl;
    }
    catch (...) {
        MagLog::error() << "EXCEPTION in mag_" << name << "(): unknown" << std::endl;
    }
}

template <class T>
const char* c_char(const char* name, T proc) {
    try {
        return proc();
    }
    catch (std::exception& e) {
        MagLog::error() << "EXCEPTION in mag_" << name << "(): " << e.what() << std::endl;
    }
    catch (...) {
        MagLog::error() << "EXCEPTION in mag_" << name << "(): unknown" << std::endl;
    }

    return nullptr;
}


extern "C" {


#define C_VOID(NAME) \
    MAGICS_EXPORT void mag_##NAME() { c_void(#NAME, MagicsCalls::NAME); }

#define C_CHAR(NAME) \
    MAGICS_EXPORT const char* mag_##NAME() { return c_char(#NAME, MagicsCalls::NAME); }


C_VOID(axis)
C_VOID(boxplot)
C_VOID(close)
C_VOID(coast)
C_VOID(cont)
C_VOID(eps)
C_VOID(epsbar)
C_VOID(epscloud)
C_VOID(epsgraph)
C_VOID(epsinput)
C_VOID(epslight)
C_VOID(epsplumes)
C_VOID(epsshading)
C_VOID(epswave)
C_VOID(epswind)
C_VOID(geo)
C_VOID(geojson)
C_VOID(graph)
C_VOID(grib)
C_VOID(image)
C_VOID(import)
C_VOID(info)
C_VOID(input)
C_VOID(keep_compatibility)  // TODO: review name
C_VOID(legend)
C_VOID(line)
C_VOID(mapgen)
C_VOID(metbufr)
C_VOID(metgraph)
C_VOID(mute)
C_VOID(netcdf)
C_VOID(obs)
C_VOID(odb)
C_VOID(open)
C_VOID(overlay)
C_VOID(plot)
C_VOID(print)
C_VOID(raw)
C_VOID(set_python)  // TODO: review name
C_VOID(symb)
C_VOID(table)
C_VOID(taylor)
C_VOID(tephi)
C_VOID(text)
C_VOID(tile)
C_VOID(unmute)  // TODO: review name
C_VOID(wind)
C_VOID(wrepjson)

C_CHAR(knowndrivers)  // TODO: review name
C_CHAR(metagrib)      // TODO: review name
C_CHAR(metainput)     // TODO: review name
C_CHAR(metanetcdf)    // TODO: review name

MAGICS_EXPORT void mag_new(const char* page) {
    c_void("new", [page] { MagicsCalls::page(page); });
}

MAGICS_EXPORT void mag_reset(const char* name) {
    c_void("reset", [name] { MagicsCalls::reset(name); });
}

MAGICS_EXPORT void mag_setc(const char* name, const char* value) {
    c_void("setc", [name, value] { MagicsCalls::setc(name, value); });
}

MAGICS_EXPORT void mag_setr(const char* name, const double value) {
    c_void("setr", [name, value] { MagicsCalls::setr(name, value); });
}

MAGICS_EXPORT void mag_seti(const char* name, const int value) {
    c_void("seti", [name, value] { MagicsCalls::seti(name, value); });
}

MAGICS_EXPORT void mag_set1r(const char* name, const double* data, const int dim1) {
    c_void("set1r", [name, data, dim1] { MagicsCalls::set1r(name, data, dim1); });
}

MAGICS_EXPORT void mag_set2r(const char* name, const double* data, const int dim1, const int dim2) {
    c_void("set2r", [name, data, dim1, dim2] { MagicsCalls::set2r(name, data, dim1, dim2); });
}

MAGICS_EXPORT void mag_set1i(const char* name, const int* data, const int dim1) {
    c_void("set1i", [name, data, dim1] { MagicsCalls::set1i(name, data, dim1); });
}

MAGICS_EXPORT void mag_set2i(const char* name, const int* data, const int dim1, const int dim2) {
    c_void("set2i", [name, data, dim1, dim2] { MagicsCalls::set2i(name, data, dim1, dim2); });
}

MAGICS_EXPORT void mag_set1c(const char* name, const char** data, const int dim1) {
    c_void("set1c", [name, data, dim1] { MagicsCalls::set1c(name, data, dim1); });
}

MAGICS_EXPORT void mag_set3r(const char* name, const double* data, const int dim1, const int dim2, const int dim3) {
    c_void("set3r", [name, data, dim1, dim2, dim3] { MagicsCalls::set3r(name, data, dim1, dim2, dim3); });
}

MAGICS_EXPORT void mag_enqr(const char* name, double* value) {
    c_void("enqr", [name, value] { MagicsCalls::enqr(name, value); });
}

MAGICS_EXPORT void mag_enqi(const char* name, int* value) {
    c_void("enqi", [name, value] { MagicsCalls::enqi(name, value); });
}

MAGICS_EXPORT void mag_enqc(const char* name, char* value) {
    c_void("enqc", [name, value] { MagicsCalls::enqc(name, value); });
}

MAGICS_EXPORT void mag_add_warning_listener(void* data, void (*cb)(void*, const char*)) {
    c_void("add_warning_listener", [data, cb] { MagicsCalls::add_warning_listener(data, cb); });
}

MAGICS_EXPORT void mag_add_error_listener(void* data, void (*cb)(void*, const char*)) {
    c_void("add_error_listener", [data, cb] { MagicsCalls::add_error_listener(data, cb); });
}

MAGICS_EXPORT void mag_add_info_listener(void* data, void (*cb)(void*, const char*)) {
    c_void("add_info_listener", [data, cb] { MagicsCalls::add_info_listener(data, cb); });
}

MAGICS_EXPORT void mag_add_debug_listener(void* data, void (*cb)(void*, const char*)) {
    c_void("add_debug_listener", [data, cb] { MagicsCalls::add_debug_listener(data, cb); });
}

C_VOID(clear_listeners)


MAGICS_EXPORT void detect(const char* data, const char* dimension) {
    c_void("detect", [data, dimension] { MagicsCalls::detect(data, dimension); });
}


}  // end of extern "C"
