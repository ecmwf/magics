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
#include "MagException.h"
#include "MagLog.h"
#include "MagicsCalls.h"
#include "magics.h"

using namespace magics;

// =================================================================

template <class T>
void c_void(const char* name, T proc) {
    try {
        proc();
    }
    catch (std::exception& e) {
        MagLog::error() << "EXCEPTION in " << name << "_(): " << e.what() << std::endl;
    }
    catch (...) {
        MagLog::error() << "EXCEPTION in " << name << "_(): unknown" << std::endl;
    }
}

template <class T>
const char* c_char(const char* name, T proc) {
    try {
        return proc();
    }
    catch (std::exception& e) {
        MagLog::error() << "EXCEPTION in " << name << "_(): " << e.what() << std::endl;
    }
    catch (...) {
        MagLog::error() << "EXCEPTION in " << name << "_(): unknown" << std::endl;
    }

    return nullptr;
}

template <class T>
int c_int(const char* name, T proc) {
    try {
        proc();
        return 0;
    }
    catch (std::exception& e) {
        MagLog::error() << "EXCEPTION in " << name << "_(): " << e.what() << std::endl;
    }
    catch (...) {
        MagLog::error() << "EXCEPTION in " << name << "_(): unknown" << std::endl;
    }

    return -1;
}


extern "C" {


#define C_VOID(NAME) \
    MAGICS_EXPORT void p##NAME##_() { c_void(#NAME, MagicsCalls::NAME); }

#define C_CHAR(NAME) \
    MAGICS_EXPORT const char* NAME##_() { return c_char(#NAME, MagicsCalls::NAME); }

#define C_INT(NAME) \
    MAGICS_EXPORT int pNAME##_() { return c_int(#NAME, MagicsCalls::NAME); }


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
/* **************************************************************************

***
***  Fortran 77 interface
***

****************************************************************************/

static std::string fortran_string(const char* s_ptr, int s_len, bool lstrip = false) {
    std::string s(s_ptr, s_len);
    // remove the space at the start and end of the string
    string::size_type index1 = lstrip ? s.find_first_not_of(" ") : 0;
    string::size_type index2 = s.find_last_not_of(" ");
    return (index1 == string::npos || index2 == string::npos) ? "" : s.substr(index1, index2 + 1);
}

MAGICS_EXPORT void preset_(const char* name) {
    c_void("reset", [name] { MagicsCalls::reset(name); });
}


//================================================================

MAGICS_EXPORT void psetc_(const char* name_ptr, const char* value_ptr, int name_len, int valuel) {
    std::string name  = fortran_string(name_ptr, name_len);
    std::string value = fortran_string(value_ptr, valuel);
    c_void("setc", [name, value] { MagicsCalls::setc(name, value); });
}

MAGICS_EXPORT void pset1c_(const char* name_ptr, const char* value, const int* dim, int name_len, int valuel) {
    std::string name = fortran_string(name_ptr, name_len);
    stringarray values;
    for (int i = 0; i < *dim; i++) {
        values.push_back(fortran_string(value + i * valuel, valuel));
    }
    c_void("set1c", [name, values] { MagicsCalls::set1c(name, values); });
}

//================================================================

MAGICS_EXPORT void pseti_(const char* name_ptr, const int* value, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("seti", [name, value] { MagicsCalls::seti(name, *value); });
}

MAGICS_EXPORT void pset1i_(const char* name_ptr, const int* data, const int* dim, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set1i", [name, data, dim] { MagicsCalls::set1i(name, data, *dim); });
}

MAGICS_EXPORT void pset2i_(const char* name_ptr, const int* data, const int* dim1, const int* dim2, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set2i", [name, data, dim1, dim2] { MagicsCalls::set2i(name, data, *dim1, *dim2); });
}

MAGICS_EXPORT void pset3i_(const char* name_ptr, const int* data, const int* dim1, const int* dim2, const int* dim3,
                           int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set3i", [name, data, dim1, dim2, dim3] { MagicsCalls::set3i(name, data, *dim1, *dim2, *dim3); });
}

//================================================================
MAGICS_EXPORT void psetr_double(const char* name_ptr, const double* value, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("setr", [name, value] { MagicsCalls::setr(name, *value); });
}

MAGICS_EXPORT void pset1r_double(const char* name_ptr, const double* data, const int* dim, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set1r", [name, data, dim] { MagicsCalls::set1r(name, data, *dim); });
}

MAGICS_EXPORT void pset2r_double(const char* name_ptr, const double* data, const int* dim1, const int* dim2,
                                 int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set2r", [name, data, dim1, dim2] { MagicsCalls::set2r(name, data, *dim1, *dim2); });
}

MAGICS_EXPORT void pset3r_double(const char* name_ptr, const double* data, const int* dim1, const int* dim2,
                                 const int* dim3, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("set3r", [name, data, dim1, dim2, dim3] { MagicsCalls::set3r(name, data, *dim1, *dim2, *dim3); });
}

MAGICS_EXPORT void penqr_double(const char* name_ptr, double* value, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("enqr", [name, value] { MagicsCalls::enqr(name, value); });
}

//================================================================

MAGICS_EXPORT void penqi_(const char* name_ptr, int* value, int name_len) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("enqi", [name, value] { MagicsCalls::enqi(name, value); });
}

MAGICS_EXPORT void penqc_(const char* name_ptr, char* value, int name_len, int vlength) {
    std::string name = fortran_string(name_ptr, name_len);
    c_void("enqc", [name, value] { MagicsCalls::enqc(name, value); });

    for (int i = strlen(value); i < vlength; i++) {
        value[i] = ' ';
    }
}

//================================================================


MAGICS_EXPORT const char* detect_(const char* data_ptr, char* dimension_ptr, int data_len, int dimension_len) {
    std::string data      = fortran_string(data_ptr, data_len);
    std::string dimension = fortran_string(dimension_ptr, dimension_len);

    return c_char("detect", [data, dimension] { return MagicsCalls::detect(data, dimension); });
}

}  // end of extern "C"
