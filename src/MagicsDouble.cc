/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*

   API for double precision versions of Magics++

*/

#include <iostream>

extern "C" {
#include <magics_api.h>


MAGICS_EXPORT void psetr_(char* name, double* value, int length) {
    std::string n(name, length);
    mag_setr(n.c_str(), *value);
}

MAGICS_EXPORT void pset1r_(char* name, double* data, int* dim, int length) {
    std::string n(name, length);
    mag_set1r(n.c_str(), data, *dim);
}

MAGICS_EXPORT void pset2r_(char* name, double* data, int* dim, int* dim2, int length) {
    std::string n(name, length);
    mag_set2r(n.c_str(), data, *dim, *dim2);
}

MAGICS_EXPORT void pset3r_(char* name, double* data, int* dim, int* dim2, int* dim3, int length) {
    std::string n(name, length);
    mag_set3r(n.c_str(), data, *dim, *dim2, *dim3);
}

MAGICS_EXPORT void penqr_(char* name, double* value, int length) {
    std::string n(name, length);
    mag_enqr(n.c_str(), value);
}
}
