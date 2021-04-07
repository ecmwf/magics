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

   API for single precision versions of Magics++

*/

#include <vector>
#include "magics_export.h"

extern "C" {

void psetr_double(const char* namep, const double* value, int namel);
void pset1r_double(const char* namep, const double* data, const int* dim, int namel);
void pset2r_double(const char* namep, const double* data, const int* dim1, const int* dim2, int namel);
void pset3r_double(const char* namep, const double* data, const int* dim1, const int* dim2, const int* dim3, int namel);
void penqr_double(const char* namep, double* value, int namel);

MAGICS_EXPORT void psetr_(char* name, float* value, int length) {
    double dval = *value;
    // Here we try to improve the conversion to double for small numbers.
    if (*value < 1.0 && *value > -1.0) {
        int val = *value * 10000000;
        dval    = val / 10000000.;
    }
    psetr_double(name, &dval, length);
}

MAGICS_EXPORT void pset1r_(char* name, float* data, int* dim, int length) {
    std::vector<double> values;
    int size = *dim;
    for (int i = 0; i < size; i++) {
        values.push_back(data[i]);
    }
    pset1r_double(name, values.data(), dim, length);
}

MAGICS_EXPORT void pset2r_(char* name, float* data, int* dim, int* dim2, int length) {
    std::vector<double> values;
    int size = *dim * *dim2;
    for (int i = 0; i < size; i++) {
        values.push_back(data[i]);
    }

    pset2r_double(name, values.data(), dim, dim2, length);
}

MAGICS_EXPORT void pset3r_(char* name, float* data, int* dim, int* dim2, int* dim3, int length) {
    std::vector<double> values;
    int size = *dim * *dim2 * *dim3;
    for (int i = 0; i < size; i++) {
        values.push_back(data[i]);
    }

    pset3r_double(name, values.data(), dim, dim2, dim3, length);
}

MAGICS_EXPORT void penqr_(const char* name, float* value, int length) {
    double tmp;
    penqr_double(name, &tmp, length);
    *value = float(tmp);
}
}
