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

/*! \mainpage Magics++

 \section intro What is Magics++?

 Magics++ is the second generation of a meteorological graphics
 library developed at the <a href="http://www.ecmwf.int"><i>European
 Centre for Medium Range Forecasts (ECMWF)</i></a>. This library is
 developed in C++ and offers output in various formats such as PostScript,
 PDF, PNG, SVG, KML and Qt.

 \section install How-to install

 Before installation you have to compile Magics++. To do so, simply
 unpack the tarball in an appropiate directory and run <i>./configure</i>
 followed by <i>make</i>. You can type <i>./configure --help</i> to get all
 options of configuring Magics++.

 To install type <i>make install</i>. Depending on the
 choosen installation directory you need root permission.

 For more information you should read the <a href="">installation guide</a>.

 \section interfaces Magics interfaces

 Magics++ offers different interfaces to access its graphics
 functionality: C++ (for higher-level applicatiosn such as Metview), C, Fortran 77
 Python and MagML/MagJson. The Fortran interface is intended to be backwards compatible with older
 versions (mainly the the 6.x series) of Magics.

 \section modules More information

 - \ref interpolation "Interpolation and contouring"

 - \ref hilo "Computations of High &amp; Lows"

 - \ref titles "Automatic titles"

 - \ref projections "Geographical projections"

 - \ref drivers "Output drivers"

 - \ref coastlines "Coastlines"

 - \ref colours "Colours"

 - \ref obs "Plotting of observations"

- \ref compatibility "Compatibility to MAGICS 6 for deprecated parameters"

 \section links Links

 - <a href="http://software.ecmwf.int/magics">Magics++ homepage</a>

 - <a href="http://www.ecmwf.int/services/computing/training/material/com_mag.html">Training course</a>

 \section copyright License

 (C) Copyright 1996-2016 ECMWF.

 This software is licensed under the terms of the Apache Licence Version 2.0
 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 In applying this licence, ECMWF does not waive the privileges and immunities
 granted to it by virtue of its status as an intergovernmental organisation nor
 does it submit to any jurisdiction.
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
