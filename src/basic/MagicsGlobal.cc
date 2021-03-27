/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsGlobal.h
    \brief Implementation of Global values of Magics.
    \author Meteorological Visualisation Section, ECMWF

    Started: JUme 2020
*/


#include "MagicsGlobal.h"
#include "magics.h"

namespace magics {

bool MagicsGlobal::silent() {
    return instance().silent_;
}

void MagicsGlobal::silent(bool s) {
    instance().silent_ = s;
}

bool MagicsGlobal::strict() {
    return instance().strict_;
}

void MagicsGlobal::strict(bool s) {
    if(s) {
        instance().compatibility_ = false;
    }
    instance().strict_ = s;
}

bool MagicsGlobal::compatibility() {
    return instance().compatibility_;
}

void MagicsGlobal::compatibility(bool c) {
    instance().compatibility_ = c;
}

MagicsGlobal& MagicsGlobal::instance() {
    static MagicsGlobal g;
    return g;
}

}  // namespace magics
