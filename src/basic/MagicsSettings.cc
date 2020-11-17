/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsSettings.h
    \brief Implementation of Global values of Magics.
    \author Meteorological Visualisation Section, ECMWF

    Started: JUme 2020
*/


#include "MagicsSettings.h"
#include "magics.h"

namespace magics {

bool MagicsSettings::silent() {
    return instance().silent_;
}

void MagicsSettings::silent(bool s) {
    instance().silent_ = s;
}

bool MagicsSettings::strict() {
    return instance().strict_;
}

void MagicsSettings::strict(bool s) {
    if(s) {
        instance().compatibility_ = false;
    }
    instance().strict_ = s;
}

bool MagicsSettings::compatibility() {
    return instance().compatibility_;
}

void MagicsSettings::compatibility(bool c) {
    instance().compatibility_ = c;
}

MagicsSettings& MagicsSettings::instance() {
    static MagicsSettings g;
    return g;
}

}  // namespace magics
