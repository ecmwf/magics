/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File MagicsParameter.cc
// Magics Team - ECMWF 2004


#include "MagicsParameter.h"

using namespace magics;


template <class T>
void MagicsParameter<T>::print(ostream& out) const {
    out << name_ << "[" << value_ << ", " << default_ << "]";
}
