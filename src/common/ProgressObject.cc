/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File ProgressObject.cc
// Magics Team - ECMWF 2004


#include "ProgressObject.h"
#include "BaseDriver.h"

using namespace magics;

void ProgressObject::redisplay(const BaseDriver& driver) const {
    // driver.redisplay(*this);
}

void ProgressObject::print(ostream&) const {
    // out << "ProgressObject[" << progress_ << "]";
}
void ClearObject::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*this);
}

void ClearObject::print(ostream&) const {}
