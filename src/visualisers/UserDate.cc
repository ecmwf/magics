/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file UserDate.cc
    \brief Implementation of the Template class UserDate.

    Magics Team - ECMWF 2005

    Started: Mon 10-Oct-2005

    Changes:

*/


#include "UserDate.h"
#include "MagDateTime.h"

using namespace magics;

UserDate::UserDate() {}

UserDate::UserDate(const string& date) {
    MagLog::dev() << "New UserDate--->" << date << "\n";

    DateTime test(date);

    MagLog::dev() << "testing" << test << "\n";
}

UserDate::~UserDate() {}

/*!
 Class information are given to the output-stream.
*/
void UserDate::print(ostream& out) const {
    out << "UserDate[";
    out << "]";
}
