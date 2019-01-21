/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasicSceneVisitor.cc
    \brief Implementation of the Template class BasicSceneVisitor.

    Magics Team - ECMWF 2008

    Started: Fri 19-Dec-2008

    Changes:

*/


#include "BasicSceneVisitor.h"

using namespace magics;

BasicSceneVisitor::BasicSceneVisitor() {}


BasicSceneVisitor::~BasicSceneVisitor() {}

/*!
 Class information are given to the output-stream.
*/
void BasicSceneVisitor::print(ostream& out) const {
    out << "BasicSceneVisitor[";
    out << "]";
}
