/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File ProgressTask.cc
// Magics Team - ECMWF 2004


#include "ProgressTask.h"
#include "ProgressObject.h"

using namespace magics;

ProgressTask::ProgressTask() {
    push_back(new ProgressObject("dealing with Node ...."));
}


ProgressTask::~ProgressTask() {}


void ProgressTask::print(ostream& out) const {
    out << "ProgressTask";
}
