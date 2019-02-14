/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file MgQStepItem.cc
    \brief Definition of MgQStepItem
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#include "MgQStepItem.h"

// using namespace magics;

MgQStepItem::MgQStepItem(MgQLayoutItem* parentLayoutItem) : parentLayoutItem_(parentLayoutItem) {
    id_     = -1;
    cached_ = false;
}

MgQStepItem::~MgQStepItem() {}
