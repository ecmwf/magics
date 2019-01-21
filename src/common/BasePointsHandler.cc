/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasePointsHandler.h
    \brief Definition of the Template base class AbstractPoints.

    Magics Team - ECMWF 2004

    Started: Fri 16-Jan-2004

    Changes:

*/

#include "BasePointsHandler.h"

using namespace magics;

MinMaxHelper::MinMaxHelper(AbstractPoints& points) :
    minX_(INT_MAX),
    maxX_(INT_MIN),
    minY_(INT_MAX),
    maxY_(INT_MIN),
    min_(INT_MAX),
    max_(INT_MIN) {
    points.for_each(*this);
    points.setToFirst();
}
