/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file RasterData.cc
    \brief Implementation of the Template class RasterData.

    Magics Team - ECMWF 2005

    Started: Tue 12-Apr-2005

    Changes:

*/

#include "RasterData.h"

using namespace magics;

template <class P>
RasterData<P>::RasterData() : projection_(0) {}

template <class P>
RasterData<P>::~RasterData() {
    if (projection_)
        delete projection_;
}

/*!
 Class information are given to the output-stream.
*/
template <class P>
void RasterData<P>::print(ostream& out) const {
    out << "RasterData<P>[";
    out << "xRes=" << x_;
    out << ", yRes=" << y_;
    out << ", columns=" << columns_;
    out << ", rows=" << rows_;
    out << ", lower_left=(" << lowerLeft_.y() << ", " << lowerLeft_.x() << ")";
    out << ", upper_right=(" << upperRight_.y() << ", " << upperRight_.x() << ")";
    out << ", " << size() << " points]";
}
