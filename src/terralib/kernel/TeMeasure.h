/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeMeasure.h
    \brief This file defines a class for handling a measure
*/

#ifndef __TERRALIB_INTERNAL_MEASURE_H
#define __TERRALIB_INTERNAL_MEASURE_H

//! A class for handling a measure
/*!
    Provide support and definition for a measured value for All application that
    require a field measure.
    \sa TeSample, TeCountourLine
*/

#include "TeDefines.h"
class TL_DLL TeMeasure {
public:
    //! Constructor
    TeMeasure(double value) : value_(value) {}

    //! Destructor
    ~TeMeasure() {}

    //! Return the value associated to the measure
    double value() { return value_; }

protected:
    double value_;
};


// typedef TeComposite <TeMeasure> TeMeasureComposite;

#endif
