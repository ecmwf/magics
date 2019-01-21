/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeSingleton.h
    \brief This file contains a template for the "Singleton" pattern.
*/
#ifndef __TERRALIB_INTERNAL_SINGLETON_H
#define __TERRALIB_INTERNAL_SINGLETON_H

//!  Provides a template support for the "Singleton" pattern
/*!
    \note See "Design Patterns" book, page 127
*/

#include "TeDefines.h"

template <class T>
class TeSingleton {
public:
    static T& instance() {
        static T instance_;
        return instance_;
    }

    // -- Destructor

    virtual ~TeSingleton() {}  // base class

protected:
    // -- Contructors

    TeSingleton() {}


private:
    // No copy allowed

    TeSingleton(const TeSingleton&);
    TeSingleton& operator=(const TeSingleton&) { return *this; }
};

#endif
