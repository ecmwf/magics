/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeException.h
    \brief This file deals Eexception throwing in TerraLib
*/
#ifndef __TERRALIB_INTERNAL_EXCEPTION_H
#define __TERRALIB_INTERNAL_EXCEPTION_H

#ifdef WIN32
#pragma warning(disable : 4355)
#endif

#include "TeErrorLog.h"
#include "TeSingleton.h"

#include <map>
#include <string>
using namespace std;


//!  Provides a class for handling exceptions on Terralib
class TL_DLL TeException {
public:
    // -- Contructors

    TeException(TeErrorType msgCode, const string& userText = "", bool hasErrno = false);

    // -- Destructor

    virtual ~TeException() {}  // base class

    // --  Members

    string message() const;
    TeErrorType code() const;

protected:
    // -- Members

    //! code associated with TeException
    TeErrorType msgCode_;

    //! message associated with TeException
    string userText_;

private:
};
#endif
