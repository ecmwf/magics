/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Exception
// Sylvie Lamy-Thepaut - ECMWF Mar 02
#include "MagException.h"
#include <string.h>

using namespace magics;


MagicsException::MagicsException(const std::string& why) : what_(why) {
    if (std::getenv("MAGICS_ABORT_EXCEPTION")) {
        std::abort();
    }
}


AssertionFailed::AssertionFailed(const char* msg, int line, const char* file, const char* proc) {
    ostringstream s;

    s << "Assertion failed: " << msg << " in " << proc << ", line " << line << " of " << file;

    reason(s.str());
}

CannotOpenFile::CannotOpenFile(const std::string& path) {
#ifdef MAGICS_ON_WINDOWS
    const char* estr = _strerror(NULL);
#else
    char estr[256];
    strerror_r(errno, estr, sizeof(estr));
#endif
    reason("Cannot open file " + path + ": " + string(estr));
}

NotYetImplemented::NotYetImplemented(const std::string& type, const std::string& method) :
    MagicsException(type + " " + method + " : not yet implemented... ") {}

MethodNotYetImplemented::MethodNotYetImplemented(const std::string& method) : NotYetImplemented("Method", method) {}

ParameterNotYetImplemented::ParameterNotYetImplemented(const std::string& param) :
    NotYetImplemented("Parameter", param) {}

NotImplemented::NotImplemented(const std::string& msg) : MagicsException("Not implemented: " + msg) {}

std::string MagicsException::syserror() {
#ifdef MAGICS_ON_WINDOWS
    return _strerror(NULL);
#else
    char estr[256];
    strerror_r(errno, estr, sizeof(estr));
    return estr;
#endif
}
