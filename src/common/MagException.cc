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
#include <string.h>
#include <MagException.h>

using namespace magics;


AssertionFailed::AssertionFailed(const char* msg, int line, const char* file, const char* proc) {
    ostringstream s;

    s << "Assertion failed: " << msg << " in " << proc << ", line " << line << " of " << file;

    what_ = s.str();
}

CannotOpenFile::CannotOpenFile(const std::string& path) {
#ifdef MAGICS_ON_WINDOWS
    const char* estr = _strerror(NULL);
#else
    int e = errno;
    char estr[256];
    strerror_r(errno, estr, sizeof(estr));
#endif
    reason("Cannot open file " + path + ": " + string(estr));

}
