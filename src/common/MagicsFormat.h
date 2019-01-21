/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagicsFormat_H
#define MagicsFormat_H
#include "magics.h"


namespace magics {

class MagicsFormat {
public:
    MagicsFormat(const string& format, double value) : format_(format), value_(value) {}
    virtual ~MagicsFormat() {}
    bool valid(ostream& out) const;

    virtual ostream& format(ostream& out) const;


    friend ostream& operator<<(ostream& out, const MagicsFormat& manip) { return manip.format(out); }
    string format_;
    double value_;
};


}  // namespace magics

#endif
