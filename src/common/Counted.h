/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file Counted.h
/// @date Jun 1996
/// @author Baudouin Raoult
/// @author Tiago Quintino

#ifndef magics_Counted_h
#define magics_Counted_h

#include <stdlib.h>

namespace magics {

class Counted {
public:  // methods
    void attach() const { count_++; }

    void detach() const {
        if (--count_ == 0) {
            delete this;
        }

    }

    size_t count() const { return count_; }

public:
    Counted() : count_(0) {}

    virtual ~Counted();

private:  // members
    mutable size_t count_;
};

//----------------------------------------------------------------------------------------------------------------------


}  // namespace magics

#endif
