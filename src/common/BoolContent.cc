/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "BoolContent.h"
#include "JSON.h"
#include "Translator.h"
#include <iostream>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


BoolContent::BoolContent(bool l) : value_(l) {}

Content* BoolContent::clone() const {
    return new BoolContent(value_);
}


BoolContent::~BoolContent() {}

void BoolContent::print(std::ostream& s) const {
    s << (value_ ? "true" : "false");
}

void BoolContent::json(JSON& s) const {
    s << value_;
}

int BoolContent::compare(const Content& other) const {
    return -other.compareBool(*this);
}

int BoolContent::compareBool(const BoolContent& other) const {
    bool equal = !(value_ - other.value_);

    if (equal)
        return 0;  // both equal in value, hence 0

    if (!value_)
        return -1;  // this is false, hence smaller than other

    return 1;  // this is true, hence larger than other
}

void BoolContent::value(bool& l) const {
    l = value_;
}

void BoolContent::value(std::string& s) const {
    s = value_ ? "true" : "false";
}

void BoolContent::dump(std::ostream& out, size_t depth, bool indent) const {
    if (indent) {
        while (depth-- > 0) {
            out << ' ';
        }
    }

    out << (value_ ? "true" : "false");
}

void BoolContent::value(long long& l) const {
    l = value_;
}

void BoolContent::value(double& d) const {
    d = value_;
}

Content* BoolContent::add(const Content& other) const {
    return other.addBool(*this);
}


Content* BoolContent::sub(const Content& other) const {
    return other.subBool(*this);
}

Content* BoolContent::mul(const Content& other) const {
    return other.mulBool(*this);
}

Content* BoolContent::div(const Content& other) const {
    return other.divBool(*this);
}

Content* BoolContent::mod(const Content& other) const {
    return other.modBool(*this);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
