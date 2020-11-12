/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "DoubleContent.h"
#include "JSON.h"
#include "NumberContent.h"
#include "Translator.h"

#include <iostream>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


DoubleContent::DoubleContent(double l) : value_(l) {}


Content* DoubleContent::clone() const {
    return new DoubleContent(value_);
}


DoubleContent::~DoubleContent() {}

void DoubleContent::print(std::ostream& s) const {
    s << value_;
}

void DoubleContent::json(JSON& s) const {
    s << value_;
}


int DoubleContent::compare(const Content& other) const {
    return -other.compareDouble(*this);
}

int DoubleContent::compareDouble(const DoubleContent& other) const {
    double diff = (value_ - other.value_);
    if (diff == 0) {
        return 0;
    }

    return diff > 0 ? 1 : -1;
}

int DoubleContent::compareNumber(const NumberContent& other) const {
    double diff = (value_ - other.value_);

    if (diff == 0) {
        return 0;
    }

    return diff > 0 ? 1 : -1;
}

void DoubleContent::value(double& l) const {
    l = value_;
}

void DoubleContent::value(std::string& s) const {
    s = Translator<double, std::string>()(value_);
}

Content* DoubleContent::add(const Content& other) const {
    return other.addDouble(*this);
}

Content* DoubleContent::addDouble(const DoubleContent& other) const {
    return new DoubleContent(other.value_ + value_);
}

Content* DoubleContent::sub(const Content& other) const {
    return other.subDouble(*this);
}

Content* DoubleContent::subDouble(const DoubleContent& other) const {
    return new DoubleContent(other.value_ - value_);
}

Content* DoubleContent::mul(const Content& other) const {
    return other.mulDouble(*this);
}

Content* DoubleContent::mulDouble(const DoubleContent& other) const {
    return new DoubleContent(other.value_ * value_);
}

Content* DoubleContent::div(const Content& other) const {
    return other.divDouble(*this);
}

Content* DoubleContent::mod(const Content& other) const {
    return other.modDouble(*this);
}


Content* DoubleContent::divDouble(const DoubleContent& other) const {
    return new DoubleContent(other.value_ / value_);
}

Value DoubleContent::negate() const {
    return Value(-value_);
}

void DoubleContent::dump(std::ostream& out, size_t depth, bool indent) const {
    if (indent) {
        while (depth-- > 0) {
            out << ' ';
        }
    }
    out << "double(" << value_ << ")";
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
