/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "NumberContent.h"
#include "MagExceptions.h"
#include "JSON.h"
#include "Translator.h"
#include "DoubleContent.h"

#include <iostream>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


class BadBoolConversion : public MagException {
public:
    BadBoolConversion(const std::string& w) : MagException(std::string("Bad Bool Conversion: ") + w) {}
};

//----------------------------------------------------------------------------------------------------------------------


NumberContent::NumberContent(long long l) : value_(l) {}



Content* NumberContent::clone() const {
    return new NumberContent(value_);
}


NumberContent::~NumberContent() {}

void NumberContent::print(std::ostream& s) const {
    s << value_;
}

void NumberContent::json(JSON& s) const {
    s << value_;
}

int NumberContent::compare(const Content& other) const {
    return -other.compareNumber(*this);
}

int NumberContent::compareNumber(const NumberContent& other) const {
    long long dif = (value_ - other.value_);
    if (dif == 0)
        return dif;
    if (dif < 0)
        return -1;

    return 1;
}

int NumberContent::compareDouble(const DoubleContent& other) const {
    double dif = (value_ - other.value_);
    if (dif == 0)
        return dif;
    if (dif < 0)
        return -1;
    return 1;
}

void NumberContent::value(bool& b) const {
    if (value_ == 0)
        b = false;
    else
        b = true;
}

void NumberContent::value(long long& l) const {
    l = value_;
}

void NumberContent::value(double& l) const {
    l = value_;
}

void NumberContent::value(std::string& s) const {
    s = Translator<long long, std::string>()(value_);
}

Content* NumberContent::add(const Content& other) const {
    return other.addNumber(*this);
}

Content* NumberContent::addNumber(const NumberContent& other) const {
    return new NumberContent(other.value_ + value_);
}

Content* NumberContent::sub(const Content& other) const {
    return other.subNumber(*this);
}

Content* NumberContent::subNumber(const NumberContent& other) const {
    return new NumberContent(other.value_ - value_);
}

Content* NumberContent::mul(const Content& other) const {
    return other.mulNumber(*this);
}

Content* NumberContent::mod(const Content& other) const {
    return other.modNumber(*this);
}

Content* NumberContent::mulNumber(const NumberContent& other) const {
    return new NumberContent(other.value_ * value_);
}

Content* NumberContent::div(const Content& other) const {
    return other.divNumber(*this);
}

Content* NumberContent::divNumber(const NumberContent& other) const {
    return new NumberContent(other.value_ / value_);
}

Value NumberContent::negate() const {
    return Value(-value_);
}

void NumberContent::dump(std::ostream& out, size_t depth, bool indent) const {
    if (indent) {
        while (depth-- > 0) {
            out << ' ';
        }
    }
    out << "number(" << value_ << ")";
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
