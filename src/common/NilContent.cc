/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "NilContent.h"
#include "JSON.h"
#include "Value.h"

#include <iostream>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------



NilContent::NilContent() {}


Content* NilContent::clone() const {
    return new NilContent();
}


NilContent::~NilContent() {}

void NilContent::value(ValueList& v) const {
    v = ValueList();
}

void NilContent::print(std::ostream& out) const {
    out << "(nil)";
}

void NilContent::json(JSON& s) const {
    s.null();
}

int NilContent::compare(const Content& other) const {
    return -other.compareNil(*this);
}

int NilContent::compareNil(const NilContent&) const {
    return 0;  // They're equals
}

Content* NilContent::add(const Content& other) const {
    return other.addNil(*this);
}

Content* NilContent::addNil(const NilContent&) const {
    return (Content*)this;
}

Content* NilContent::sub(const Content& other) const {
    return other.subNil(*this);
}

Content* NilContent::subNil(const NilContent&) const {
    return (Content*)this;
}

Content* NilContent::mul(const Content& other) const {
    return other.mulNil(*this);
}

Content* NilContent::mulNil(const NilContent&) const {
    return (Content*)this;
}

Content* NilContent::div(const Content& other) const {
    return other.divNil(*this);
}

Content* NilContent::divNil(const NilContent&) const {
    return (Content*)this;
}

Content* NilContent::mod(const Content& other) const {
    return other.divNil(*this);
}

bool NilContent::contains(const Value&) const {
    return false;
}

void NilContent::dump(std::ostream& out, size_t depth, bool indent) const {
    if (indent) {
        while (depth-- > 0) {
            out << ' ';
        }
    }
    out << "nil";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
