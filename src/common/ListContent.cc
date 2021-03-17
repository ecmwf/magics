/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "ListContent.h"
#include "JSON.h"
#include "MagException.h"

#include <iostream>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


ListContent::ListContent() {}

ListContent::ListContent(const ValueList& v) {
    std::copy(v.begin(), v.end(), std::back_inserter(value_));
}

ListContent::ListContent(const Value& v) {
    value_.push_back(v);
}

Content* ListContent::clone() const {
    ValueList v;
    v.reserve(value_.size());
    for (size_t i = 0; i < value_.size(); ++i) {
        v.push_back(value_[i].clone());
    }
    return new ListContent(v);
}

ListContent::~ListContent() {}

size_t ListContent::size() const {
    return value_.size();
}

void ListContent::value(ValueList& v) const {
    v = value_;
}

int ListContent::compare(const Content& other) const {
    return -other.compareList(*this);
}

int ListContent::compareList(const ListContent& other) const {
    if (value_ == other.value_) {
        return 0;
    }
    if (value_ < other.value_) {
        return -1;
    }
    return 1;
}

void ListContent::json(JSON& s) const {
    s.startList();

    for (size_t i = 0; i < value_.size(); i++) {
        s << value_[i];
    }

    s.endList();
}


void ListContent::print(std::ostream& s) const {
    s << '(';

    for (size_t i = 0; i < value_.size(); i++) {
        if (i > 0) {
            s << ',';
        }
        s << value_[i];
    }

    s << ')';
}


Content* ListContent::add(const Content& other) const {
    return other.addList(*this);
}

Content* ListContent::addList(const ListContent& other) const {
    ValueList tmp;
    std::copy(other.value_.begin(), other.value_.end(), std::back_inserter(tmp));
    std::copy(value_.begin(), value_.end(), std::back_inserter(tmp));
    return new ListContent(tmp);
}

Content* ListContent::sub(const Content& other) const {
    return other.subList(*this);
}

Content* ListContent::mul(const Content& other) const {
    return other.mulList(*this);
}

Content* ListContent::div(const Content& other) const {
    return other.divList(*this);
}

Content* ListContent::mod(const Content& other) const {
    return other.modList(*this);
}


void ListContent::value(long long& n) const {
    if (value_.size() == 1) {
        n = value_[0];
    }
    else {
        Content::value(n);
    }
}

void ListContent::value(bool& n) const {
    if (value_.size() == 1) {
        n = value_[0];
    }
    else {
        Content::value(n);
    }
}

void ListContent::value(double& n) const {
    if (value_.size() == 1) {
        n = value_[0];
    }
    else {
        Content::value(n);
    }
}

void ListContent::value(std::string& n) const {
    if (value_.size() == 1) {
        n = std::string(value_[0]);
    }
    else {
        Content::value(n);
    }
}

Value& ListContent::element(const Value& v) {
    long long n = v;

    ASSERT(n >= 0 && (size_t)n < value_.size());
    return value_.at(n);
}

bool ListContent::contains(const Value& v) const {
    long long n = v;
    return (n >= 0 && (size_t)n < value_.size());
}

void ListContent::dump(std::ostream& out, size_t depth, bool indent) const {
    if (indent) {
        size_t n = depth;
        while (n-- > 0) {
            out << ' ';
        }
    }

    out << '[' << std::endl;

    for (size_t i = 0; i < value_.size(); i++) {
        if (i > 0) {
            out << ',' << std::endl;
        }
        value_[i].dump(out, depth + 3);
    }

    out << ']';
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
