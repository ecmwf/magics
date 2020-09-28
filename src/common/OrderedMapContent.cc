/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <algorithm>

#include "OrderedMapContent.h"
#include "JSON.h"
#include "MagException.h"

namespace magics {

//----------------------------------------------------------------------------------------------------------------------



OrderedMapContent::OrderedMapContent() {}

OrderedMapContent::OrderedMapContent(const ValueMap& v, const ValueList& keys) : value_(v) {
    ASSERT(keys.size() == value_.size());
    keys_ = keys;
}



OrderedMapContent::~OrderedMapContent() {}

void OrderedMapContent::value(ValueMap& v) const {
    v.clear();
    for (ValueList::const_iterator j = keys_.begin(); j != keys_.end(); ++j) {
        v[(*j)] = value(*j);
    }
}

const Value& OrderedMapContent::value(const Value& key) const {
    ValueMap::const_iterator j = value_.find(key);
    ASSERT(j != value_.end());
    return (*j).second;
}

Value OrderedMapContent::keys() const {
    return keys_;
}

Value OrderedMapContent::remove(const Value& key) {
    Value result = value_[key];
    value_.erase(key);
    auto it = std::find(keys_.begin(), keys_.end(), key);
    if (it != keys_.end()) keys_.erase(it);
    return result;
}

Value& OrderedMapContent::element(const Value& key) {
    if (value_.find(key) == value_.end()) {  // key is new so add too order list
        keys_.push_back(key);
    }
    return value_[key];
}

bool OrderedMapContent::contains(const Value& key) const {
    return value_.find(key) != value_.end();
}

int OrderedMapContent::compare(const Content& other) const {
    return -other.compareOrderedMap(*this);
}

int OrderedMapContent::compareOrderedMap(const OrderedMapContent& other) const {
    int b                    = 1;
    const ValueList* shorter = &keys_;
    const ValueList* longer  = &other.keys_;
    bool swap                = keys_.size() > other.keys_.size();
    if (swap) {
        std::swap(shorter, longer);
        b = -1;
    }

    // compare the keys in order
    ValueList::const_iterator jc = (*longer).begin();
    for (ValueList::const_iterator j = shorter->begin(); j != shorter->end(); ++j, ++jc) {

        if (*j == *jc) {
            continue;
        }

        return (*j < *jc) ? -b : b;
    }

    if (keys_.size() != other.keys_.size()) {
        return -b;
    }  // the map with more elements is larger

    // all keys are equal and in same order, compare now the values
    jc = (*longer).begin();
    for (ValueList::const_iterator j = shorter->begin(); j != shorter->end(); ++j, ++jc) {

        const Value& k     = *j;
        const Value& left  = value_.at(k);
        const Value& right = other.value_.at(k);

        if (left == right) {
            continue;
        }

        return (left < right) ? -b : b;
    }

    return 0;  // all keys and values are the same and in same order
}

void OrderedMapContent::print(std::ostream& s) const {
    s << '{';
    for (ValueList::const_iterator j = keys_.begin(); j != keys_.end(); ++j) {
        if (j != keys_.begin())
            s << " , ";
        s << *j;
        s << " => ";
        s << value(*j);
    }
    s << '}';
}

void OrderedMapContent::json(JSON& s) const {
    s.startObject();
    for (ValueList::const_iterator j = keys_.begin(); j != keys_.end(); ++j) {
        s << *j;
        s << value(*j);
    }
    s.endObject();
}


Content* OrderedMapContent::clone() const {

    OrderedMapContent* m = new OrderedMapContent();

    for (ValueMap::const_iterator j = value_.begin(); j != value_.end(); ++j) {
        m->element((*j).first.clone()) = (*j).second.clone();
    }

    return m;
}

Content* OrderedMapContent::add(const Content& other) const {
    return other.addOrderedMap(*this);
}

Content* OrderedMapContent::sub(const Content& other) const {
    return other.subOrderedMap(*this);
}

Content* OrderedMapContent::mul(const Content& other) const {
    return other.mulOrderedMap(*this);
}

Content* OrderedMapContent::div(const Content& other) const {
    return other.divOrderedMap(*this);
}

Content* OrderedMapContent::mod(const Content& other) const {
    return other.modOrderedMap(*this);
}


void OrderedMapContent::dump(std::ostream& out, size_t depth, bool indent) const {

    if (indent) {
        size_t n = depth;
        while (n-- > 0) {
            out << ' ';
        }
    }

    out << "{";
    const char* sep = "\n";

    for (ValueList::const_iterator j = keys_.begin(); j != keys_.end(); ++j) {
        out << sep;
        (*j).dump(out, depth + 3);
        out << ": ";
        value(*j).dump(out, depth + 3, false);
        sep = ",\n";
    }

    if (!value_.empty()) {
        out << '\n';
        size_t n = depth;
        while (n-- > 0) {
            out << ' ';
        }
    }

    out << "}";
}



//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
