/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "Value.h"

#include "BoolContent.h"
// #include "DateContent.h"
// #include "DateTimeContent.h"
#include "DoubleContent.h"
#include "ListContent.h"
#include "MapContent.h"
#include "NilContent.h"
#include "NumberContent.h"
#include "OrderedMapContent.h"
#include "StringContent.h"
// #include "TimeContent.h"

//----------------------------------------------------------------------------------------------------------------------

namespace magics {

namespace {

class Nil : public NilContent {
public:
    Nil() { attach(); }  // the only instance of Nil (below) *will* be leaked at_exit()
};

static Nil* nil = 0;  // must be a pointer, so we control when is created to respect order of destruction at_exit()

static Nil* nill() {
    if (!nil)
        nil = new Nil();
    return nil;
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

Value::Value() : content_(nill()) {
    content_->attach();
}

Value::Value(int l) : content_(new NumberContent(l)) {
    content_->attach();
}

Value::Value(long long l) : content_(new NumberContent(l)) {
    content_->attach();
}

Value::Value(unsigned long long l) : content_(new NumberContent(l)) {
    content_->attach();
}

Value::Value(unsigned long l) : content_(new NumberContent(l)) {
    content_->attach();
}


Value::Value(unsigned int l) : content_(new NumberContent(l)) {
    content_->attach();
}

Value::Value(long l) : content_(new NumberContent(l)) {
    content_->attach();
}


Value::Value(bool l) : content_(new BoolContent(l)) {
    content_->attach();
}

Value::Value(double l) : content_(new DoubleContent(l)) {
    content_->attach();
}

Value::Value(const std::string& s) : content_(new StringContent(s)) {
    content_->attach();
}

Value::Value(const char* s) : content_(new StringContent(s)) {
    content_->attach();
}


Value::~Value() {
    content_->detach();
}

Value::Value(const Value& other) : content_(other.content_) {
    content_->attach();
}

Value Value::clone() const {
    return Value(content_->clone());
}


Value& Value::operator=(const Value& other) {
    Content* current = content_;

    content_ = other.content_;
    content_->attach();
    current->detach();

    return *this;
}


Value Value::operator+(const Value& v) const {
    return Value(content_->add(*(v.content_)));
}

Value& Value::operator+=(const Value& v) {
    *this = *this + v;
    return *this;
}

Value Value::operator-(const Value& v) const {
    return Value(content_->sub(*(v.content_)));
}

Value& Value::operator-=(const Value& v) {
    *this = *this - v;
    return *this;
}

Value Value::operator*(const Value& v) const {
    return Value(content_->mul(*(v.content_)));
}

Value& Value::operator*=(const Value& v) {
    *this = *this * v;
    return *this;
}

Value Value::operator/(const Value& v) const {
    return Value(content_->div(*(v.content_)));
}

Value& Value::operator/=(const Value& v) {
    *this = *this / v;
    return *this;
}

Value Value::operator%(const Value& v) const {
    return Value(content_->mod(*(v.content_)));
}

Value& Value::operator%=(const Value& v) {
    *this = *this % v;
    return *this;
}

Value Value::makeList() {
    return Value(new ListContent());
}

Value Value::makeMap() {
    return Value(new MapContent());
}

Value Value::makeOrderedMap() {
    return Value(new OrderedMapContent());
}

Value Value::makeMap(const ValueMap& m) {
    return Value(new MapContent(m));
}

Value Value::makeOrderedMap(const ValueMap& m, const ValueList& l) {
    return Value(new OrderedMapContent(m, l));
}

Value Value::makeList(const Value& v) {
    return Value(new ListContent(v));
}

Value Value::makeList(const ValueList& v) {
    return Value(new ListContent(v));
}

Value::Value(const ValueList& v) : content_(new ListContent(v)) {
    content_->attach();
}

Value::Value(const ValueMap& m) : content_(new MapContent(m)) {
    content_->attach();
}

Value::Value(Content* c) : content_(c) {
    content_->attach();
}

Value Value::head() const {
    ValueList v;
    content_->value(v);

    return v.size() > 0 ? v[0] : Value();
}

Value Value::tail() const {
    ValueList v;
    content_->value(v);

    if (v.size() > 1) {
        v.erase(v.begin());
        return v;
    }
    else {
        return Value();
    }
}

Value::operator ValueList() const {
    ValueList v;
    content_->value(v);
    return v;
}

Value::operator ValueMap() const {
    ValueMap v;
    content_->value(v);
    return v;
}


bool Value::contains(const Value& key) const {
    return content_->contains(key);
}

bool Value::contains(const char* key) const {
    return content_->contains(key);
}

bool Value::contains(const std::string& key) const {
    return content_->contains(key);
}

bool Value::contains(int key) const {
    return content_->contains(key);
}

Value Value::operator-() const {
    return content_->negate();
}

Value Value::keys() const {
    return content_->keys();
}

size_t Value::size() const {
    return content_->size();
}

std::ostream& Value::dump(std::ostream& out, size_t depth, bool indent) const {
    content_->dump(out, depth, indent);
    return out;
}

std::string Value::typeName() const {
    return content_->typeName();
}

Value Value::operator[](const char* key) const {
    return element(Value(key));
}

Value Value::operator[](const std::string& key) const {
    return element(Value(key));
}

Value Value::operator[](const Value& key) const {
    return element(key);
}

Value Value::operator[](int key) const {
    return element(Value(key));
}

Value& Value::operator[](const char* key) {
    return element(Value(key));
}

Value& Value::operator[](const std::string& key) {
    return element(Value(key));
}

Value& Value::operator[](const Value& key) {
    return element(key);
}

Value& Value::operator[](int key) {
    return element(Value(key));
}

Value& Value::element(const Value& key) {
    update();
    return content_->element(key);
}

Value Value::element(const Value& key) const {
    return content_->element(key);
}

Value Value::remove(const Value& key) {
    update();
    return content_->remove(key);
}

void Value::update() {
    if (content_->count() > 1) {
        Content* c = content_->clone();
        c->attach();
        content_->detach();
        content_ = c;
    }
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
