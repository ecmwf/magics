/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Manuel Fuentes
/// @author Baudouin Raoult
/// @author Tiago Quintino

#ifndef magics_Value_h
#define magics_Value_h

#include <list>

#include "Content.h"


namespace magics {

class Content;
class JSON;

class Value {
public:
    // -- Contructors

    Value();

    Value(bool);
    Value(int);
    Value(long);
    Value(long long);
    Value(unsigned int);
    Value(unsigned long);
    Value(unsigned long long);
    Value(double);

    Value(const std::string&);
    Value(const char*);


    Value(const ValueList&);
    Value(const ValueMap&);

    // -- Copy

    Value(const Value&);
    Value& operator=(const Value&);

    // -- Destructor

    ~Value();

    // -- Operators

    template <typename T>
    T get_value() const {
        T r;
        content_->value(r);
        return r;
    }

    operator short() const {
        long long l;
        content_->value(l);
        return l;
    }

    operator unsigned short() const {
        long long l;
        content_->value(l);
        return l;
    }

    operator int() const {
        long long l;
        content_->value(l);
        return l;
    }
    operator unsigned int() const {
        long long l;
        content_->value(l);
        return l;
    }

    operator long() const {
        long long l;
        content_->value(l);
        return l;
    }
    operator unsigned long() const {
        long long l;
        content_->value(l);
        return l;
    }

    operator long long() const {
        long long l;
        content_->value(l);
        return l;
    }
    operator unsigned long long() const {
        long long l;
        content_->value(l);
        return l;
    }

    operator double() const {
        double d;
        content_->value(d);
        return d;
    }
    operator bool() const {
        bool d;
        content_->value(d);
        return d;
    }

    operator std::string() const {
        std::string s;
        content_->value(s);
        return s;
    }


    operator ValueList() const;
    operator ValueMap() const;

    bool operator<(const Value& v) const { return *content_ < *(v.content_); }
    bool operator==(const Value& v) const { return *content_ == *(v.content_); }

    bool operator>(const Value& v) const { return v < *this; }
    bool operator!=(const Value& v) const { return !(*this == v); }

    bool operator>=(const Value& v) const { return !(*this < v); }
    bool operator<=(const Value& v) const { return !(v < *this); }


    Value operator+(const Value&) const;
    Value& operator+=(const Value&);

    Value operator-() const;

    Value operator-(const Value&) const;
    Value& operator-=(const Value&);

    Value operator*(const Value&)const;
    Value& operator*=(const Value&);

    Value operator/(const Value&) const;
    Value& operator/=(const Value&);

    Value operator%(const Value&) const;
    Value& operator%=(const Value&);

    Value operator[](const char*) const;
    Value operator[](const std::string&) const;
    Value operator[](const Value&) const;
    Value operator[](int) const;

    Value& operator[](const char*);
    Value& operator[](const std::string&);
    Value& operator[](const Value&);
    Value& operator[](int);

    Value keys() const;
    size_t size() const;

    std::ostream& dump(std::ostream& out, size_t depth = 0, bool indent = true) const;

    std::string typeName() const;


public:
    bool contains(const char*) const;
    bool contains(const std::string&) const;
    bool contains(const Value&) const;
    bool contains(int) const;

    Value& element(const Value&);
    Value element(const Value&) const;
    Value remove(const Value&);

    // -- Methods

    int compare(const Value& v) const { return content_->compare(*(v.content_)); }

    bool isNil() const { return content_->isNil(); }
    bool isNumber() const { return content_->isNumber(); }
    bool isBool() const { return content_->isBool(); }
    bool isDouble() const { return content_->isDouble(); }
    bool isString() const { return content_->isString(); }
    bool isList() const { return content_->isList(); }
    bool isMap() const { return content_->isMap(); }
    bool isOrderedMap() const { return content_->isOrderedMap(); }

    Value tail() const;
    Value head() const;

    Value clone() const;
    bool shared() const;  // Ensure that value is not shared

    // -- Class Methods

    static Value makeList();
    static Value makeList(const Value&);
    static Value makeList(const ValueList&);

    static Value makeMap();
    static Value makeMap(const ValueMap&);

    static Value makeOrderedMap();
    static Value makeOrderedMap(const ValueMap&, const ValueList&);


protected:
    Value(Content*);

private:  // members
    Content* content_;

private:  // methods
    void json(JSON& s) const { s << *content_; }
    void print(std::ostream& s) const { s << *content_; }


    void update();

    friend JSON& operator<<(JSON& s, const Value& v) {
        v.json(s);
        return s;
    }

    friend std::ostream& operator<<(std::ostream& s, const Value& v) {
        v.print(s);
        return s;
    }


    friend class Content;
};

//----------------------------------------------------------------------------------------------------------------------


}  // namespace eckit

#endif
