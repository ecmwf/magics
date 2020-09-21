/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Content.h
// Manuel Fuentes - ECMWF Jun 96

#ifndef magics_Content_h
#define magics_Content_h

#include <map>
#include <string>
#include <vector>

#include "Counted.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

// List here all the Content's

class DoubleContent;
class BoolContent;
class NumberContent;
class StringContent;
class NilContent;
class ListContent;
class MapContent;
class OrderedMapContent;
class Value;
class JSON;

class ValueList: public std::vector<Value>{};
class ValueMap: public std::map<Value, Value>{};

//----------------------------------------------------------------------------------------------------------------------

// Assuptions for comparisons:
// Nil < Number < String < List

class Content : public Counted {
public:
    // Double-dispatching on subclasses
    // Needs a compare??? for every new subclass

    virtual int compareBool(const BoolContent&) const;
    virtual int compareNumber(const NumberContent&) const;
    virtual int compareDouble(const DoubleContent&) const;
    virtual int compareString(const StringContent&) const;
    virtual int compareNil(const NilContent&) const;
    virtual int compareList(const ListContent&) const;
    virtual int compareMap(const MapContent&) const;
    virtual int compareOrderedMap(const OrderedMapContent&) const;

    // Double-dispatching on subclasses for addition
    // Needs an add??? for every new subclass

    virtual Content* addNumber(const NumberContent&) const;
    virtual Content* addBool(const BoolContent&) const;
    virtual Content* addDouble(const DoubleContent&) const;
    virtual Content* addString(const StringContent&) const;
    virtual Content* addNil(const NilContent&) const;
    virtual Content* addList(const ListContent&) const;
    virtual Content* addMap(const MapContent&) const;
    virtual Content* addOrderedMap(const OrderedMapContent&) const;

    virtual Content* subNumber(const NumberContent&) const;
    virtual Content* subDouble(const DoubleContent&) const;
    virtual Content* subBool(const BoolContent&) const;
    virtual Content* subString(const StringContent&) const;
    virtual Content* subNil(const NilContent&) const;
    virtual Content* subList(const ListContent&) const;
    virtual Content* subMap(const MapContent&) const;
    virtual Content* subOrderedMap(const OrderedMapContent&) const;

    virtual Content* mulNumber(const NumberContent&) const;
    virtual Content* mulDouble(const DoubleContent&) const;
    virtual Content* mulBool(const BoolContent&) const;
    virtual Content* mulString(const StringContent&) const;
    virtual Content* mulNil(const NilContent&) const;
    virtual Content* mulList(const ListContent&) const;
    virtual Content* mulMap(const MapContent&) const;
    virtual Content* mulOrderedMap(const OrderedMapContent&) const;

    virtual Content* divNumber(const NumberContent&) const;
    virtual Content* divDouble(const DoubleContent&) const;
    virtual Content* divBool(const BoolContent&) const;
    virtual Content* divString(const StringContent&) const;
    virtual Content* divNil(const NilContent&) const;
    virtual Content* divList(const ListContent&) const;
    virtual Content* divMap(const MapContent&) const;
    virtual Content* divOrderedMap(const OrderedMapContent&) const;

    virtual Content* modNumber(const NumberContent&) const;
    virtual Content* modDouble(const DoubleContent&) const;
    virtual Content* modBool(const BoolContent&) const;
    virtual Content* modString(const StringContent&) const;
    virtual Content* modNil(const NilContent&) const;
    virtual Content* modList(const ListContent&) const;
    virtual Content* modMap(const MapContent&) const;
    virtual Content* modOrderedMap(const OrderedMapContent&) const;


protected:
    // -- Constructor

    Content();

    // -- Destructor

    virtual ~Content();

    // -- Operators

    bool operator==(const Content&) const;
    bool operator<(const Content&) const;


    //  void *operator new(size_t s)  { return MemoryPool::fastAllocate(s);}
    //  void operator delete(void* p) { MemoryPool::fastDeallocate(p);     }

    // -- Methods

    virtual void value(bool&) const;
    virtual void value(long long&) const;
    virtual void value(double&) const;
    virtual void value(std::string&) const;
    virtual void value(ValueList&) const;
    virtual void value(ValueMap&) const;

    Content* operator+(const Content&) const;
    Content* operator-(const Content&) const;
    Content* operator*(const Content&)const;
    Content* operator/(const Content&) const;


    // -- Methods

    virtual void print(std::ostream&) const                            = 0;
    virtual void dump(std::ostream&, size_t, bool indent = true) const = 0;

    virtual std::string typeName() const = 0;
    virtual void json(JSON&) const       = 0;
    virtual Content* clone() const       = 0;

    virtual bool isNil() const { return false; }
    virtual bool isNumber() const { return false; }
    virtual bool isBool() const { return false; }
    virtual bool isDouble() const { return false; }
    virtual bool isString() const { return false; }
    virtual bool isList() const { return false; }
    virtual bool isMap() const { return false; }
    virtual bool isOrderedMap() const { return false; }


    virtual bool contains(const Value&) const;
    virtual Value& element(const Value&);
    virtual Value remove(const Value&);

    virtual Value keys() const;
    virtual Value negate() const;
    virtual size_t size() const;



private:
    // -- No copy allowed

    Content(const Content&);
    Content& operator=(const Content&);

    // -- Class members


    // -- Methods

    virtual int compare(const Content&) const  = 0;
    virtual Content* add(const Content&) const = 0;
    virtual Content* sub(const Content&) const = 0;
    virtual Content* mul(const Content&) const = 0;
    virtual Content* div(const Content&) const = 0;
    virtual Content* mod(const Content&) const = 0;

    void badConversion(const std::string&) const;
    void badComparison(const std::string&) const;
    void badOperator(const std::string&, const std::string&) const;

    // -- Friends

    friend std::ostream& operator<<(std::ostream& s, const Content& content) {
        content.print(s);
        return s;
    }
    friend JSON& operator<<(JSON& s, const Content& content) {
        content.json(s);
        return s;
    }

    friend class Value;
};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace eckit

#endif  // eckit_Content_h
