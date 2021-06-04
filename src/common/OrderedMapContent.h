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

#ifndef magics_OrderedMapContent_h
#define magics_OrderedMapContent_h

#include "Value.h"

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


class OrderedMapContent : public Content {

protected:
    // -- Constructor

    OrderedMapContent();
    OrderedMapContent(const ValueMap&, const ValueList&);

    // -- Destructor

    virtual ~OrderedMapContent() override;

    // -- Overridden methods

    // -- From Content

    virtual int compare(const Content& other) const override;

    virtual void value(bool& n) const override { Content::value(n); }
    virtual void value(long long& n) const override { Content::value(n); }
    virtual void value(double& n) const override { Content::value(n); }
    virtual void value(std::string& n) const override { Content::value(n); }
    virtual void value(ValueList& n) const override { Content::value(n); }
    virtual void value(ValueMap& n) const override;

    virtual int compareBool(const BoolContent&) const override { return -1; }
    virtual int compareNumber(const NumberContent&) const override { return -1; }
    virtual int compareDouble(const DoubleContent&) const override { return -1; }
    virtual int compareString(const StringContent&) const override { return -1; }
    virtual int compareNil(const NilContent&) const override { return -1; }
    virtual int compareList(const ListContent&) const override { return -1; }
    virtual int compareMap(const MapContent&) const override { return -1; }
    virtual int compareOrderedMap(const OrderedMapContent&) const override;

    virtual Content* add(const Content&) const override;
    virtual Content* sub(const Content&) const override;
    virtual Content* mul(const Content&) const override;
    virtual Content* div(const Content&) const override;
    virtual Content* mod(const Content&) const override;

    virtual Value keys() const override;
    virtual Value& element(const Value&) override;
    virtual bool contains(const Value& key) const override;
    virtual Value remove(const Value&) override;


    virtual void print(std::ostream&) const override;
    virtual void json(JSON&) const override;

    virtual std::string typeName() const override { return "OrderedMap"; }

    virtual bool isMap() const override { return true; }
    virtual bool isOrderedMap() const override { return true; }
    virtual Content* clone() const override;
    virtual void dump(std::ostream& out, size_t depth, bool indent = true) const override;


private:
    // -- Methods

    const Value& value(const Value& key) const;

    // -- Members

    ValueMap value_;
    ValueList keys_;

    friend class Value;

};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
