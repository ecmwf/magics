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

    virtual ~OrderedMapContent();

// -- Overridden methods

    // -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const { Content::value(n); }
    virtual void value(long long& n)   const { Content::value(n); }
    virtual void value(double& n)      const { Content::value(n); }
    virtual void value(std::string& n) const { Content::value(n); }
    virtual void value(ValueList& n)   const { Content::value(n); }
    virtual void value(ValueMap& n)    const;

    virtual int  compareBool(const BoolContent&)            const {return -1; }
    virtual int  compareNumber(const NumberContent&)        const {return -1; }
    virtual int  compareDouble(const DoubleContent&)        const {return -1; }
    virtual int  compareString(const StringContent&)        const {return -1; }
    virtual int  compareNil(const NilContent&)              const {return -1; }
    virtual int  compareList(const ListContent&)            const {return -1; }
    virtual int  compareMap(const MapContent&)              const {return -1; }
    virtual int  compareOrderedMap(const OrderedMapContent&) const;

    virtual Content* add(const Content&) const;
    virtual Content* sub(const Content&) const;
    virtual Content* mul(const Content&) const;
    virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    virtual Value   keys() const;
    virtual Value&  element(const Value&);
    virtual bool    contains(const Value& key) const;
    virtual Value   remove(const Value&);


    virtual void   print(std::ostream&) const;
    virtual void   json(JSON&)     const;

    virtual std::string typeName() const       { return "OrderedMap"; }

    virtual bool   isMap() const         { return true; }
    virtual bool   isOrderedMap() const         { return true; }
    virtual Content* clone() const;
    virtual void    dump(std::ostream& out, size_t depth, bool indent = true) const;


private:

// -- Methods

    const Value& value(const Value& key) const;

// -- Members

    ValueMap  value_;
    ValueList keys_;

    friend class Value;

};


//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
