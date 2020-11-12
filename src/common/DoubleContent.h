/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Baudouin Raoult
/// @author Manuel Fuentes

#ifndef magics_DoubleContent_h
#define magics_DoubleContent_h

#include "Content.h"
#include "Value.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class DoubleContent : public Content {
protected:
    // -- Constructor

    DoubleContent(double);

    // -- Destructor

    virtual ~DoubleContent() override;

    // -- Overridden methods

    // -- From Content
    virtual int compare(const Content& other) const override;

    virtual void value(bool& n) const override { Content::value(n); }
    virtual void value(long long& n) const override { Content::value(n); }
    virtual void value(double& n) const override;
    virtual void value(std::string& n) const override;
    virtual void value(ValueList& n) const override { Content::value(n); }
    virtual void value(ValueMap& n) const override { Content::value(n); }

    virtual int compareBool(const BoolContent&) const override { return -1; }
    virtual int compareNumber(const NumberContent&) const override;
    virtual int compareDouble(const DoubleContent&) const override;
    virtual int compareString(const StringContent&) const override { return 1; }
    virtual int compareNil(const NilContent&) const override { return 1; }
    virtual int compareList(const ListContent&) const override { return 1; }
    virtual int compareMap(const MapContent&) const override { return 1; }
    virtual int compareOrderedMap(const OrderedMapContent&) const override { return 1; }


    virtual Content* add(const Content&) const override;
    virtual Content* sub(const Content&) const override;
    virtual Content* mul(const Content&) const override;
    virtual Content* div(const Content&) const override;
    virtual Content* mod(const Content&) const override;

    virtual Content* addDouble(const DoubleContent&) const override;
    virtual Content* subDouble(const DoubleContent&) const override;
    virtual Content* mulDouble(const DoubleContent&) const override;
    virtual Content* divDouble(const DoubleContent&) const override;


    virtual Value negate() const override;

    //        virtual Content* addNumber(const NumberContent&) const override;
    //        virtual Content* subNumber(const NumberContent&) const override;
    //        virtual Content* mulNumber(const NumberContent&) const override;
    //        virtual Content* divNumber(const NumberContent&) const override;

    virtual void print(std::ostream&) const override;
    virtual void json(JSON&) const override;
    virtual std::string typeName() const override { return "Double"; }
    virtual bool isDouble() const override { return true; }
    virtual Content* clone() const override;
    virtual void dump(std::ostream& out, size_t depth, bool indent = true) const override;

private:
    DoubleContent(const DoubleContent&);
    DoubleContent& operator=(const DoubleContent&);

    // -- Members

    double value_;

    friend class Value;
    friend class NumberContent;
};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
