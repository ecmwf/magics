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

#ifndef magics_NilContent_h
#define magics_NilContent_h

#include "Content.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class NilContent : public Content {
protected:
    // -- Constructors

    NilContent();

    // -- Destructors

    virtual ~NilContent() override;

    // -- Overridden Methods

    // From Content

    virtual int compare(const Content& other) const override;

    virtual void value(bool& n) const override { Content::value(n); }
    virtual void value(long long& n) const override { Content::value(n); }
    virtual void value(double& n) const override { Content::value(n); }
    virtual void value(std::string& n) const override { Content::value(n); }
    virtual void value(ValueList& n) const override;
    virtual void value(ValueMap& n) const override { Content::value(n); }

    virtual int compareBool(const BoolContent&) const override { return -1; }
    virtual int compareNumber(const NumberContent&) const override { return -1; }
    virtual int compareDouble(const DoubleContent&) const override { return -1; }
    virtual int compareString(const StringContent&) const override { return -1; }
    virtual int compareNil(const NilContent&) const override;
    virtual int compareList(const ListContent&) const override { return 1; }
    virtual int compareMap(const MapContent&) const override { return 1; }
    virtual int compareOrderedMap(const OrderedMapContent&) const override { return 1; }

    virtual Content* add(const Content&) const override;
    virtual Content* sub(const Content&) const override;
    virtual Content* mul(const Content&) const override;
    virtual Content* div(const Content&) const override;
    virtual Content* mod(const Content&) const override;

    virtual Content* addNil(const NilContent&) const override;
    virtual Content* subNil(const NilContent&) const override;
    virtual Content* mulNil(const NilContent&) const override;
    virtual Content* divNil(const NilContent&) const override;

    virtual bool isNil() const override { return true; }
    virtual std::string typeName() const override { return "Nil"; }
    virtual void print(std::ostream&) const override;
    virtual void json(JSON&) const override;
    virtual Content* clone() const override;
    virtual void dump(std::ostream& out, size_t depth, bool indent = true) const override;

    virtual bool contains(const Value&) const override;


private:
    // -- No copy allowed

    NilContent(const NilContent&);
    NilContent& operator=(const NilContent&);

    friend class Value;
};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
