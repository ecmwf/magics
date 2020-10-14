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

#ifndef magics_BoolContent_h
#define magics_BoolContent_h

#include "Content.h"
#include "Value.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class BoolContent : public Content {

protected:

    // -- Constructor

    BoolContent(bool);

    // -- Destructor

    virtual ~BoolContent();

    // -- Overridden methods

    // -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const;
    virtual void value(long long& n)   const;
    virtual void value(double& n)      const;
    virtual void value(std::string& n) const;
    virtual void value(ValueList& n)   const { Content::value(n); }
    virtual void value(ValueMap& n)    const { Content::value(n); }

    virtual int  compareBool(const BoolContent&)            const;
    virtual int  compareNumber(const NumberContent&)        const {return 1; }
    virtual int  compareDouble(const DoubleContent&)        const {return 1; }
    virtual int  compareString(const StringContent&)        const {return 1; }
    virtual int  compareNil(const NilContent&)              const {return 1; }
    virtual int  compareList(const ListContent&)            const {return 1; }
    virtual int  compareMap(const MapContent&)              const {return 1; }
    virtual int  compareOrderedMap(const OrderedMapContent&) const { return 1; }

    virtual Content* add(const Content&)  const;
    virtual Content* sub(const Content&) const;
    virtual Content* mul(const Content&) const;
    virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    //        virtual Content* addBool(const BoolContent&) const;
    //        virtual Content* subBool(const BoolContent&) const;
    //        virtual Content* mulBool(const BoolContent&) const;
    //        virtual Content* divBool(const BoolContent&) const;

    //        virtual Content* addNumber(const NumberContent&) const;
    //        virtual Content* subNumber(const NumberContent&) const;
    //        virtual Content* mulNumber(const NumberContent&) const;
    //        virtual Content* divNumber(const NumberContent&) const;

    virtual void    print(std::ostream&) const;
    virtual void    dump(std::ostream& out, size_t depth, bool indent=true) const;

    virtual void    json(JSON&)     const;
    virtual std::string  typeName()      const { return "Bool"; }
    virtual bool    isBool()      const { return true; }
    virtual Content* clone() const;



private:

    BoolContent(const BoolContent&);
    BoolContent& operator=(const BoolContent&);

    // -- Members

    bool value_;

    // -- Class Members

    // -- Friends

    friend class Value;
};

//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
