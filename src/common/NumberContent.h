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

#ifndef magics_NumberContent_h
#define magics_NumberContent_h

#include "Content.h"
#include "Value.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class NumberContent : public Content {

protected:

// -- Constructor

	NumberContent(long long);

// -- Destructor

	virtual ~NumberContent();

// -- Overridden methods

	// -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const;
    virtual void value(long long& n)   const;
    virtual void value(double& n)      const;
    virtual void value(std::string& n) const;
    virtual void value(ValueList& n)   const { Content::value(n); }
    virtual void value(ValueMap& n)    const { Content::value(n); }

    virtual int  compareBool(const BoolContent&)            const {return -1; }
    virtual int  compareNumber(const NumberContent&)        const;
    virtual int  compareDouble(const DoubleContent&)        const;
    virtual int  compareString(const StringContent&)        const {return 1; }
    virtual int  compareNil(const NilContent&)              const {return 1; }
    virtual int  compareList(const ListContent&)            const {return 1; }
    virtual int  compareMap(const MapContent&)              const {return 1; }
    virtual int  compareOrderedMap(const OrderedMapContent&) const { return 1; }

    virtual Content* add(const Content&) const;
	virtual Content* sub(const Content&) const;
	virtual Content* mul(const Content&) const;
	virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    virtual Content* addNumber(const NumberContent&) const;
    virtual Content* subNumber(const NumberContent&) const;
    virtual Content* mulNumber(const NumberContent&) const;
    virtual Content* divNumber(const NumberContent&) const;

    virtual Value negate() const;

//    virtual Content* addDouble(const DoubleContent&) const;
//    virtual Content* subDouble(const DoubleContent&) const;
//    virtual Content* mulDouble(const DoubleContent&) const;
//    virtual Content* divDouble(const DoubleContent&) const;

    virtual void    print(std::ostream&) const;
    virtual void   json(JSON&)     const;
	virtual std::string  typeName()      const { return "Number"; }
	virtual bool    isNumber()      const { return true; }
    virtual Content* clone() const;
    virtual void    dump(std::ostream& out, size_t depth, bool indent=true) const;

private:

	NumberContent(const NumberContent&);
	NumberContent& operator=(const NumberContent&);

// -- Members

    long long value_;

// -- Class Members

	friend class Value;
    friend class DoubleContent;
};


//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
