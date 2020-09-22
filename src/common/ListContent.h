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

#ifndef magics_ListContent_h
#define magics_ListContent_h

#include "Value.h"


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class ListContent : public Content {

protected:

// -- Constructor

	ListContent();
	ListContent(const ValueList&);
	ListContent(const Value&);


// -- Destructor

	virtual ~ListContent();

// -- Overridden methods

    // -- From Content

    virtual int compare(const Content& other) const;

    virtual void value(bool& n)        const;
    virtual void value(long long& n)   const;
    virtual void value(double& n)      const;
    virtual void value(std::string& n) const;
    virtual void value(ValueMap& n)    const { Content::value(n); }
    virtual void value(ValueList& n)   const;

    virtual int  compareBool(const BoolContent&)            const {return -1; }
    virtual int  compareNumber(const NumberContent&)        const {return -1; }
    virtual int  compareDouble(const DoubleContent&)        const {return -1; }
    virtual int  compareString(const StringContent&)        const {return -1; }
    virtual int  compareNil(const NilContent&)              const {return -1; }
    virtual int  compareList(const ListContent&)            const;
    virtual int  compareMap(const MapContent&)              const {return 1; }
    virtual int  compareOrderedMap(const OrderedMapContent&) const { return 1; }

    virtual Content* add(const Content&) const;
	virtual Content* sub(const Content&) const;
	virtual Content* mul(const Content&) const;
	virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    virtual Content* addList(const ListContent&) const;

	virtual void   print(std::ostream&) const;
    virtual void   json(JSON&)     const;
	virtual std::string typeName() const       { return "List"; }

	virtual bool   isList() const         { return true; }
    virtual Value& element(const Value&);
    virtual bool contains(const Value& key) const;
    virtual Content* clone() const;
    virtual size_t size() const;
    virtual void    dump(std::ostream& out, size_t depth, bool indent=true) const;

private:

	ListContent(const ListContent&);
	ListContent& operator=(const ListContent&);

// -- Members

    ValueList value_;

// -- Class Members

	friend class Value;

};


//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
