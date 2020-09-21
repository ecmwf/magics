/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @author Manuel Fuentes
/// @author Tiago Quintino
/// @date   Jun 97


#ifndef magics_StringContent_h
#define magics_StringContent_h

#include "Content.h"

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


class StringContent : public Content {

protected:

    // -- Constructor

    StringContent(const std::string&);
    StringContent(const char*);

    // -- Destructor

    virtual ~StringContent();

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
    virtual int  compareNumber(const NumberContent&)        const {return -1; }
    virtual int  compareDouble(const DoubleContent&)        const {return -1; }
    virtual int  compareString(const StringContent&)        const;
    virtual int  compareNil(const NilContent&)              const {return 1; }
    virtual int  compareList(const ListContent&)            const {return 1; }
    virtual int  compareMap(const MapContent&)              const {return 1; }
    virtual int  compareOrderedMap(const OrderedMapContent&) const { return 1; }

    virtual Content* add(const Content&)             const;
    virtual Content* sub(const Content&) const;
    virtual Content* mul(const Content&) const;
    virtual Content* div(const Content&) const;
    virtual Content* mod(const Content&) const;

    virtual Content* addString(const StringContent&) const;

    virtual void    print(std::ostream&) const;
    virtual void   json(JSON&)     const;
    virtual std::string  typeName() const      { return "String"; }
    virtual bool    isString() const      { return true; }
    virtual Content* clone() const;
    virtual void    dump(std::ostream& out, size_t depth, bool indent=true) const;


private:

    // -- No copy allowed

    StringContent(const StringContent&);
    StringContent& operator=(const StringContent&);

    // -- Members

    std::string value_;

    friend class Value;
};


//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
