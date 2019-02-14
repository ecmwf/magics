/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinition.h
    \brief Definition of the Template class ColourTableDefinition.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#ifndef ColourTableDefinition_H
#define ColourTableDefinition_H

#include "ColourTable.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

namespace magics {

class XmlNode;

class ColourTableDefinition {
public:
    ColourTableDefinition() {}
    virtual ~ColourTableDefinition() {}
    virtual void set(ColourTable&, int = 0){};
    virtual ColourTableDefinition* clone() const { return new ColourTableDefinition(); }

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }

    virtual void toxml(ostream&) const {}
    virtual void prepare() {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    ColourTableDefinition(const ColourTableDefinition&);
    //! Overloaded << operator to copy - No copy allowed
    ColourTableDefinition& operator=(const ColourTableDefinition&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourTableDefinition& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, ColourTableDefinition> {
public:
    ColourTableDefinition* operator()(const string& val) {
        return SimpleObjectMaker<ColourTableDefinition>::create(val);
    }
    ColourTableDefinition* magics(const string& param) {
        ColourTableDefinition* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics

#endif
