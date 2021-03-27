/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourSelection.h
    \brief Definition of the Template class ColourSelection.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef ColourSelection_H
#define ColourSelection_H

#include "magics.h"

#include "Factory.h"
#include "FloatSelection.h"
#include "MagTranslator.h"

namespace magics {

class XmlNode;

class ColourSelection : public FloatSelection {
public:
    ColourSelection();
    virtual ~ColourSelection();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual void toxml(ostream&, int) const {}
    virtual ColourSelection* clone() const {
        ColourSelection* object = new ColourSelection();
        return object;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    ColourSelection(const ColourSelection&);
    //! Overloaded << operator to copy - No copy allowed
    ColourSelection& operator=(const ColourSelection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourSelection& p) {
        p.print(s);
        return s;
    }
};
template <>
class MagTranslator<string, ColourSelection> {
public:
    ColourSelection* operator()(const string& val) { return SimpleObjectMaker<ColourSelection>::create(val); }

    ColourSelection* magics(const string& param) {
        ColourSelection* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
