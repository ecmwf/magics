/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TextOrientation.h
    \brief Definition of the Template class TextOrientation.

    Magics Team - ECMWF 2005

    Started: Fri 7-Oct-2005

    Changes:

*/

#ifndef TextOrientation_H
#define TextOrientation_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class TextOrientation {
public:
    TextOrientation();
    virtual ~TextOrientation();

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual TextOrientation* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new TextOrientation();
    }
    virtual void toxml(ostream&, int) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    TextOrientation(const TextOrientation&);
    //! Overloaded << operator to copy - No copy allowed
    TextOrientation& operator=(const TextOrientation&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TextOrientation& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, TextOrientation> {
public:
    TextOrientation* operator()(const string& val) { return SimpleObjectMaker<TextOrientation>::create(val); }

    TextOrientation* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
