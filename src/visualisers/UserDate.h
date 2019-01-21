/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file UserDate.h
    \brief Definition of the Template class UserDate.

    Magics Team - ECMWF 2005

    Started: Mon 10-Oct-2005

    Changes:

*/

#ifndef UserDate_H
#define UserDate_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class UserDate {
public:
    UserDate();
    UserDate(const string&);
    virtual ~UserDate();

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual UserDate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new UserDate();
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    UserDate(const UserDate&);
    //! Overloaded << operator to copy - No copy allowed
    UserDate& operator=(const UserDate&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const UserDate& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, UserDate> {
public:
    UserDate* operator()(const string& val) { return new UserDate(val); }

    UserDate* magics(const string& param) {
        UserDate* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
