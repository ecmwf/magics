/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PageIDBase.h
    \brief Definition of the Template class PageIDBase.

    Magics Team - ECMWF 2006

    Started: Thu 9-Feb-2006

    Changes:

*/

#ifndef PageIDBase_H
#define PageIDBase_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class Task;


class PageIDBase {
public:
    PageIDBase() {}
    virtual ~PageIDBase() {}

    virtual void set(const XmlNode&) { MagLog::dev() << "PageIDBase::set(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual PageIDBase* clone() const {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
        return new PageIDBase();
    }
    virtual void toxml(ostream&, int = 0) const {
        MagLog::dev() << "PageIDBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }

    virtual void operator()(const ErrorReporter&, Task&) const {
        MagLog::dev()
            << "PageIDBase::operator()(const ErrorReporter&, vector<BaseGraphicsObject*>&)---> to be checked!...\n";
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "PageIDBase\n"; }

private:
    //! Copy constructor - No copy allowed
    PageIDBase(const PageIDBase&);
    //! Overloaded << operator to copy - No copy allowed
    PageIDBase& operator=(const PageIDBase&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const PageIDBase& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, PageIDBase> {
public:
    PageIDBase* operator()(const string& val) { return SimpleObjectMaker<PageIDBase>::create(val); }

    PageIDBase* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
