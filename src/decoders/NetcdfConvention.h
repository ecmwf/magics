/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfConvention.h
    \brief Definition of the Template class NetcdfConvention.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#ifndef NetcdfConvention_H
#define NetcdfConvention_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class XmlNode;

class NetcdfConvention {
public:
    NetcdfConvention();
    virtual ~NetcdfConvention();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    virtual NetcdfConvention* clone() { return new NetcdfConvention(); }
    void toxml(ostream&, int) const {}


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    NetcdfConvention(const NetcdfConvention&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfConvention& operator=(const NetcdfConvention&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfConvention& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, NetcdfConvention> {
public:
    NetcdfConvention* operator()(const string& val) { return SimpleObjectMaker<NetcdfConvention>::create(val); }
    NetcdfConvention* magics(const string& param) {
        NetcdfConvention* object;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics
#endif
