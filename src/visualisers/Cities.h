/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Cities.h
    \brief Definition of the Template class NoCities.

    Magics Team - ECMWF 2006

    Started: Tue 29-Aug-2006

    Changes:

*/

#ifndef Cities_H
#define Cities_H

#include "BasicGraphicsObject.h"
#include "CitiesAttributes.h"
#include "SceneVisitor.h"

namespace magics {

class NoCities {
public:
    NoCities();
    virtual ~NoCities();

    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    virtual void set(const map<string, string>&) {}
    virtual NoCities* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoCities();
    }

    virtual void toxml(ostream&, int = 0) const {
        MagLog::dev() << "NoCities::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }

    virtual void operator()(const map<string, string>&, BasicGraphicsObjectContainer&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    NoCities(const NoCities&);
    //! Overloaded << operator to copy - No copy allowed
    NoCities& operator=(const NoCities&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoCities& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoCities> {
public:
    NoCities* operator()(const string& val) { return SimpleObjectMaker<NoCities>::create(val); }

    NoCities* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};


class Cities : public NoCities, public CitiesAttributes {
public:
    Cities();
    virtual ~Cities();

    virtual void set(const XmlNode& node) { CitiesAttributes::set(node); }

    virtual void set(const map<string, string>& map) { CitiesAttributes::set(map); }

    bool accept(const string& node) { return CitiesAttributes::accept(node); }

    virtual NoCities* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new Cities();
    }

    void operator()(const map<string, string>&, BasicGraphicsObjectContainer&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    Cities(const Cities&);
    //! Overloaded << operator to copy - No copy allowed
    Cities& operator=(const Cities&);
};

}  // namespace magics
#endif
