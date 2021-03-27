/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisControl.h
    \brief Definition of the Template class AxisControl.

    Magics Team - ECMWF 2005

    Started: Thu 13-Oct-2005

    Changes:

*/

#ifndef AxisControl_H
#define AxisControl_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {


class Layout;
class Transformation;
class AxisMethod;

class AxisControl {
public:
    AxisControl();
    virtual ~AxisControl();

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }
    virtual AxisControl* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new AxisControl();
    }
    virtual void toxml(ostream&) const {}

    virtual void vertical(Layout&, Transformation&, AxisMethod&);
    virtual void horizontal(Layout&, Transformation&, AxisMethod&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    AxisControl(const AxisControl&);
    //! Overloaded << operator to copy - No copy allowed
    AxisControl& operator=(const AxisControl&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisControl& p) {
        p.print(s);
        return s;
    }
};

class AutomaticAxisControl : public AxisControl {
public:
    AutomaticAxisControl() {}
    ~AutomaticAxisControl() {}

protected:
    virtual AxisControl* clone() const { return new AutomaticAxisControl(); }
    virtual void vertical(Layout&, Transformation&, AxisMethod&);
    virtual void horizontal(Layout&, Transformation&, AxisMethod&);
};

template <>
class MagTranslator<string, AxisControl> {
public:
    AxisControl* operator()(const string& val) { return SimpleObjectMaker<AxisControl>::create(val); }

    AxisControl* magics(const string& param) {
        AxisControl* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
