/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisTick.h
    \brief Definition of the Template class AxisTick.

    Magics Team - ECMWF 2005

    Started: Fri 7-Oct-2005

    Changes:

*/

#ifndef AxisTick_H
#define AxisTick_H

#include "AxisItem.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


#include "AxisTickAttributes.h"

namespace magics {


class Transformation;
class HorizontalAxisVisitor;
class VerticalAxisVisitor;

class AxisTick : public AxisTickAttributes {
public:
    AxisTick();
    virtual ~AxisTick();

    virtual void set(const XmlNode& node) { AxisTickAttributes::set(node); }
    virtual void set(const map<string, string>& map) { AxisTickAttributes::set(map); }
    virtual AxisTick* clone() const {
        AxisTick* tick = new AxisTick();
        tick->copy(*this);
        return tick;
    }

    virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&);
    virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    AxisTick(const AxisTick&);
    //! Overloaded << operator to copy - No copy allowed
    AxisTick& operator=(const AxisTick&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisTick& p) {
        p.print(s);
        return s;
    }
};

class NoAxisTick : public AxisTick {
public:
    NoAxisTick() {}
    ~NoAxisTick(){};

protected:
    virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&) {}
    virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&) {}
    AxisTick* clone() const { return new NoAxisTick(); }
};


template <>
class MagTranslator<string, AxisTick> {
public:
    AxisTick* operator()(const string& val) { return SimpleObjectMaker<AxisTick>::create(val); }

    AxisTick* magics(const string& param) {
        AxisTick* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
