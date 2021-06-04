/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisMinorTick.h
    \brief Definition of the Template class AxisMinorTick.

    Magics Team - ECMWF 2005

    Started: Thu 15-Dec-2005

    Changes:

*/

#ifndef AxisMinorTick_H
#define AxisMinorTick_H

#include "AxisItem.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "AxisMinorTickAttributes.h"


namespace magics {

class HorizontalAxisVisitor;
class VerticalAxisVisitor;
class Transformation;

class AxisMinorTick : public AxisMinorTickAttributes {
public:
    AxisMinorTick();
    virtual ~AxisMinorTick() override;

    virtual void set(const XmlNode& node) override { AxisMinorTickAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { AxisMinorTickAttributes::set(map); }
    virtual AxisMinorTick* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new AxisMinorTick();
    }

    virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&) override;
    virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    AxisMinorTick(const AxisMinorTick&);
    //! Overloaded << operator to copy - No copy allowed
    AxisMinorTick& operator=(const AxisMinorTick&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisMinorTick& p) {
        p.print(s);
        return s;
    }
};

class NoMinorAxisTick : public AxisMinorTick {
public:
    NoMinorAxisTick() {}
    ~NoMinorAxisTick() override{};

protected:
    virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&) {}
    virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&) {}

    AxisMinorTick* clone() const override { return new NoMinorAxisTick(); }
};


template <>
class MagTranslator<string, AxisMinorTick> {
public:
    AxisMinorTick* operator()(const string& val) { return SimpleObjectMaker<AxisMinorTick>::create(val); }

    AxisMinorTick* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
