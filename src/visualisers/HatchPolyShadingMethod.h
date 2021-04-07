/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HatchPolyShadingMethod.h
    \brief Definition of the Template class HatchPolyShadingMethod.

    Magics Team - ECMWF 2004

    Started: Wed 25-Aug-2004

    Changes:

*/

#ifndef HatchPolyShadingMethod_H
#define HatchPolyShadingMethod_H

#include "magics.h"

#include "HatchPolyShadingMethodAttributes.h"
#include "IntervalMap.h"
#include "PolyShadingMethod.h"

namespace magics {


class HatchPolyShadingMethod : public PolyShadingMethod, public HatchPolyShadingMethodAttributes {
public:
    HatchPolyShadingMethod() {}
    virtual ~HatchPolyShadingMethod() override {}


    virtual void set(const map<string, string>& map) override { HatchPolyShadingMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) override { HatchPolyShadingMethodAttributes::set(node); }
    virtual bool accept(const string& node) override { return HatchPolyShadingMethodAttributes::accept(node); }
    virtual PolyShadingMethod* clone() const override {
        HatchPolyShadingMethod* object = new HatchPolyShadingMethod();
        object->copy(*this);
        return object;
    }

    virtual void prepare(LevelSelection& levels, const ColourTechnique&) override;
    virtual void operator()(Polyline& poly) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "HatchPolyShadingMethod[]"; }
    vector<int> hatches_;

private:
    //! Copy constructor - No copy allowed
    HatchPolyShadingMethod(const HatchPolyShadingMethod&);
    //! Overloaded << operator to copy - No copy allowed
    HatchPolyShadingMethod& operator=(const HatchPolyShadingMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HatchPolyShadingMethod& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics

#endif
