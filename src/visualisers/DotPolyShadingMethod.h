/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DotPolyShadingMethod.h
    \brief Definition of the Template class DotPolyShadingMethod.

    Magics Team - ECMWF 2004

    Started: Wed 25-Aug-2004

    Changes:

*/

#ifndef DotPolyShadingMethod_H
#define DotPolyShadingMethod_H

#include "MagException.h"
#include "magics.h"

#include "DotPolyShadingMethodAttributes.h"
#include "IntervalMap.h"
#include "PolyShadingMethod.h"

namespace magics {


class DotPolyShadingMethod : public map<double, pair<double, double> >,
                             public PolyShadingMethod,
                             public DotPolyShadingMethodAttributes {
public:
    DotPolyShadingMethod() {}
    virtual ~DotPolyShadingMethod() {}

    virtual void set(const map<string, string>& map) { DotPolyShadingMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) { DotPolyShadingMethodAttributes::set(node); }
    virtual bool accept(const string& node) { return DotPolyShadingMethodAttributes::accept(node); }


    virtual PolyShadingMethod* clone() const {
        DotPolyShadingMethod* object = new DotPolyShadingMethod();
        object->copy(*this);
        return object;
    }

    virtual void prepare(LevelSelection& levels, const ColourTechnique& colours);

    virtual void operator()(Polyline& poly) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

    vector<float> dots_;

private:
    //! Copy constructor - No copy allowed
    DotPolyShadingMethod(const DotPolyShadingMethod&);
    //! Overloaded << operator to copy - No copy allowed
    DotPolyShadingMethod& operator=(const DotPolyShadingMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const DotPolyShadingMethod& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
// Source in PolySghadingMethod.cc

#endif
