/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CellShading.h
    \brief Definition of the Template class CellShading.

    Magics Team - ECMWF 2005

    Started: Tue 30-Aug-2005

    Changes:

*/

#ifndef CellShading_H
#define CellShading_H

#include "magics.h"

#include "CellShadingAttributes.h"
#include "IsoShading.h"

#include "ColourTable.h"

namespace magics {


class CellShading : public ShadingTechnique, public CellShadingAttributes {
public:
    CellShading();
    virtual ~CellShading();
    void set(const map<string, string>& map) { CellShadingAttributes::set(map); }
    void set(const XmlNode& node) { CellShadingAttributes::set(node); }
    bool accept(const string& node) { return CellShadingAttributes::accept(node); }

    virtual ShadingTechnique* clone() const {
        CellShading* object = new CellShading();
        return object;
    }

    void copy(const CellShading& from) {
        CellShadingAttributes::copy(from);
        // IsoShadingAttributes::copy(from);
    }
    bool shadingMode() { return shading_ == "grid"; }
    virtual void visit(LegendVisitor&, const ColourTechnique&);

    int index(double value);
    void operator()(Polyline*) const;

    bool hasLegend() { return true; }  // Isolien legend is not needed!
    virtual void operator()(IsoPlot*, MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual bool prepare(LevelSelection&, const ColourTechnique&);
    virtual void colour(double, Colour&);
    CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation, int width,
                     int height, float resolution, const string& technique);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    IntervalMap<int> map_;
    vector<Colour> colours_;
    string shading_;
    bool adaptive_;

private:
    //! Copy constructor - No copy allowed
    CellShading(const CellShading&);
    //! Overloaded << operator to copy - No copy allowed
    CellShading& operator=(const CellShading&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CellShading& p) {
        p.print(s);
        return s;
    }
};
class DumpShading : public CellShading {
public:
    DumpShading();
    virtual ~DumpShading();

    virtual ShadingTechnique* clone() const {
        DumpShading* object = new DumpShading();
        return object;
    }
    void copy(const CellShading& from) {
        CellShadingAttributes::copy(from);
        // IsoShadingAttributes::copy(from);
    }


    void operator()(IsoPlot*, MatrixHandler&, BasicGraphicsObjectContainer&);
};


}  // namespace magics


#endif
