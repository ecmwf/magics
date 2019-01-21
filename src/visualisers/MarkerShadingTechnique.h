/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MarkerShadingTechnique.h
    \brief Definition of the Template class MarkerShadingTechnique.

    Magics Team - ECMWF 2004

    Started: Thu 26-Aug-2004

    Changes:

*/

#ifndef MarkerShadingTechnique_H
#define MarkerShadingTechnique_H

#include "magics.h"

#include "MarkerShadingTechniqueAttributes.h"
#include "ShadingTechnique.h"
#include "Symbol.h"
#include "SymbolMode.h"

namespace magics {


class MarkerShadingTechnique
    : private vector<BasicGraphicsObject*>,  // Internal buffer to store graphical objects during creation phase
      public ShadingTechnique,
      public MarkerShadingTechniqueAttributes {
public:
    MarkerShadingTechnique();
    virtual ~MarkerShadingTechnique();
    virtual void set(const map<string, string>& map) { MarkerShadingTechniqueAttributes::set(map); }
    virtual void set(const XmlNode& node) { MarkerShadingTechniqueAttributes::set(node); }
    virtual ShadingTechnique* clone() const {
        MarkerShadingTechnique* object = new MarkerShadingTechnique();
        object->copy(*this);
        return object;
    }

    bool accept(const string& node) { return MarkerShadingTechniqueAttributes::accept(node); }

    void operator()(IsoPlot*, MatrixHandler&, BasicGraphicsObjectContainer&);

    Symbol* operator()(double);
    virtual void operator()(const PaperPoint&);
    // virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual bool prepare(LevelSelection&, const ColourTechnique&);
    virtual void visit(LegendVisitor&, const ColourTechnique&);
    bool hasLegend() { return true; }  // Isolien legend is not needed!
    CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation, int width,
                     int height, float resolution, const string& technique);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    vector<Symbol*>::iterator current_;
    map<Interval, Symbol*> map_;
    map<Interval, Symbol*> legend_;

private:
    //! Copy constructor - No copy allowed
    MarkerShadingTechnique(const MarkerShadingTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    MarkerShadingTechnique& operator=(const MarkerShadingTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MarkerShadingTechnique& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
