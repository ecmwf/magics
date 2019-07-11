/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolyShadingTechnique.h
    \brief Definition of the Template class PolyShadingTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/

#ifndef PolyShadingTechnique_H
#define PolyShadingTechnique_H

#include "magics.h"

#include "ColourTechnique.h"
#include "ContourMethod.h"
#include "DotPolyShadingMethod.h"
#include "GridShadingAttributes.h"
#include "HatchPolyShadingMethod.h"
#include "PolyShadingTechniqueAttributes.h"
#include "ShadingTechnique.h"

namespace magics {

class LevelSelection;


class PolyShadingTechnique : public ShadingTechnique, public PolyShadingTechniqueAttributes {
public:
    PolyShadingTechnique() {}
    virtual ~PolyShadingTechnique() {}

    void set(const map<string, string>& map) { PolyShadingTechniqueAttributes::set(map); }
    void set(const XmlNode& node) { PolyShadingTechniqueAttributes::set(node); }
    bool accept(const string& node) { return PolyShadingTechniqueAttributes::accept(node); }

    virtual ShadingTechnique* clone() const { return new PolyShadingTechnique(); }

    bool shadingMode() { return true; }
    bool hasLegend() { return true; }  // Isolien legend is not needed!
    void operator()(Polyline* poly) const { (*this->method_)(*poly); }

    void visit(LegendVisitor& legend, const ColourTechnique& colour) { (*this->method_).visit(legend, colour); }

    int index(double value) { return method_->index(value); }
    int rightIndex(double value) { return method_->rightIndex(value); }
    int leftIndex(double value) { return method_->leftIndex(value); }

    virtual bool prepare(LevelSelection& levels, const ColourTechnique& colours) {
        method_->prepare(levels, colours);
        // True if the shading technique needs the isolines to be calculated...
        return true;
    }
    virtual CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation,
                             int width, int height, float resolution, const string& technique);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}


private:
    //! Copy constructor - No copy allowed
    PolyShadingTechnique(const PolyShadingTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    PolyShadingTechnique& operator=(const PolyShadingTechnique&);
};

class GridShading : public PolyShadingTechnique, public GridShadingAttributes {
public:
    GridShading() {}
    virtual ~GridShading() {}

    void set(const map<string, string>& map) { GridShadingAttributes::set(map); }
    void set(const XmlNode& node) { GridShadingAttributes::set(node); }
    bool accept(const string& node) { return GridShadingAttributes::accept(node); }

    ShadingTechnique* clone() const { return new GridShading(); }
    void operator()(Polyline* poly, const ColourTechnique& technique) const {
        poly->setFilled(true);
        poly->setStroke(false);
        FillShadingProperties* shading = new FillShadingProperties();
        poly->setShading(shading);
    }
    void operator()(Polyline* poly) const;
    void visit(LegendVisitor& legend, const ColourTechnique& colour);
    CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation, int width,
                     int height, float resolution, const string& technique);
    virtual bool needClipping() { return true; }
    bool method(ContourMethod* method) {
        method = new ContourMethod();
        return true;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    void print(ostream&) const {}


private:
    //! Copy constructor - No copy allowed
    GridShading(const GridShading&);
    //! Overloaded << operator to copy - No copy allowed
    GridShading& operator=(const GridShading&);
};
struct LegendEntryBuilder {
    LegendEntryBuilder(LegendVisitor& legend, const PolyShadingMethod* method, const ColourTechnique& colours) :
        legend_(legend),
        method_(method),
        colours_(colours),
        first_(true){};
    LegendEntryBuilder(LegendVisitor& legend, const ColourTechnique& colours) :
        legend_(legend),
        method_(0),
        colours_(colours),
        first_(true){};
    ~LegendEntryBuilder(){};
    bool operator()(const pair<double, ColourInfo>& first, const pair<double, ColourInfo>& second) {
        Polyline* box = new Polyline();

        box->index_ = first.second.index_;
        double min  = first.second.level_;
        double max  = second.second.level_;

        if (method_)
            (*method_)(*box);
        else
            box->setShading(new FillShadingProperties());
        box->setFillColour(colours_.right(min));
        box->setFilled(true);
        box->setStroke(true);
        box->setColour(colours_.right(min));

        LegendEntry* entry = new BoxEntry(min, max, box);
        if (first_) {
            first_ = false;
            entry->first();
        }
        for (vector<double>::iterator val = legend_.values_list_.begin(); val != legend_.values_list_.end(); ++val) {
            if (min <= *val && *val < max) {
                string text = tostring(*val);
                entry->userText(text, "user");
                break;
            }
        }

        if (entry->isLast()) {
            if (legend_.values_list_.size() && same(legend_.values_list_.back(), max)) {
                string text = tostring(max);
                entry->userText(text, "user");
                // Try to detect the last entry
            }
        }

        legend_.add(entry);
        return false;
    }
    LegendVisitor& legend_;
    const PolyShadingMethod* method_;
    const ColourTechnique& colours_;
    bool first_;
};
}  // namespace magics


#endif
