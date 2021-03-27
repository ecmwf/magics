/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ShadingTechnique.h
    \brief Definition of the Template class ShadingTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/

#ifndef ShadingTechnique_H
#define ShadingTechnique_H

#include "ColourTechnique.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class LevelSelection;
class BasicGraphicsObject;
class MatrixHandler;
class Data;
class BasicGraphicsObjectContainer;
class Polyline;
class CellArray;
class Transformation;
class ContourMethod;
class IsoPlot;

class ShadingTechnique {
public:
    ShadingTechnique() : done_(false) {}
    virtual ~ShadingTechnique() {}
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    void toxml(ostream&) const {}
    virtual ShadingTechnique* clone() const { return 0; }
    virtual bool more() { return false; }
    virtual bool shadingMode() { return false; }
    virtual bool hasLegend() { return false; }
    virtual BasicGraphicsObject* next() { return 0; }
    virtual void operator()(Data&, BasicGraphicsObjectContainer&) {}
    virtual void operator()(IsoPlot* iso, MatrixHandler& data, BasicGraphicsObjectContainer& parent);
    virtual void operator()(Polyline*) const {};
    virtual bool prepare(LevelSelection&, const ColourTechnique&) { return false; }
    virtual void visit(LegendVisitor&, const ColourTechnique&) {}
    virtual int index(double) { return -1; }
    virtual int leftIndex(double) { return -1; }
    virtual int rightIndex(double) { return -1; }
    virtual CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation,
                             int width, int height, float resolution, const string& technique) {
        ASSERT(0);
        return 0;
    }
    virtual bool needClipping() { return false; }
    virtual bool method(ContourMethod*) { return false; }
    void reset() { done_ = false; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    bool done_;

private:
    //! Copy constructor - No copy allowed
    ShadingTechnique(const ShadingTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    ShadingTechnique& operator=(const ShadingTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ShadingTechnique& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, ShadingTechnique> {
public:
    ShadingTechnique* operator()(const string& val) { return SimpleObjectMaker<ShadingTechnique>::create(val); }
    ShadingTechnique* magics(const string& param) {
        ShadingTechnique* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics

#endif
