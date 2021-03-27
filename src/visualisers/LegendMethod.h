/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LegendMethod.h
    \brief Definition of the Template class LegendMethod.

    Magics Team - ECMWF 2006

    Started: Tue 9-May-2006

    Changes:

*/

#ifndef LegendMethod_H
#define LegendMethod_H

#include "ContinuousLegendMethodAttributes.h"
#include "Factory.h"
#include "HistogramLegendMethodAttributes.h"
#include "MagTranslator.h"
#include "PaperPoint.h"
#include "Text.h"
#include "magics.h"


namespace magics {

class LegendEntry;
class LegendTask;


class LegendMethod {
public:
    LegendMethod();
    virtual ~LegendMethod();
    enum LegendType
    {
        DISJOINT,
        CONTINUOUS,
        HISTOGRAM
    };
    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string& node) { return magCompare(node, "disjoint"); }
    virtual LegendMethod* clone() const { return new LegendMethod(); }
    void toxml(ostream&, int = 0) const {}
    virtual void row(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);
    virtual void column(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);
    virtual LegendType name() const { return DISJOINT; }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    LegendMethod(const LegendMethod&);
    //! Overloaded << operator to copy - No copy allowed
    LegendMethod& operator=(const LegendMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LegendMethod& p) {
        p.print(s);
        return s;
    }
};

class ContinuousLegendMethod : public LegendMethod, public ContinuousLegendMethodAttributes {
public:
    ContinuousLegendMethod() : labelCount_(0) {}
    ~ContinuousLegendMethod() {}
    virtual void set(const XmlNode& node) { ContinuousLegendMethodAttributes::set(node); }
    virtual void set(const map<string, string>& map) { ContinuousLegendMethodAttributes::set(map); }
    virtual bool accept(const string& node) { return magCompare(node, "continuous"); }
    virtual LegendType name() const { return CONTINUOUS; }
    LegendMethod* clone() const { return new ContinuousLegendMethod(); }
    virtual void row(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);
    virtual void column(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);

protected:
    int labelCount_;
};

class HistogramLegendMethod : public LegendMethod, public HistogramLegendMethodAttributes {
public:
    HistogramLegendMethod() : labelCount_(0) {}
    ~HistogramLegendMethod() {}
    virtual void set(const XmlNode& node) { HistogramLegendMethodAttributes::set(node); }
    virtual void set(const map<string, string>& map) { HistogramLegendMethodAttributes::set(map); }
    virtual bool accept(const string& node) { return magCompare(node, "histogram"); }
    virtual LegendType name() const { return HISTOGRAM; }
    LegendMethod* clone() const { return new HistogramLegendMethod(); }
    virtual void row(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);
    virtual void column(LegendEntry&, double, double, Text&, BasicGraphicsObjectContainer&);
    const Colour& gridColour() const { return *histo_grid_colour_; }
    int gridThickness() const { return histo_grid_thickness_; }
    LineStyle gridStyle() const { return histo_grid_style_; }
    bool max() const { return histo_max_; }
    bool mean() const { return histo_mean_; }
    const Colour& meanColour() const { return *histo_mean_colour_; }
    int meanMarker() const { return histo_mean_marker_; }
    double meanSize() const { return histo_mean_size_; }

protected:
    int labelCount_;
};
template <>
class MagTranslator<string, LegendMethod> {
public:
    LegendMethod* operator()(const string& val) { return SimpleObjectMaker<LegendMethod>::create(val); }

    LegendMethod* magics(const string& param) {
        LegendMethod* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
