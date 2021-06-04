/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ValuePlot.h
    \brief Definition of the Template class ValuePlot.

    Magics Team - ECMWF 2004

    Started: Wed 3-Mar-2004

    Changes:

*/

#ifndef ValuePlot_H
#define ValuePlot_H

#include "BasicSceneObject.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "ValuePlotAttributes.h"
#include "ValuePlotBase.h"
#include "magics.h"

namespace magics {


class ValuePlot : public ValuePlotBase, public ValuePlotAttributes {
public:
    ValuePlot();
    virtual ~ValuePlot() override;

    virtual ValuePlotBase* clone() const override {
        ValuePlot* plot = new ValuePlot();
        plot->copy(*this);
        return plot;
    }
    virtual bool accept(const string& node) override {
        return ValuePlotAttributes::accept(node);
        ;
    }

    // Implements the VisualComponent Interface...
    void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) override;
    void operator()(Data&, BasicGraphicsObjectContainer&) override;


    virtual void visit(LegendVisitor&) override;
    virtual void set(const map<string, string>& map) override { ValuePlotAttributes::set(map); }
    virtual void set(const XmlNode& node) override { ValuePlotAttributes::set(node); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    ValuePlot(const ValuePlot&);
    //! Overloaded << operator to copy - No copy allowed
    ValuePlot& operator=(const ValuePlot&);


    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ValuePlot& p) {
        p.print(s);
        return s;
    }
};


class NoValuePlot : public ValuePlotBase {
public:
    NoValuePlot(){};
    ~NoValuePlot() override{};
    ValuePlotBase* clone() const override { return new NoValuePlot(); }
    bool accept(const string& node) override { return magCompare(node, "nogridvalues"); }

    void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) override {}
    void operator()(PointsHandler&, BasicGraphicsObjectContainer&) {}
    void visit(LegendVisitor&) override {}
};

}  // namespace magics

#endif
