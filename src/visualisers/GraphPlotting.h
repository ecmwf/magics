/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphPlotting.h
    \brief Definition of the Template class GraphPlotting.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef GraphPlotting_H
#define GraphPlotting_H

#include "magics.h"

#include "GraphPlottingAttributes.h"
#include "UserPoint.h"
#include "Visdef.h"


namespace magics {


class DateTime;

class GraphPlotting : public GraphPlottingAttributes, public Visdef {
public:
    GraphPlotting();
    virtual ~GraphPlotting() override;

    // Implements the Visualiser interface  ...

    void operator()(Data& data, BasicGraphicsObjectContainer& parent) override { return (*type_)(data, parent); }
    bool needLegend() override { return legend_; }
    void visit(LegendVisitor& legend) override {
        type_->legend(legend_, legend_text_);
        type_->visit(legend);
    }
    void visit(TopAxisVisitor& top) override { type_->visit(top); }
    virtual void visit(Transformation& transformation, Data& data) override { type_->visit(transformation, data); }
    // Implements the set method ...
    void set(const map<string, string>& map) override {
        GraphPlottingAttributes::set(map);
        type_->set(map);
    }
    void set(const XmlNode& node) override {
        GraphPlottingAttributes::set(node);
        type_->set(node);
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    GraphPlotting(const GraphPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    GraphPlotting& operator=(const GraphPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GraphPlotting& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
