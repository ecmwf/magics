/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ArrowPlotting.h
    \brief Definition of the Template class ArrowPlotting.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#ifndef ArrowPlotting_H
#define ArrowPlotting_H

#include "magics.h"

#include "Arrow.h"
#include "ArrowPlottingAttributes.h"
#include "WindPlotting.h"

namespace magics {


class ArrowPlotting : public WindPlotting, public ArrowPlottingAttributes {
public:
    ArrowPlotting() {}
    virtual ~ArrowPlotting() override {}

    virtual void set(const map<string, string>& map) override {
        WindPlottingAttributes::set(map);
        ArrowPlottingAttributes::set(map);
    }

    virtual void set(const XmlNode& node) override {
        WindPlottingAttributes::set(node);
        ArrowPlottingAttributes::set(node);
    }

    void copy(const ArrowPlotting& other) {
        WindPlottingAttributes::copy(other);
        ArrowPlottingAttributes::copy(other);
    }
    bool accept(const string& node) override { return ArrowPlottingAttributes::accept(node); }

    virtual WindPlotting* clone() override {
        ArrowPlotting* object = new ArrowPlotting();
        object->copy(*this);
        return object;
    }

    Arrow* southArrow(const Colour&);
    Arrow* northArrow(const Colour&);
    double minSpeed() override { return min_speed_; }
    double maxSpeed() override { return max_speed_; }


    virtual void operator()(bool, const PaperPoint& point, double x, double y, double val) override;

    virtual void prepare(BasicGraphicsObjectContainer& out, double res) override;
    virtual void prepare(BasicGraphicsObjectContainer&) override;
    virtual void finish(BasicGraphicsObjectContainer&) override;
    void visit(LegendVisitor& legend) override;
    void visit(Data&, PointsHandler&, HistoVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "ArrowPlotting<P>"; }
    map<Colour, Arrow*> southArrows_;
    map<Colour, Arrow*> northArrows_;
    double maxVelocity_;

private:
    //! Copy constructor - No copy allowed
    ArrowPlotting(const ArrowPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    ArrowPlotting& operator=(const ArrowPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ArrowPlotting& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
