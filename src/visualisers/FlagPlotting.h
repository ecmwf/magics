/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FlagPlotting.h
    \brief Definition of the Template class FlagPlotting.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#ifndef FlagPlotting_H
#define FlagPlotting_H

#include "magics.h"

#include "Flag.h"
#include "FlagPlottingAttributes.h"
#include "WindPlotting.h"

namespace magics {

class FlagPlotting : public WindPlotting, public FlagPlottingAttributes {
public:
    FlagPlotting() {}
    virtual ~FlagPlotting() override {}
    virtual void set(const map<string, string>& map) override {
        WindPlottingAttributes::set(map);
        FlagPlottingAttributes::set(map);
    }
    virtual void set(const XmlNode& node) override {
        WindPlottingAttributes::set(node);
        FlagPlottingAttributes::set(node);
    }
    bool accept(const string& node) override {
        return FlagPlottingAttributes::accept(node);
        ;
    }
    void copy(const FlagPlotting& other) {
        WindPlottingAttributes::copy(other);
        FlagPlottingAttributes::copy(other);
    }
    virtual WindPlotting* clone() override {
        FlagPlotting* object = new FlagPlotting();
        object->copy(*this);
        return object;
    }

    virtual void operator()(bool, const PaperPoint& point, double x, double y, double) override;
    Flag* southFlag(const Colour& colour);
    Flag* northFlag(const Colour& colour);
    virtual void prepare(BasicGraphicsObjectContainer& task, double) override;
    void visit(LegendVisitor& legend) override;
    void visit(Data&, PointsHandler&, HistoVisitor&) override;
    double minSpeed() override { return min_speed_; }
    double maxSpeed() override { return max_speed_; }
    void finish(BasicGraphicsObjectContainer&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "FlagPlotting<P>"; }
    map<Colour, Flag*> northFlags_;
    map<Colour, Flag*> southFlags_;

private:
    //! Copy constructor - No copy allowed
    FlagPlotting(const FlagPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    FlagPlotting& operator=(const FlagPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FlagPlotting& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
