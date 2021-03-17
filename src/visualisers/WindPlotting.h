/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file WindPlotting.h
    \brief Definition of the Template class WindPlotting.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#ifndef WindPlotting_H
#define WindPlotting_H

#include "Factory.h"
#include "IntervalMap.h"
#include "MagTranslator.h"
#include "Transformation.h"
#include "WindPlottingAttributes.h"
#include "magics.h"

namespace magics {


class WindPlotting : public WindPlottingAttributes, public LevelSelectionInterface, public ColourTechniqueInterface {
public:
    WindPlotting();
    virtual ~WindPlotting() override {}
    virtual void set(const map<string, string>& map) override { WindPlottingAttributes::set(map); };
    virtual void set(const XmlNode& node) override { WindPlottingAttributes::set(node); }
    virtual bool accept(const string& key) override { return WindPlottingAttributes::accept(key); }
    virtual void toxml(ostream&) {}
    virtual WindPlotting* clone() { return 0; };
    virtual void operator()(bool, const PaperPoint&, double, double, double = 0) {}
    virtual bool operator()(Data& data, BasicGraphicsObjectContainer& parent) { return false; }
    virtual void prepare(BasicGraphicsObjectContainer&, double) {}
    virtual void prepare(BasicGraphicsObjectContainer&) {}
    virtual void finish(BasicGraphicsObjectContainer&) {}
    virtual void visit(LegendVisitor&);
    virtual void visit(Data&, PointsHandler&, HistoVisitor&) {}
    virtual void adjust(CustomisedPointsList&, const Transformation&);
    bool needLegend() { return legend_; }
    void legendOnly(bool legend) {
        legendOnly_  = legend;
        legend_only_ = legend;
    }
    Colour& colour(Colour& Colour, double, double, double);

    Colour& off(Colour& colour, double, double, double) { return colour; }
    Colour& advanced(Colour& colour, double, double, double);
    void offMinMax(double&, double&);
    void advancedMinMax(double&, double&);

    double value(double, double, double);
    double speed(double, double, double);
    double parameter(double, double, double);
    int getCount() const override { return count_; }
    int getTolerance() const override { return tolerance_; }
    double getReference() const override { return reference_; }
    double getInterval() const override { return interval_; }
    double getMin() const override { return min_; }
    double getMax() const override { return max_; }
    const Colour& getMinColour() const override { return *minColour_; }
    const Colour& getMaxColour() const override { return *maxColour_; }
    const string& getDirection() const override { return direction_; }
    ListPolicy getPolicy() const override { return ListPolicy::LASTONE; }
    stringarray getColours() const override { return colours_; }
    floatarray getList() const override { return list_; }

protected:
    bool legendOnly_;
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "WindPlotting"; }
    typedef Colour& (WindPlotting::*AdvancedMethod)(Colour&, double, double, double);
    typedef double (WindPlotting::*ColouringMethod)(double, double, double);
    typedef void (WindPlotting::*MinMaxMethod)(double&, double&);
    typedef void (WindPlotting::*SettingMethod)(double&, double&);
    static map<string, WindPlotting::AdvancedMethod> methods_;
    static map<string, WindPlotting::ColouringMethod> colouringMethods_;
    static map<string, WindPlotting::MinMaxMethod> minMaxMethods_;
    static map<string, WindPlotting::SettingMethod> settingMethods_;

    void setAdvanced(double&, double&);
    void setNormal(double&, double&) {}

    IntervalMap<Colour> map_;
    virtual double minSpeed() {
        NOTIMP;
        return 0;
    }
    virtual double maxSpeed() {
        NOTIMP;
        return 0;
    }


private:
    //! Copy constructor - No copy allowed
    WindPlotting(const WindPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    WindPlotting& operator=(const WindPlotting&);


    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const WindPlotting& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, WindPlotting> {
public:
    WindPlotting* operator()(const string& val) { return SimpleObjectMaker<WindPlotting>::create(val); }
    WindPlotting* magics(const string& param) {
        WindPlotting* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics

#endif
