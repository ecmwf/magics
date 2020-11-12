/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SimplePolylineVisualiser.h
    \brief Definition of the Template class SimplePolylineVisualiser.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef SimplePolylineVisualiser_H
#define SimplePolylineVisualiser_H
#include "magics.h"


#include "SimplePolylineAttributes.h"


#include "Data.h"
#include "UserPoint.h"
#include "Visdef.h"
#include "magics.h"

namespace magics {

class XmlNode;


class SimplePolylineVisualiser : public SimplePolylineAttributes, public Visdef {
public:
    SimplePolylineVisualiser();
    virtual ~SimplePolylineVisualiser() override;


    void operator()(Data&, BasicGraphicsObjectContainer&) override;
    void visit(Data&, LegendVisitor&) override;
    bool needLegend() override { return legend_; }
    int getCount() const override { return count_; }
    int getTolerance() const override { return tolerance_; }
    double getReference() const override { return reference_; }
    double getInterval() const override { return interval_; }
    doublearray getList() const override { return list_; };
    double getMin() const override { return min_; }
    double getMax() const override { return max_; }

    const Colour& getMinColour() const override { return *minColour_; }
    const Colour& getMaxColour() const override { return *maxColour_; }
    const string& getDirection() const override { return direction_; }
    stringarray getColours() const override { return colours_; }

    ListPolicy getPolicy() const override { return ListPolicy::LASTONE; }
    // Implements the set method ...
    void set(const map<string, string>& map) override { SimplePolylineAttributes::set(map); }
    void set(const XmlNode& node) override { SimplePolylineAttributes::set(node); }

    void basic(Data&, BasicGraphicsObjectContainer&);
    void smooth(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    Colour colour(const CustomisedPoint&);
    LineStyle style(const CustomisedPoint&);
    double thickness(const CustomisedPoint&);
    double alpha(const CustomisedPoint&);
    double priority(const CustomisedPoint&);

    void setup();

    typedef void (SimplePolylineVisualiser::*Method)(Data&, BasicGraphicsObjectContainer&);
    map<string, Method> map_;
    IntervalMap<Colour> colour_map_;
    IntervalMap<LineStyle> style_map_;
    IntervalMap<double> thickness_map_;
    IntervalMap<int> alpha_map_;

private:
    //! Copy constructor - No copy allowed
    SimplePolylineVisualiser(const SimplePolylineVisualiser&);
    //! Overloaded << operator to copy - No copy allowed
    SimplePolylineVisualiser& operator=(const SimplePolylineVisualiser&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SimplePolylineVisualiser& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
