/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Curve.h
    \brief Definition of the Template class Curve.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef Curve_H
#define Curve_H

#include "magics.h"

#include "CurveAreaAttributes.h"
#include "CurveAttributes.h"
#include "Graph.h"
#include "Polyline.h"

namespace magics {

class XmlNode;
class LegendEntry;


class Curve : public CurveAttributes, public Graph {
public:
    Curve();
    virtual ~Curve();
    enum PointPosition
    {
        in,
        out,
        enter,
        exit
    };


    void set(const XmlNode& node) {
        Graph::set(node);
        CurveAttributes::set(node);
    }

    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    void set(const PaperPoint&, BasicGraphicsObjectContainer&, LegendEntry&);

    Polyline* newCurve(BasicGraphicsObjectContainer&) const;
    PointPosition where(const UserPoint& point) const;

    // Implements the set method ...
    void set(const map<string, string>& map) { CurveAttributes::set(map); }
    void symbol(vector<PaperPoint>& points, BasicGraphicsObjectContainer& out);
    void legend_symbol(PaperPoint& point, BasicGraphicsObjectContainer& task);

    typedef bool (Curve::*MissingMethod)(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&,
                                         BasicGraphicsObjectContainer&);
    typedef void (Curve::*CurveMethod)(const UserPoint&, vector<UserPoint>&);
    void straight(const UserPoint& point, vector<UserPoint>& out) { out.push_back(point); }
    void stepped(const UserPoint& point, vector<UserPoint>& out);

    virtual void legend(Polyline&) {}
    bool ignore(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);
    bool join(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);
    bool drop(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    bool missing(CustomisedPoint&) const;
    std::map<string, MissingMethod> missingMethods_;
    std::map<string, CurveMethod> curveMethods_;

    std::map<string, int> thicknessHandler_;
    std::map<string, LineStyle> styleHandler_;
    std::map<string, string> colourHandler_;

    Colour currentColour_;
    LineStyle currentStyle_;
    int currentThickness_;

private:
    //! Copy constructor - No copy allowed
    Curve(const Curve&);
    //! Overloaded << operator to copy - No copy allowed
    Curve& operator=(const Curve&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Curve& p) {
        p.print(s);
        return s;
    }
};

class CurveArea : public Curve, public CurveAreaAttributes {
public:
    CurveArea() {}
    virtual ~CurveArea() {}
    void set(const map<string, string>& map) {
        CurveAttributes::set(map);
        CurveAreaAttributes::set(map);
    }

    void operator()(Data&, BasicGraphicsObjectContainer&);
    void legend(Polyline&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const {
        out << "CurveArea";
        Curve::print(out);
    }


private:
    //! Copy constructor - No copy allowed
    CurveArea(const CurveArea&);
    //! Overloaded << operator to copy - No copy allowed
    CurveArea& operator=(const CurveArea&);
};


}  // namespace magics
#endif
