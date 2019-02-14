/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Bar.h
    \brief Definition of the Template class Bar.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef Bar_H
#define Bar_H

#include "magics.h"

#include "BarAttributes.h"
#include "Curve.h"
#include "GraphArrowAttributes.h"
#include "GraphFlagAttributes.h"
#include "Polyline.h"

namespace magics {

class XmlNode;


class Bar : public BarAttributes, public Graph {
public:
    Bar();
    virtual ~Bar();
    // Implements the set method ...
    void set(const XmlNode& node);
    void set(const map<string, string>& map) {
        BarAttributes::set(map);
        Graph::set(map);
    }

    void operator()(Data&, BasicGraphicsObjectContainer&);
    void horizontal(CustomisedPointsList&, BasicGraphicsObjectContainer&);
    void vertical(CustomisedPointsList&, BasicGraphicsObjectContainer&);

    void visit(LegendVisitor&);
    void visit(TopAxisVisitor&);
    void visit(Transformation&, Data&);

    void fullbar(double, double, double, BasicGraphicsObjectContainer&);
    void linebar(double, double, double, BasicGraphicsObjectContainer&);

    void fullbar_x(double, double, double, BasicGraphicsObjectContainer&);
    void linebar_x(double, double, double, BasicGraphicsObjectContainer&);
    void fullbar_y(double, double, double, BasicGraphicsObjectContainer&);
    void linebar_y(double, double, double, BasicGraphicsObjectContainer&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    vector<Text*> texts_;

    typedef void (Bar::*Renderer)(double, double, double, BasicGraphicsObjectContainer&);
    map<string, Renderer> renderers_;


private:
    //! Copy constructor - No copy allowed
    Bar(const Bar&);
    //! Overloaded << operator to copy - No copy allowed
    Bar& operator=(const Bar&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Bar& p) {
        p.print(s);
        return s;
    }
};
class GraphFlag : public GraphFlagAttributes, public Graph {
public:
    GraphFlag();
    virtual ~GraphFlag();

    void set(const XmlNode& node) {
        GraphFlagAttributes::set(node);
        Graph::set(node);
    }

    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);


    // Implements the set method ...
    void set(const map<string, string>& map) { GraphFlagAttributes::set(map); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


private:
    //! Copy constructor - No copy allowed
    GraphFlag(const GraphFlag&);
    //! Overloaded << operator to copy - No copy allowed
    GraphFlag& operator=(const GraphFlag&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GraphFlag& p) {
        p.print(s);
        return s;
    }
};

class GraphArrow : public GraphArrowAttributes, public Graph {
public:
    GraphArrow();
    virtual ~GraphArrow();

    void set(const XmlNode& node) {
        GraphArrowAttributes::set(node);
        Graph::set(node);
    }

    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);


    // Implements the set method ...
    void set(const map<string, string>& map) { GraphArrowAttributes::set(map); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


private:
    //! Copy constructor - No copy allowed
    GraphArrow(const GraphArrow&);
    //! Overloaded << operator to copy - No copy allowed
    GraphArrow& operator=(const GraphArrow&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GraphArrow& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
