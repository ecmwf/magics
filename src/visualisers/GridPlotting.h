/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GridPlotting.h
    \brief Definition of the Template class GridPlotting.

    Magics Team - ECMWF 2004

    Started: Mon 2-Feb-2004

    Changes:

*/

#ifndef GridPlotting_H
#define GridPlotting_H

#include "magics.h"

#include "GridPlottingAttributes.h"
#include "NoGridPlottingAttributes.h"
#include "Polyline.h"
#include "SceneVisitor.h"
#include "UserPoint.h"


namespace magics {


class NoGridPlotting : public NoGridPlottingAttributes {
public:
    NoGridPlotting() {}
    virtual ~NoGridPlotting() {}

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }
    virtual void toxml(ostream&, int = 0) const {}
    virtual NoGridPlotting* clone() const { return new NoGridPlotting(); }
    virtual void operator()(DrawingVisitor&);
    virtual void operator()(PreviewVisitor&) {}

    const vector<double>& longitudes(const Transformation&) const;
    const vector<double>& latitudes(const Transformation&) const;

    const vector<double>& longitudes() const { return longitudes_; }
    const vector<double>& latitudes() const { return latitudes_; }
    void longitudes(vector<double>&, int) const;
    void latitudes(vector<double>&, int) const;
    virtual string colour() const { return "NONE"; }
    virtual void layer(BasicGraphicsObjectContainer*) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "NoGridPlotting\n"; }
    mutable vector<double> latitudes_;
    mutable vector<double> longitudes_;

    mutable double minx_;
    mutable double miny_;
    mutable double maxx_;
    mutable double maxy_;

private:
    //! Copy constructor - No copy allowed
    NoGridPlotting(const NoGridPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    NoGridPlotting& operator=(const NoGridPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoGridPlotting& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, NoGridPlotting> {
public:
    NoGridPlotting* operator()(const string& val) { return SimpleObjectMaker<NoGridPlotting>::create(val); }

    NoGridPlotting* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

class GridPlotting : public NoGridPlotting, public GridPlottingAttributes {
public:
    GridPlotting();
    virtual ~GridPlotting();

    virtual NoGridPlotting* clone() const {
        GridPlotting* object = new GridPlotting();
        object->copy(*this);
        return object;
    }

    virtual void copy(const GridPlotting& other) {
        NoGridPlottingAttributes::copy(other);
        GridPlottingAttributes::copy(other);
    }
    virtual void set(const XmlNode& node) {
        NoGridPlottingAttributes::set(node);
        GridPlottingAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        NoGridPlottingAttributes::set(map);
        GridPlottingAttributes::set(map);
    }
    bool accept(const string& node) { return GridPlottingAttributes::accept(node); }
    virtual void operator()(DrawingVisitor&);
    virtual void operator()(PreviewVisitor&);


    void add(Polyline&) const;
    void addFrame(Polyline&) const;
    void layer(BasicGraphicsObjectContainer* layer) { layer_ = layer; }


    void visit(Transformation&) {}


    virtual string colour() const { return colour_->name(); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


    BasicGraphicsObjectContainer* layer_;

private:
    //! Copy constructor - No copy allowed
    GridPlotting(const GridPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    GridPlotting& operator=(const GridPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GridPlotting& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics
#endif
