/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphShadeStyle.h
    \brief Definition of the Template class GraphShadeStyle.

    Magics Team - ECMWF 2006

    Started: Thu 17-Aug-2006

    Changes:

*/

#ifndef GraphShadeStyle_H
#define GraphShadeStyle_H

#include "BasicGraphicsObject.h"
#include "DotGraphShadeStyleAttributes.h"
#include "Factory.h"
#include "GraphShadeStyleAttributes.h"
#include "HatchGraphShadeStyleAttributes.h"
#include "MagTranslator.h"
#include "magics.h"

namespace magics {

class Polyline;
class PaperPoint;


class GraphShadeStyle : public GraphShadeStyleAttributes {
public:
    GraphShadeStyle();
    virtual ~GraphShadeStyle() override;

    virtual void set(const XmlNode& node) override { GraphShadeStyleAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { GraphShadeStyleAttributes::set(map); }
    virtual GraphShadeStyle* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new GraphShadeStyle();
    }
    // virtual void operator()(Polyline*, BasicGraphicsObjectContainer&) ;
    virtual void operator()(Polyline&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    GraphShadeStyle(const GraphShadeStyle&);
    //! Overloaded << operator to copy - No copy allowed
    GraphShadeStyle& operator=(const GraphShadeStyle&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GraphShadeStyle& p) {
        p.print(s);
        return s;
    }
};

class DotGraphShadeStyle : public GraphShadeStyle, public DotGraphShadeStyleAttributes {
public:
    DotGraphShadeStyle() {}
    virtual ~DotGraphShadeStyle() override {}

    virtual void set(const XmlNode& node) override {
        GraphShadeStyle::set(node);
        DotGraphShadeStyleAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) override {
        GraphShadeStyle::set(map);
        DotGraphShadeStyleAttributes::set(map);
    }
    virtual GraphShadeStyle* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new DotGraphShadeStyle();
    }
    // void operator()(Polyline*, BasicGraphicsObjectContainer&);
    virtual void operator()(Polyline&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};

class HatchGraphShadeStyle : public GraphShadeStyle, public HatchGraphShadeStyleAttributes {
public:
    HatchGraphShadeStyle() {}
    virtual ~HatchGraphShadeStyle() override {}

    virtual void set(const XmlNode& node) override {
        GraphShadeStyle::set(node);
        HatchGraphShadeStyleAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) override {
        GraphShadeStyle::set(map);
        HatchGraphShadeStyleAttributes::set(map);
    }
    virtual GraphShadeStyle* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new HatchGraphShadeStyle();
    }
    // void operator()(Polyline*, BasicGraphicsObjectContainer&);
    void operator()(Polyline&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};


template <>
class MagTranslator<string, GraphShadeStyle> {
public:
    GraphShadeStyle* operator()(const string& val) { return SimpleObjectMaker<GraphShadeStyle>::create(val); }


    GraphShadeStyle* magics(const string& param) {
        GraphShadeStyle* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
