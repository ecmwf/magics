/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphShade.h
    \brief Definition of the Template class GraphShade.

    Magics Team - ECMWF 2006

    Started: Thu 17-Aug-2006

    Changes:

*/

#ifndef GraphShade_H
#define GraphShade_H

#include "BasicGraphicsObject.h"
#include "CustomisedPoint.h"
#include "Factory.h"
#include "GraphShadeAttributes.h"
#include "MagTranslator.h"
#include "Polyline.h"
#include "magics.h"

namespace magics {

class Polyline;
class PaperPoint;
class UserPoint;

class GraphShade : public GraphShadeAttributes {
public:
    GraphShade();
    virtual ~GraphShade();

    virtual void set(const XmlNode& node) { GraphShadeAttributes::set(node); }
    virtual void set(const map<string, string>& map) { GraphShadeAttributes::set(map); }
    virtual GraphShade* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new GraphShade();
    }

    virtual void operator()(Polyline&);
    virtual void legend(Polyline&);
    virtual void operator()(CustomisedPointsList&, vector<UserPoint>&);
    virtual bool needCustomised() { return true; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    GraphShade(const GraphShade&);
    //! Overloaded << operator to copy - No copy allowed
    GraphShade& operator=(const GraphShade&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GraphShade& p) {
        p.print(s);
        return s;
    }
};

class NoGraphShade : public GraphShade {
public:
    NoGraphShade() {}
    virtual ~NoGraphShade() {}

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual GraphShade* clone() const { return new NoGraphShade(); }

    virtual void operator()(Polyline& poly) { poly.setFilled(false); }
    virtual void legend(Polyline&) {}
    void operator()(CustomisedPointsList&, vector<UserPoint>&);
    bool needCustomised() { return false; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
};


template <>
class MagTranslator<string, GraphShade> {
public:
    GraphShade* operator()(const string& val) { return SimpleObjectMaker<GraphShade>::create(val); }

    GraphShade* magics(const string& param) {
        GraphShade* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
