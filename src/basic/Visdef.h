/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Visdef.h
    \brief Definition of the Template class Visdef.

    Magics Team - ECMWF 2007

    Started: Fri 16-Mar-2007

    Changes:

*/

#ifndef Visdef_H
#define Visdef_H

#include "BasicSceneObject.h"
#include "Data.h"
#include "LegendVisitor.h"
#include "magics.h"

namespace magics {

class MetaDataVisitor;
class Transformation;
class HistoVisitor;
class Colour;
template <class T>
class IntervalMap;


class Visdef : public MetviewIcon {
public:
    Visdef() {}
    virtual ~Visdef() {}


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) = 0;
    virtual void visit(LegendVisitor&) {}
    virtual void visit(Data&, LegendVisitor& legend) { this->visit(legend); }
    virtual void visit(Data&, HistoVisitor&) { MagLog::dev() << "Here could build a basic histogram!" << endl; }
    virtual void visit(MetaDataVisitor&) {}
    virtual void visit(TopAxisVisitor&) {}
    virtual void visit(Transformation&, Data&) {}
    virtual void visit(Layer& layer) { MetviewIcon::visit(layer); }
    virtual void beanInfo(IntervalMap<Colour>&) { NOTIMP; }
    virtual bool needLegend() { return false; }
    virtual void getReady(const LegendVisitor& legend) { legendOnly_ = legend.only_; }
    void theme(const string& theme) { theme_ = theme; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "visdef"; }

    bool legendOnly_;
    string theme_;

private:
    //! Copy constructor - No copy allowed
    Visdef(const Visdef&);
    //! Overloaded << operator to copy - No copy allowed
    Visdef& operator=(const Visdef&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Visdef& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics

#endif
