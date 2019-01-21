/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisGrid.h
    \brief Definition of the Template class AxisGrid.

    Magics Team - ECMWF 2004

    Started: Fri 7-May-2004

    Changes:

*/

#ifndef AxisGrid_H
#define AxisGrid_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "AxisGridAttributes.h"
#include "AxisItem.h"


namespace magics {


class XmlNode;
class Transformation;
class DrawingVisitor;
class HorizontalAxisVisitor;
class VerticalAxisVisitor;

class AxisGrid : public AxisGridAttributes {
public:
    AxisGrid();
    virtual ~AxisGrid();

    void set(const map<string, string>& map) { AxisGridAttributes::set(map); }
    void set(const XmlNode& node) { AxisGridAttributes::set(node); }

    AxisGrid* clone() {
        AxisGrid* grid = new AxisGrid();
        grid->copy(*this);
        return grid;
    }

    virtual void vertical(const AxisItems&, DrawingVisitor& out) const;
    virtual void horizontal(const AxisItems&, DrawingVisitor& out) const;
    virtual void vertical(const AxisItems&, HorizontalAxisVisitor&) const {}
    virtual void vertical(const AxisItems&, VerticalAxisVisitor&) const {}
    virtual void horizontal(const AxisItems&, HorizontalAxisVisitor&) const {}
    virtual void horizontal(const AxisItems&, VerticalAxisVisitor&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    AxisGrid(const AxisGrid&);
    //! Overloaded << operator to copy - No copy allowed
    AxisGrid& operator=(const AxisGrid&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisGrid& p) {
        p.print(s);
        return s;
    }
};

class NoAxisGrid : public AxisGrid {
public:
    NoAxisGrid() {}
    ~NoAxisGrid() {}

    virtual void vertical(const AxisItems&, DrawingVisitor&) const {}
    virtual void horizontal(const AxisItems&, DrawingVisitor&) const {}

    AxisGrid* clone() {
        AxisGrid* grid = new NoAxisGrid();
        return grid;
    }
};


template <>
class MagTranslator<string, AxisGrid> {
public:
    AxisGrid* operator()(const string& val) { return SimpleObjectMaker<AxisGrid>::create(val); }

    AxisGrid* magics(const string& param) {
        AxisGrid* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
