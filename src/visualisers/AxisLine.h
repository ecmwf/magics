/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisLine.h
    \brief Definition of the Template class AxisLine.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri 7-Oct-2005

    Changes:

*/

#ifndef AxisLine_H
#define AxisLine_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


#include "AxisLineAttributes.h"

namespace magics {

class Transformation;
class RightAxisVisitor;
class LeftAxisVisitor;
class TopAxisVisitor;
class BottomAxisVisitor;
class PaperPoint;
class Polyline;

class AxisLine : public AxisLineAttributes {
public:
    AxisLine();
    virtual ~AxisLine() override;

    virtual void set(const XmlNode& node) override { AxisLineAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { AxisLineAttributes::set(map); }
    virtual AxisLine* clone() const override {
        AxisLine* line = new AxisLine() override;
        line->copy(*this);
        return line;
    }

    Colour colour();

    Polyline* line() const;

    virtual void horizontal(TopAxisVisitor& out) const override;
    virtual void horizontal(BottomAxisVisitor& out) const override;


    virtual void vertical(LeftAxisVisitor& out) const override;
    virtual void vertical(RightAxisVisitor& out) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    AxisLine(const AxisLine&);
    //! Overloaded << operator to copy - No copy allowed
    AxisLine& operator=(const AxisLine&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisLine& p) {
        p.print(s);
        return s;
    }
};

class NoAxisLine : public AxisLine {
public:
    NoAxisLine() {}
    ~NoAxisLine() override {}
    AxisLine* clone() const override { return new NoAxisLine(); }
    virtual void horizontal(TopAxisVisitor&) const {}
    virtual void horizontal(BottomAxisVisitor&) const {}
    virtual void horizontal(LeftAxisVisitor&) const {}
    virtual void horizontal(RightAxisVisitor&) const {}
    virtual void vertical(TopAxisVisitor&) const {}
    virtual void vertical(BottomAxisVisitor&) const {}
    virtual void vertical(LeftAxisVisitor&) const {}
    virtual void vertical(RightAxisVisitor&) const override {}
};


template <>
class MagTranslator<string, AxisLine> {
public:
    AxisLine* operator()(const string& val) { return SimpleObjectMaker<AxisLine>::create(val); }

    AxisLine* magics(const string& param) {
        AxisLine* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
