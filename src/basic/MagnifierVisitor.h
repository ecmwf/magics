/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagnifierVisitor.h
    \brief Definition of the Template class NoMagnifierVisitor.

    Magics Team - ECMWF 2009

    Started: Tue 27-Jan-2009

    Changes:

*/

#ifndef MagnifierVisitor_H
#define MagnifierVisitor_H

#include "Factory.h"
#include "Layout.h"
#include "MagTranslator.h"
#include "MagnifierVisitorAttributes.h"
#include "PaperPoint.h"
#include "SceneVisitor.h"
#include "Symbol.h"
#include "magics.h"

namespace magics {


class NoMagnifierVisitor : public SceneVisitor, public MagnifierLayout {
public:
    NoMagnifierVisitor();
    virtual ~NoMagnifierVisitor() override;

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual bool accept(const string&) { return false; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual void toxml(ostream&, int = 0) const {}
    virtual NoMagnifierVisitor* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoMagnifierVisitor();
    }
    void visit(magics::BasicSceneObject&) override {}
    virtual void add(const PaperPoint&) {}
    virtual void owner(BasicSceneObject* owner) { owner_ = owner; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    BasicSceneObject* owner_;


private:
    //! Copy constructor - No copy allowed
    NoMagnifierVisitor(const NoMagnifierVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    NoMagnifierVisitor& operator=(const NoMagnifierVisitor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoMagnifierVisitor& p) {
        p.print(s);
        return s;
    }
};

class MagnifierVisitor : public NoMagnifierVisitor, public MagnifierVisitorAttributes {
public:
    MagnifierVisitor();

    virtual ~MagnifierVisitor() override;
    void visit(BasicGraphicsObjectContainer&) override;
    virtual NoMagnifierVisitor* clone() const override {
        MagnifierVisitor* visitor = new MagnifierVisitor();
        visitor->owner_           = owner_;
        visitor->copy(*this);
        return visitor;
    }
    virtual void set(const XmlNode& node) override { MagnifierVisitorAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { MagnifierVisitorAttributes::set(map); }
    void redisplay(const BaseDriver& driver) const override;
    void redisplay(const BaseDriver& driver, vector<PaperPoint>&, float, float) const override;
    void visit(magics::BasicSceneObject& object) override { object.visit(*this); }
    void add(const PaperPoint&) override;
    void addMore(const PaperPoint&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    mutable TextSymbol* values_;
    mutable Symbol* more_;
};

template <>
class MagTranslator<string, NoMagnifierVisitor> {
public:
    NoMagnifierVisitor* operator()(const string& val) { return SimpleObjectMaker<NoMagnifierVisitor>::create(val); }

    NoMagnifierVisitor* magics(const string& param) {
        NoMagnifierVisitor* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
