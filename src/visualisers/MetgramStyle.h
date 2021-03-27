/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MetgramStyle.h
    \brief Definition of the Template class MetgramStyle.

    Magics Team - ECMWF 2006

    Started: Mon 16-Oct-2006

    Changes:

*/

#ifndef MetgramStyle_H
#define MetgramStyle_H

#include "BasicGraphicsObject.h"
#include "CustomisedPoint.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "MetgramBarAttributes.h"
#include "MetgramCurveAttributes.h"
#include "MetgramFlagsAttributes.h"
#include "PaperPoint.h"
#include "magics.h"

namespace magics {


class LegendVisitor;
class BasicSceneObject;

class MetgramStyle {
public:
    MetgramStyle();
    virtual ~MetgramStyle();

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }
    virtual MetgramStyle* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MetgramStyle();
    }
    virtual void operator()(CustomisedPointsList&, BasicGraphicsObjectContainer&) {}

    virtual void toxml(ostream&) const {}
    virtual void visit(LegendVisitor&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    MetgramStyle(const MetgramStyle&);
    //! Overloaded << operator to copy - No copy allowed
    MetgramStyle& operator=(const MetgramStyle&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MetgramStyle& p) {
        p.print(s);
        return s;
    }
};

class MetgramBar : public MetgramStyle, public MetgramBarAttributes {
public:
    MetgramBar() {}
    virtual ~MetgramBar() override {}

    virtual void set(const XmlNode& node) override { MetgramBarAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { MetgramBarAttributes::set(map); }
    virtual bool accept(const string& node) override { return magCompare(node, "bar"); }
    virtual MetgramStyle* clone() const override { return new MetgramBar(); }

    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};

class MetgramCurve : public MetgramStyle, public MetgramCurveAttributes {
public:
    MetgramCurve() {}
    virtual ~MetgramCurve() override {}

    virtual void set(const XmlNode& node) override { MetgramCurveAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { MetgramCurveAttributes::set(map); }
    virtual MetgramStyle* clone() const override { return new MetgramCurve(); }
    virtual bool accept(const string& node) override { return magCompare(node, "curve"); }
    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task) override;
    virtual void visit(LegendVisitor&) override;
    virtual void set(const PaperPoint&, BasicGraphicsObjectContainer&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};


class MetgramFlags : public MetgramStyle, public MetgramFlagsAttributes {
public:
    MetgramFlags() {}
    virtual ~MetgramFlags() override {}

    virtual void set(const XmlNode& node) override { MetgramFlagsAttributes::set(node); }
    virtual void set(const map<string, string>& node) override { MetgramFlagsAttributes::set(node); }
    virtual bool accept(const string& node) override { return magCompare(node, "flags"); }
    virtual MetgramStyle* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MetgramFlags();
    }

    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};

template <>
class MagTranslator<string, MetgramStyle> {
public:
    MetgramStyle* operator()(const string& val) { return SimpleObjectMaker<MetgramStyle>::create(val); }

    MetgramStyle* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
