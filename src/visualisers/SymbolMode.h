/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolMode.h
    \brief Definition of the Template class SymbolMode.

    Magics Team - ECMWF 2004

    Started: Wed 21-Jan-2004

    Changes:

*/

#ifndef SymbolMode_H
#define SymbolMode_H

#include "magics.h"

#include "SymbolIndividualModeAttributes.h"
#include "SymbolModeAttributes.h"
#include "SymbolTableModeAttributes.h"

#include "Colour.h"
#include "Data.h"
#include "Factory.h"
#include "IntervalMap.h"
#include "MagTranslator.h"
#include "Symbol.h"
#include "Text.h"
#include "UserPoint.h"

namespace magics {

class LegendVisitor;
class HistoVisitor;
class SymbolPlotting;


class SymbolMode : public SymbolModeAttributes {
public:
    SymbolMode();
    virtual ~SymbolMode();

    virtual SymbolMode* clone() const {
        SymbolMode* object = new SymbolMode();
        return object;
    }

    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    virtual void toxml(ostream&, int = 0) const {}

    virtual bool accept(double) { return true; }
    virtual void prepare() {}
    virtual SymbolProperties operator()(double) const { throw OutOfRangeMagException(); }

    void parent(SymbolPlotting* parent) { parent_ = parent; }


    virtual void visit(LegendVisitor&){};
    virtual void visit(Data&, LegendVisitor& legend) { visit(legend); }
    virtual void visit(Data&, HistoVisitor&);


    virtual void adjust(double, double, bool, const Transformation&, double) {}
    void set(const string& type) { type_ = type; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    SymbolPlotting* parent_;
    string type_;

private:
    //! Copy constructor - No copy allowed
    SymbolMode(const SymbolMode&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolMode& operator=(const SymbolMode&);


    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolMode& p) {
        p.print(s);
        return s;
    }
};

class SymbolIndividualMode : public SymbolMode, public SymbolIndividualModeAttributes {
public:
    SymbolIndividualMode();
    virtual ~SymbolIndividualMode();
    virtual void set(const map<string, string>& map) {
        SymbolMode::set(map);
        SymbolIndividualModeAttributes::set(map);
        update();
    }
    virtual void set(const XmlNode& node) {
        SymbolMode::set(node);
        SymbolIndividualModeAttributes::set(node);
        update();
    }

    virtual bool accept(const string& node) { return SymbolIndividualModeAttributes::accept(node); }

    virtual SymbolMode* clone() const {
        SymbolIndividualMode* object = new SymbolIndividualMode();
        // SymbolIndividualModeAttributes::copy(*this);
        return object;
    }

    void adjust(double, double, bool, const Transformation&, double);
    virtual void visit(LegendVisitor&);
    void prepare() {
        update();
        properties();
    }
    void update();
    void properties() const;
    SymbolProperties operator()(double) const { return properties_; }
    void visit(Data&, LegendVisitor&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    mutable SymbolProperties properties_;
    mutable vector<string>::const_iterator current_;


private:
    //! Copy constructor - No copy allowed
    SymbolIndividualMode(const SymbolIndividualMode&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolIndividualMode& operator=(const SymbolIndividualMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolIndividualMode& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, SymbolMode> {
public:
    SymbolMode* operator()(const string& val) { return SimpleObjectMaker<SymbolMode>::create(val); }

    SymbolMode* magics(const string& param) {
        SymbolMode* object;
        ParameterManager::update(param, object);
        return object;
    }
};


class SymbolTableMode : public SymbolMode, public SymbolTableModeAttributes {
public:
    SymbolTableMode();
    virtual ~SymbolTableMode();
    virtual void prepare();
    virtual bool accept(double);
    SymbolProperties operator()(double) const;

    void set(const map<string, string>& map) {
        SymbolTableModeAttributes::set(map);
        SymbolMode::set(map);
        prepare();
    }

    virtual void set(const XmlNode& node) {
        SymbolMode::set(node);
        SymbolTableModeAttributes::set(node);
        prepare();
    }
    virtual bool accept(const string& node) { return SymbolTableModeAttributes::accept(node); }

    void adjust(double, double, bool, const Transformation&, double);

    void visit(LegendVisitor&);
    void visit(Data&, LegendVisitor&);
    void visit(Data&, HistoVisitor&);
    void buildBins(const IntervalMap<SymbolProperties>&, IntervalMap<Colour>&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    IntervalMap<SymbolProperties> map_;

private:
    //! Copy constructor - No copy allowed
    SymbolTableMode(const SymbolTableMode&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolTableMode& operator=(const SymbolTableMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolTableMode& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics

#endif
