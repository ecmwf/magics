/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotBasicItem.h
    \brief Definition of the Template class BoxPlotBasicItem.
    \author Meteorological Visualisation Section, ECMWF

    Started: Sep 2006

*/

#ifndef BoxPlotBasicItem_H
#define BoxPlotBasicItem_H

#include "Factory.h"
#include "MagTranslator.h"
#include "Polyline.h"
#include "magics.h"


namespace magics {

class CustomisedPoint;
class PaperPoint;


class NoBoxPlotBox {
public:
    NoBoxPlotBox() {}
    virtual ~NoBoxPlotBox() {}

    void cm(double cm) { cm_ = cm; }

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }
    virtual NoBoxPlotBox* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoBoxPlotBox();
    }
    virtual void toxml(ostream&) const { MagLog::dev() << "toxml(ostream& out, int tab)---> to be checked!...\n"; }
    virtual void operator()(BasicGraphicsObjectContainer&, const CustomisedPoint&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    double cm_;

private:
    //! Copy constructor - No copy allowed
    NoBoxPlotBox(const NoBoxPlotBox&);
    //! Overloaded << operator to copy - No copy allowed
    NoBoxPlotBox& operator=(const NoBoxPlotBox&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoBoxPlotBox& p) {
        p.print(s);
        return s;
    }
};


class NoBoxPlotBoxBorder {
public:
    NoBoxPlotBoxBorder() {}
    virtual ~NoBoxPlotBoxBorder() {}

    void cm(double cm) { cm_ = cm; }

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }
    virtual NoBoxPlotBoxBorder* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoBoxPlotBoxBorder();
    }
    virtual void toxml(ostream&) const { MagLog::dev() << "toxml(ostream& out, int tab)---> to be checked!...\n"; }

    virtual void operator()(Polyline& poly) const { poly.setStroke(false); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    double cm_;

private:
    //! Copy constructor - No copy allowed
    NoBoxPlotBoxBorder(const NoBoxPlotBoxBorder&);
    //! Overloaded << operator to copy - No copy allowed
    NoBoxPlotBoxBorder& operator=(const NoBoxPlotBoxBorder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoBoxPlotBoxBorder& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoBoxPlotBoxBorder> {
public:
    NoBoxPlotBoxBorder* operator()(const string& val) { return SimpleObjectMaker<NoBoxPlotBoxBorder>::create(val); }

    NoBoxPlotBoxBorder* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};


template <>
class MagTranslator<string, NoBoxPlotBox> {
public:
    NoBoxPlotBox* operator()(const string& val) { return SimpleObjectMaker<NoBoxPlotBox>::create(val); }

    NoBoxPlotBox* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};


class NoBoxPlotMedian {
public:
    NoBoxPlotMedian() {}
    virtual ~NoBoxPlotMedian() {}
    virtual bool accept(const string&) { return false; }
    void cm(double cm) { cm_ = cm; }

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual NoBoxPlotMedian* clone() const { return new NoBoxPlotMedian(); }
    virtual void toxml(ostream&) const {}

    virtual void operator()(BasicGraphicsObjectContainer&, Polyline*) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    double cm_;

private:
    //! Copy constructor - No copy allowed
    NoBoxPlotMedian(const NoBoxPlotMedian&);
    //! Overloaded << operator to copy - No copy allowed
    NoBoxPlotMedian& operator=(const NoBoxPlotMedian&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoBoxPlotMedian& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoBoxPlotMedian> {
public:
    NoBoxPlotMedian* operator()(const string& val) { return SimpleObjectMaker<NoBoxPlotMedian>::create(val); }

    NoBoxPlotMedian* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

class NoBoxPlotWhisker {
public:
    NoBoxPlotWhisker() {}
    virtual ~NoBoxPlotWhisker() {}
    virtual bool accept(const string&) { return false; }

    void cm(double cm) { cm_ = cm; }

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual NoBoxPlotWhisker* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoBoxPlotWhisker();
    }
    virtual void toxml(ostream&) const { MagLog::dev() << "toxml(ostream& out, int tab)---> to be checked!...\n"; }

    virtual void top(BasicGraphicsObjectContainer&, const CustomisedPoint&) const {}
    virtual void bottom(BasicGraphicsObjectContainer&, const CustomisedPoint&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    double cm_;

private:
    //! Copy constructor - No copy allowed
    NoBoxPlotWhisker(const NoBoxPlotWhisker&);
    //! Overloaded << operator to copy - No copy allowed
    NoBoxPlotWhisker& operator=(const NoBoxPlotWhisker&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoBoxPlotWhisker& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoBoxPlotWhisker> {
public:
    NoBoxPlotWhisker* operator()(const string& val) { return SimpleObjectMaker<NoBoxPlotWhisker>::create(val); }

    NoBoxPlotWhisker* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

class NoBoxPlotWhiskerBorder {
public:
    NoBoxPlotWhiskerBorder() {}
    virtual ~NoBoxPlotWhiskerBorder() {}
    virtual bool accept(const string&) { return false; }

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual NoBoxPlotWhiskerBorder* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoBoxPlotWhiskerBorder();
    }
    virtual void toxml(ostream&) const { MagLog::dev() << "toxml(ostream& out, int tab)---> to be checked!...\n"; }

    virtual void operator()(Polyline&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    NoBoxPlotWhiskerBorder(const NoBoxPlotWhiskerBorder&);
    //! Overloaded << operator to copy - No copy allowed
    NoBoxPlotWhiskerBorder& operator=(const NoBoxPlotWhiskerBorder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoBoxPlotWhiskerBorder& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, NoBoxPlotWhiskerBorder> {
public:
    NoBoxPlotWhiskerBorder* operator()(const string& val) {
        return SimpleObjectMaker<NoBoxPlotWhiskerBorder>::create(val);
    }

    NoBoxPlotWhiskerBorder* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // end namespace magics

#endif
