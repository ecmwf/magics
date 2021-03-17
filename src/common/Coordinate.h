/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Coordinate.h
    \brief Definition of the Template class Coordinate.

    Magics Team - ECMWF 2006

    Started: Thu 10-Aug-2006

    Changes:

*/

#ifndef Coordinate_H
#define Coordinate_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "XRegularCoordinateAttributes.h"
#include "YRegularCoordinateAttributes.h"

#include "XLogarithmicCoordinateAttributes.h"
#include "YLogarithmicCoordinateAttributes.h"

#include "XDateCoordinateAttributes.h"
#include "YDateCoordinateAttributes.h"

#include "MagDateTime.h"
#include "UserPoint.h"
#include "XHyperCoordinateAttributes.h"
#include "YHyperCoordinateAttributes.h"

#include "XmlNode.h"

#include <limits>
namespace magics {

class XmlNode;

class Coordinate {
public:
    Coordinate();
    virtual ~Coordinate();

    virtual double min() { return 0; }
    virtual double max() { return 100; }
    virtual double minpc() { return 0; }
    virtual double maxpc() { return 100; }
    virtual vector<double> maxs() {
        vector<double> vmax;
        vmax.push_back(max());
        return vmax;
    }
    virtual vector<double> mins() {
        vector<double> vmin;
        vmin.push_back(min());
        return vmin;
    }
    virtual string reference() { return ""; }


    virtual double operator()(double c) { return c; }
    virtual double revert(double c) { return c; }

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual void metadata(ostream& out) { out << "axis : {}"; }
    virtual void set() {}
    virtual bool accept(const string&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return false;
    }

    virtual void toxml(ostream&) const {}
    virtual AxisAutomaticSetting automatic() { return AxisAutomaticSetting::OFF; }
    virtual void automatic(bool) {}

    virtual double operator()(const string& val) const { return tonumber(val); }
    virtual string type() const { return "regular"; }

    virtual void minmax(double max, double min) { NOTIMP; }

    virtual void dataMinMax(double min, double max)                = 0;
    virtual void dataMinMax(double min, double max, const string&) = 0;

    virtual void setAutomatic(AxisAutomaticSetting) {}
    virtual void getNewDefinition(const UserPoint&, const UserPoint&, map<string, string>&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    Coordinate(const Coordinate&);
    //! Overloaded << operator to copy - No copy allowed
    Coordinate& operator=(const Coordinate&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Coordinate& p) {
        p.print(s);
        return s;
    }
};

class XCoordinate : public Coordinate {
public:
    XCoordinate() {}
    virtual ~XCoordinate() override {}
    virtual XCoordinate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new XCoordinate();
    }
    virtual void toxml(ostream& out) const override {}
    void set(const XmlNode& node) override {}
    void set(const map<string, string>& map) override {}
    void set() override {}
    void print(ostream& out) const override {}
    virtual AxisAutomaticSetting automatic() override { return AxisAutomaticSetting::OFF; }
    virtual void automatic(bool val) override {}
    bool accept(const string& tag) override { return false; }
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const XCoordinate& p) {
        p.print(s);
        return s;
    }
    virtual void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        def["x_axis_type"] = type();
        def["x_min"]       = tostring(ll.x_);
        def["x_max"]       = tostring(ur.x_);
        def["x_automatic"] = "off";
    }
    virtual void dataMinMax(double min, double max) override { minmax(min, max); }
    virtual void dataMinMax(double min, double max, const string&) override { minmax(min, max); }
};

class YCoordinate : public Coordinate {
public:
    YCoordinate() {}
    virtual ~YCoordinate() override {}
    virtual void toxml(ostream& out) const override {}
    virtual YCoordinate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new YCoordinate();
    }
    virtual void automatic(bool val) override {}
    void set(const XmlNode& node) override {}
    void set(const map<string, string>& map) override {}
    void set() override {}
    void print(ostream& out) const override {}
    bool accept(const string& tag) override { return false; }
    virtual AxisAutomaticSetting automatic() override { return AxisAutomaticSetting::OFF; }
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const YCoordinate& p) {
        p.print(s);
        return s;
    }
    virtual void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        def["y_axis_type"] = type();
        def["y_min"]       = tostring(ll.y_);
        def["y_max"]       = tostring(ur.y_);
        def["y_automatic"] = "off";
    }
    virtual void dataMinMax(double min, double max) override { minmax(min, max); }
    virtual void dataMinMax(double min, double max, const string&) override { minmax(min, max); }
};

class RegularCoordinate {
public:
    RegularCoordinate() {}
    virtual ~RegularCoordinate() {}
};

class XRegularCoordinate : public RegularCoordinate, public XCoordinate, public XRegularCoordinateAttributes {
public:
    XRegularCoordinate() { set(); }
    virtual ~XRegularCoordinate() override {}
    void toxml(ostream& out) const override { XRegularCoordinateAttributes::toxml(out); }
    void set() override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                if (reverse_) {
                    max_ = std::numeric_limits<double>::max();
                    min_ = -max_;
                }
                else {
                    min_ = std::numeric_limits<double>::max();
                    max_ = -min_;
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                min_ = std::numeric_limits<double>::max();
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                max_ = -std::numeric_limits<double>::max();
                break;
            default:
                break;
        }
    }

    virtual void set(const XmlNode& node) override {
        XRegularCoordinateAttributes::set(node);
        set();
    }
    void set(const map<string, string>& map) override {
        XRegularCoordinateAttributes::set(map);
        set();
    }
    virtual bool accept(const string& node) override { return XRegularCoordinateAttributes::accept(node); }
    double min() override { return min_; }
    double max() override { return max_; }
    double minpc() override { return min_; }
    double maxpc() override { return max_; }

    AxisAutomaticSetting automatic() override { return automatic_; }
    void setAutomatic(AxisAutomaticSetting automatic) override { automatic_ = automatic; }
    virtual void automatic(bool automatic) override {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
        set();
    }

    void minmax(double min, double max) override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    max_ = std::min(min, max_);
                    min_ = std::max(max, min_);
                }
                else {
                    min_ = std::min(min, min_);
                    max_ = std::max(max, max_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    max_ = std::min(min, max_);
                else
                    min_ = std::min(min, min_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    min_ = std::max(max, min_);
                else
                    max_ = std::max(max, max_);
                break;
            default:
                break;
        }
    }
    virtual XCoordinate* clone() const override {
        XRegularCoordinate* x = new XRegularCoordinate();
        x->copy(*this);
        return x;
    }


protected:
    virtual void print(ostream& out) const override { XRegularCoordinateAttributes::print(out); }
};

class YRegularCoordinate : public RegularCoordinate, public YCoordinate, public YRegularCoordinateAttributes {
public:
    YRegularCoordinate() { set(); }
    virtual ~YRegularCoordinate() override {}

    void toxml(ostream& out) const override { YRegularCoordinateAttributes::toxml(out); }

    void set() override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                if (reverse_) {
                    max_ = std::numeric_limits<double>::max();
                    min_ = -max_;
                }
                else {
                    min_ = std::numeric_limits<double>::max();
                    max_ = -min_;
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:

                min_ = std::numeric_limits<double>::max();
                break;
            case AxisAutomaticSetting::MAX_ONLY:

                max_ = -std::numeric_limits<double>::max();
                break;
            default:
                break;
        }
    }
    double min() override { return min_; }
    double max() override { return max_; }
    double minpc() override { return min_; }
    double maxpc() override { return max_; }
    virtual void setAutomatic(AxisAutomaticSetting automatic) override {
        automatic_ = automatic;
        set();
    }
    void minmax(double min, double max) override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    max_ = std::min(min, max_);
                    min_ = std::max(max, min_);
                }
                else {
                    min_ = std::min(min, min_);
                    max_ = std::max(max, max_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    max_ = std::min(min, max_);
                else
                    min_ = std::min(min, min_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    min_ = std::max(max, min_);
                else
                    max_ = std::max(max, max_);
                break;
            default:
                break;
        }
    }

    virtual void automatic(bool automatic) override {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
        set();
    }
    virtual YCoordinate* clone() const override {
        YRegularCoordinate* y = new YRegularCoordinate();
        y->copy(*this);
        return y;
    }
    void set(const XmlNode& node) override {
        YRegularCoordinateAttributes::set(node);
        set();
    }
    void set(const map<string, string>& map) override {
        YRegularCoordinateAttributes::set(map);
        set();
    }
    bool accept(const string& node) override { return YRegularCoordinateAttributes::accept(node); }

    void setMin(double min) { min_ = min; }
    void setMax(double max) { max_ = max; }
    AxisAutomaticSetting automatic() override { return automatic_; }


protected:
    virtual void print(ostream& out) const override { YRegularCoordinateAttributes::print(out); }
};


class LogarithmicCoordinate : public Coordinate {
public:
    LogarithmicCoordinate() {}
    virtual ~LogarithmicCoordinate() override {}
    string type() const override { return "logarithmic"; }
};

class XLogarithmicCoordinate : public LogarithmicCoordinate,
                               public XCoordinate,
                               public XLogarithmicCoordinateAttributes {
public:
    XLogarithmicCoordinate() {}
    virtual ~XLogarithmicCoordinate() override {}
    virtual void toxml(ostream& out) const override { XLogarithmicCoordinateAttributes::toxml(out); }
    double min() override { return min_; }
    double max() override { return max_; }
    double minpc() override { return (*this)(min_); }
    double maxpc() override { return (*this)(max_); }
    void set(const XmlNode& node) override {
        if (!magCompare(node.name(), "x_logarithmic"))
            return;
        XmlNode regular = node;
        regular.name("x_regular");
        XLogarithmicCoordinateAttributes::set(regular);
        set();
    }

    void set(const map<string, string>& map) override {
        XLogarithmicCoordinateAttributes::set(map);
        set();
    }
    void set() override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                if (reverse_) {
                    max_ = std::numeric_limits<double>::max();
                    min_ = -max_;
                }
                else {
                    min_ = std::numeric_limits<double>::max();
                    max_ = -min_;
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                automatic_ = AxisAutomaticSetting::OFF;
                min_       = std::numeric_limits<double>::max();
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                automatic_ = AxisAutomaticSetting::OFF;
                max_       = -std::numeric_limits<double>::max();
                break;
            default:
                break;
        }
    }
    void minmax(double min, double max) override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    max_ = std::min(min, max_);
                    min_ = std::max(max, min_);
                }
                else {
                    min_ = std::min(min, min_);
                    max_ = std::max(max, max_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    max_ = std::min(min, max_);
                else
                    min_ = std::min(min, min_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    min_ = std::max(max, min_);
                else
                    max_ = std::max(max, max_);
                break;
            default:
                break;
        }
    }

    double operator()(double c) override { return (c) ? log10(c) : 0; }
    double revert(double c) override { return pow(c, 10); }

    virtual XCoordinate* clone() const override {
        XLogarithmicCoordinate* x = new XLogarithmicCoordinate();
        x->copy(*this);
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return x;
    }

    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        def["x_axis_type"] = "logarithmic";
        def["x_min"]       = tostring(ll.x_);
        def["x_max"]       = tostring(ur.x_);
        def["x_automatic"] = "off";
    }

    virtual void dataMinMax(double min, double max) override { NOTIMP; }
    virtual void dataMinMax(double min, double max, const string&) override { NOTIMP; }
    AxisAutomaticSetting automatic() override { return automatic_; }
    virtual void automatic(bool automatic) override {
        automatic_ = (automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF);
    }
    virtual void setAutomatic(AxisAutomaticSetting automatic) override { automatic_ = automatic; }

protected:
    virtual void print(ostream& out) const override {
        out << "XLogarithmicCoordinate[";
        XLogarithmicCoordinateAttributes::print(out);
        out << "]";
    }
};


class YLogarithmicCoordinate : public LogarithmicCoordinate,
                               public YCoordinate,
                               public YLogarithmicCoordinateAttributes {
public:
    YLogarithmicCoordinate() {}
    virtual ~YLogarithmicCoordinate() override {}
    virtual void set(const XmlNode& node) override {
        if (!magCompare(node.name(), "y_logarithmic"))
            return;
        XmlNode regular = node;
        regular.name("y_regular");
        YLogarithmicCoordinateAttributes::set(regular);
        set();
    }

    virtual void set(const map<string, string>& map) override {
        YLogarithmicCoordinateAttributes::set(map);
        set();
    }
    virtual void toxml(ostream& out) const override { YLogarithmicCoordinateAttributes::toxml(out); }
    void set() override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                if (reverse_) {
                    max_ = std::numeric_limits<double>::max();
                    min_ = -max_;
                }
                else {
                    min_ = std::numeric_limits<double>::max();
                    max_ = -min_;
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                min_ = std::numeric_limits<double>::max();
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                max_ = -std::numeric_limits<double>::max();
                break;
            default:
                break;
        }
    }
    double min() override { return min_; }
    double max() override { return max_; }
    double minpc() override { return (*this)(min_); }
    double maxpc() override { return (*this)(max_); }
    virtual void dataMinMax(double min, double max) override { NOTIMP; }
    virtual void dataMinMax(double min, double max, const string&) override { NOTIMP; }
    void minmax(double min, double max) override {
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    max_ = std::min(min, max_);
                    min_ = std::max(max, min_);
                }
                else {
                    min_ = std::min(min, min_);
                    max_ = std::max(max, max_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    max_ = std::min(min, max_);
                else
                    min_ = std::min(min, min_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    min_ = std::max(max, min_);
                else
                    max_ = std::max(max, max_);
                break;
            default:
                break;
        }
    }
    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        def["y_axis_type"] = "logarithmic";
        def["y_min"]       = tostring(::pow(10., ll.y_));
        def["y_max"]       = tostring(::pow(10., ur.y_));
        def["y_automatic"] = "off";
    }
    AxisAutomaticSetting automatic() override { return automatic_; }

    void setMinMax(double min, double max) {
        min_ = min;
        max_ = max;
    }

    double operator()(double c) override { return (c) ? log10(c) : 0; }
    double revert(double c) override { return ::pow(10., c); }
    virtual YCoordinate* clone() const override {
        YLogarithmicCoordinate* y = new YLogarithmicCoordinate();
        y->copy(*this);
        return y;
    }

    virtual void automatic(bool automatic) override {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }
    virtual void setAutomatic(AxisAutomaticSetting automatic) override { automatic_ = automatic; }

protected:
    virtual void print(ostream& out) const override {
        out << "YLogarithmicCoordinate[";
        YLogarithmicCoordinateAttributes::print(out);
        out << "]";
    }
};


class DateCoordinate {
public:
    DateCoordinate() {}
    virtual ~DateCoordinate() {}
    virtual string type() const { return "date"; }
};

class XDateCoordinate : public DateCoordinate, public XDateCoordinateAttributes, public XCoordinate {
public:
    XDateCoordinate() { anna_ = false; }
    virtual ~XDateCoordinate() override {}
    virtual void toxml(ostream& out) const override { XDateCoordinateAttributes::toxml(out); }
    double min() override { return 0; }
    double max() override { return DateTime(date_max_) - DateTime(date_min_); }

    double minpc() override { return anna_ ? min_anna_ : 0; }
    double maxpc() override { return anna_ ? max_anna_ : max(); }

    void min(double min) {
        min_anna_ = min;
        anna_     = true;
    }
    void max(double max) {
        max_anna_ = max;
        anna_     = true;
    }

    string reference() override { return DateTime(date_min_); }
    virtual XCoordinate* clone() const override {
        XDateCoordinate* x = new XDateCoordinate();

        return x;
    }
    virtual string type() const override { return "date"; }
    AxisAutomaticSetting automatic() override {
        return (automatic_ != AxisAutomaticSetting::OFF) ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }
    void set(const XmlNode& node) override { XDateCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) override { XDateCoordinateAttributes::set(map); }
    bool accept(const string& xml) override { return XDateCoordinateAttributes::accept(xml); }

    void setMin(double) {}
    void setMax(double) {}

    void automatic(bool automatic) override {
        automatic_ = (automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF);
    }
    virtual void setAutomatic(AxisAutomaticSetting automatic) override { automatic_ = (automatic); }

    void setMinMax(const string& min, const string& max) {
        date_min_ = min;
        date_max_ = max;
    }


    void dataMinMax(double min, double max, const string& date) override {
        DateTime base(date);
        DateTime mind = base + Second(min);
        DateTime maxd = base + Second(max);
        if (date_min_.empty())
            date_min_ = string(mind);
        if (date_max_.empty())
            date_max_ = string(maxd);
        DateTime mind_(date_min_);
        DateTime maxd_(date_max_);

        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    maxd_ = std::min(mind, maxd_);
                    mind_ = std::max(maxd, mind_);
                }
                else {
                    mind_ = std::min(mind, mind_);
                    maxd_ = std::max(maxd, maxd_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    maxd_ = std::min(mind, maxd_);
                else
                    mind_ = std::min(mind, mind_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    mind_ = std::max(maxd, mind_);
                else
                    maxd_ = std::max(maxd, maxd_);
                break;
            default:
                break;
        }
        date_max_ = string(maxd_);
        date_min_ = string(mind_);
    }


    double operator()(double c) override { return c; }

    double operator()(const string& val) const override {
        DateTime date(val);
        return date - DateTime(date_min_);
    }

    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        DateTime min       = DateTime(date_min_) + Second(ll.x_);
        DateTime max       = DateTime(date_min_) + Second(ur.x_);
        def["x_axis_type"] = "date";
        def["x_date_min"]  = tostring(min);
        def["x_date_max"]  = tostring(max);
        def["x_automatic"] = "off";
    }


protected:
    virtual void print(ostream& out) const override { XDateCoordinateAttributes::print(out); }
    // patch for anna!

    double min_anna_;
    double max_anna_;
    bool anna_;
};


class YDateCoordinate : public DateCoordinate, public YDateCoordinateAttributes, public YCoordinate {
public:
    YDateCoordinate() {}
    virtual ~YDateCoordinate() override {}
    virtual void toxml(ostream& out) const override { YDateCoordinateAttributes::toxml(out); }
    double min() override { return 0; }
    double max() override { return DateTime(date_max_) - DateTime(date_min_); }
    double minpc() override { return 0; }
    double maxpc() override { return DateTime(date_max_) - DateTime(date_min_); }
    string reference() override { return DateTime(date_min_); }
    virtual string type() const override { return "date"; }
    bool accept(const string& xml) override { return YDateCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) override { YDateCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) override { YDateCoordinateAttributes::set(map); }
    void setAutomatic(AxisAutomaticSetting automatic) override { automatic_ = (automatic); }
    void automatic(bool automatic) override {
        automatic_ = (automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF);
    }

    virtual YCoordinate* clone() const override {
        YDateCoordinate* y = new YDateCoordinate();
        // y->copy(*this);
        return y;
    }
    AxisAutomaticSetting automatic() override {
        return (automatic_ != AxisAutomaticSetting::OFF) ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }

    void setMinMax(double, double) {}


    void setMinMax(const string& min, const string& max) {
        date_min_ = min;
        date_max_ = max;
    }


    double operator()(double c) override { return c; }
    double operator()(const string& val) const override {
        DateTime date(val);
        return date - DateTime(date_min_);
    }

    void dataMinMax(double min, double max, const string& date) override {
        DateTime base(date);
        DateTime mind = base + Second(min);
        DateTime maxd = base + Second(max);
        if (date_min_.empty())
            date_min_ = string(mind);
        if (date_max_.empty())
            date_max_ = string(maxd);
        DateTime mind_(date_min_);
        DateTime maxd_(date_max_);

        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
                // set the
                if (reverse_) {
                    maxd_ = std::min(mind, maxd_);
                    mind_ = std::max(maxd, mind_);
                }
                else {
                    mind_ = std::min(mind, mind_);
                    maxd_ = std::max(maxd, maxd_);
                }
                break;
            case AxisAutomaticSetting::MIN_ONLY:
                if (reverse_)
                    maxd_ = std::min(mind, maxd_);
                else
                    mind_ = std::min(mind, mind_);
                break;
            case AxisAutomaticSetting::MAX_ONLY:
                if (reverse_)
                    mind_ = std::max(maxd, mind_);
                else
                    maxd_ = std::max(maxd, maxd_);
                break;
            default:
                break;
        }
        date_max_ = string(maxd_);
        date_min_ = string(mind_);
    }


    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        DateTime min       = DateTime(date_min_) + Second(ll.y_);
        DateTime max       = DateTime(date_min_) + Second(ur.y_);
        def["y_axis_type"] = "date";
        def["y_date_min"]  = tostring(min);
        def["y_date_max"]  = tostring(max);
        def["y_automatic"] = "off";
    }

protected:
    virtual void print(ostream& out) const override { YDateCoordinateAttributes::print(out); }
};


class YHyperCoordinate : public YHyperCoordinateAttributes, public YCoordinate {
public:
    YHyperCoordinate() {}
    virtual ~YHyperCoordinate() override {}
    double min() override { return (min_lon_ == max_lon_) ? min_lat_ : min_lon_; }
    double max() override { return (min_lon_ == max_lon_) ? max_lat_ : max_lon_; }
    double minpc() override { return (min_lon_ == max_lon_) ? min_lat_ : min_lon_; }
    double maxpc() override { return (min_lon_ == max_lon_) ? max_lat_ : max_lon_; }
    string reference() override { return ""; }
    void toxml(ostream& out) const override { YHyperCoordinateAttributes::toxml(out); }
    bool accept(const string& xml) override { return YHyperCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) override { YHyperCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) override { YHyperCoordinateAttributes::set(map); }
    void setAutomatic(AxisAutomaticSetting automatic) override { YCoordinate::setAutomatic(automatic); }
    void setAutomatic(bool automatic) {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }
    void automatic(bool automatic) override {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }

    virtual YCoordinate* clone() const override {
        YHyperCoordinate* y = new YHyperCoordinate();
        // y->copy(*this);
        return y;
    }
    AxisAutomaticSetting automatic() override { return automatic_; }

    void setMinMax(double, double) {}


    vector<double> mins() override {
        vector<double> mins;
        mins.push_back(min_lon_);
        mins.push_back(min_lat_);
        return mins;
    }
    vector<double> maxs() override {
        vector<double> maxs;
        maxs.push_back(max_lon_);
        maxs.push_back(max_lat_);
        return maxs;
    }
    string type() const override { return "geoline"; }
    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        double lon1 = ll.y();
        double lon2 = ur.y();

        double lat1 =
            (max_lat_ - min_lat_) ? min_lat_ + ((lon1 - min_lon_) / (max_lon_ - min_lon_)) * (max_lat_ - min_lat_) : 0;
        double lat2 =
            (max_lat_ - min_lat_) ? min_lat_ + ((lon2 - min_lon_) / (max_lon_ - min_lon_)) * (max_lat_ - min_lat_) : 0;
        def["y_automatic"]     = "off";
        def["y_axis_type"]     = "geoline";
        def["y_min_latitude"]  = tostring(lat1);
        def["y_max_latitude"]  = tostring(lat2);
        def["y_min_longitude"] = tostring(lon1);
        def["y_max_longitude"] = tostring(lon2);
    }
    void dataMinMax(double min, double max, const string& info) override {
        // interpret the info : lonmin/latmin
        Tokenizer tokenizer("/");
        vector<string> tokens;
        tokenizer(info, tokens);
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:

            case AxisAutomaticSetting::MIN_ONLY: {
                if (reverse_) {
                    max_lon_ = tonumber(tokens[0]);
                    max_lat_ = tonumber(tokens[1]);
                }
                else {
                    min_lon_ = tonumber(tokens[0]);
                    min_lat_ = tonumber(tokens[1]);
                }
                break;
            }
            default:
                break;
        }
        automatic_ =
            (automatic_ == AxisAutomaticSetting::BOTH) ? AxisAutomaticSetting::MAX_ONLY : AxisAutomaticSetting::OFF;


        // interpret the info : lonmin/latmin

        tokens.clear();
        tokenizer(info, tokens);
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
            case AxisAutomaticSetting::MAX_ONLY: {
                if (reverse_) {
                    min_lon_ = tonumber(tokens[2]);
                    min_lat_ = tonumber(tokens[3]);
                }
                else {
                    max_lon_ = tonumber(tokens[2]);
                    max_lat_ = tonumber(tokens[3]);
                }

                break;
            }
            default:
                break;
        }
        automatic_ =
            (automatic_ == AxisAutomaticSetting::BOTH) ? AxisAutomaticSetting::MIN_ONLY : AxisAutomaticSetting::OFF;
    }

protected:
    virtual void print(ostream& out) const override { YHyperCoordinateAttributes::print(out); }
};

class XHyperCoordinate : public XHyperCoordinateAttributes, public XCoordinate {
public:
    XHyperCoordinate() {}
    virtual ~XHyperCoordinate() override {}
    double min() override { return (min_lon_ == max_lon_) ? min_lat_ : min_lon_; }
    double max() override { return (min_lon_ == max_lon_) ? max_lat_ : max_lon_; }
    double minpc() override { return (min_lon_ == max_lon_) ? min_lat_ : min_lon_; }
    double maxpc() override { return (min_lon_ == max_lon_) ? max_lat_ : max_lon_; }
    string reference() override { return ""; }
    void toxml(ostream& out) const override { XHyperCoordinateAttributes::toxml(out); }
    bool accept(const string& xml) override { return XHyperCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) override { XHyperCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) override { XHyperCoordinateAttributes::set(map); }
    void setAutomatic(AxisAutomaticSetting automatic) override { XCoordinate::setAutomatic(automatic); }
    void setAutomatic(bool automatic) {
        automatic_ = (automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF);
    }
    void automatic(bool automatic) override {
        automatic_ = automatic ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }
    vector<double> mins() override {
        vector<double> mins;
        mins.push_back(min_lon_);
        mins.push_back(min_lat_);
        return mins;
    }
    vector<double> maxs() override {
        vector<double> maxs;
        maxs.push_back(max_lon_);
        maxs.push_back(max_lat_);
        return maxs;
    }

    virtual XCoordinate* clone() const override {
        XHyperCoordinate* x = new XHyperCoordinate();

        return x;
    }
    AxisAutomaticSetting automatic() override {
        return (automatic_ != AxisAutomaticSetting::OFF) ? AxisAutomaticSetting::BOTH : AxisAutomaticSetting::OFF;
    }

    void setMinMax(double, double) {}


    string type() const override { return "geoline"; }
    void dataMinMax(double min, double max, const string& info) override {
        // interpret the info : lonmin/latmin
        Tokenizer tokenizer("/");
        vector<string> tokens;
        tokenizer(info, tokens);
        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
            case AxisAutomaticSetting::MIN_ONLY: {
                if (reverse_) {
                    max_lon_ = tonumber(tokens[0]);
                    max_lat_ = tonumber(tokens[1]);
                }
                else {
                    min_lon_ = tonumber(tokens[0]);
                    min_lat_ = tonumber(tokens[1]);
                }
                break;
            }
            default:
                break;
        }

        automatic_ =
            (automatic_ == AxisAutomaticSetting::BOTH) ? AxisAutomaticSetting::MAX_ONLY : AxisAutomaticSetting::OFF;

        // interpret the info : lonmin/latmin

        switch (automatic_) {
            case AxisAutomaticSetting::BOTH:
            case AxisAutomaticSetting::MAX_ONLY: {
                if (reverse_) {
                    min_lon_ = tonumber(tokens[2]);
                    min_lat_ = tonumber(tokens[3]);
                }
                else {
                    max_lon_ = tonumber(tokens[2]);
                    max_lat_ = tonumber(tokens[3]);
                }
            }
            default:
                break;
        }
        automatic_ =
            (automatic_ == AxisAutomaticSetting::BOTH) ? AxisAutomaticSetting::MIN_ONLY : AxisAutomaticSetting::OFF;
    }
    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const override {
        double lon1 = ll.x();
        double lon2 = ur.x();

        // we have to compute the lat and lon of the point!

        double lat1 =
            (max_lat_ - min_lat_) ? min_lat_ + ((lon1 - min_lon_) / (max_lon_ - min_lon_)) * (max_lat_ - min_lat_) : 0;
        double lat2 =
            (max_lat_ - min_lat_) ? min_lat_ + ((lon2 - min_lon_) / (max_lon_ - min_lon_)) * (max_lat_ - min_lat_) : 0;
        def["x_automatic"]     = "off";
        def["x_axis_type"]     = "geoline";
        def["x_min_latitude"]  = tostring(lat1);
        def["x_max_latitude"]  = tostring(lat2);
        def["x_min_longitude"] = tostring(lon1);
        def["x_max_longitude"] = tostring(lon2);
    }

protected:
    virtual void print(ostream& out) const override { XHyperCoordinateAttributes::print(out); }
};

template <>
class MagTranslator<string, XCoordinate> {
public:
    XCoordinate* operator()(const string& val) { return SimpleObjectMaker<XCoordinate>::create(val); }

    XCoordinate* magics(const string& param) {
        XCoordinate* object;
        ParameterManager::update(param, object);
        return object;
    }
};
template <>
class MagTranslator<string, YCoordinate> {
public:
    YCoordinate* operator()(const string& val) { return SimpleObjectMaker<YCoordinate>::create(val); }

    YCoordinate* magics(const string& param) {
        YCoordinate* object;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics
#endif
