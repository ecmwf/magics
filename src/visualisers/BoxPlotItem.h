/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotItem.h
    \brief Definition of the Template class BoxPlotItem.

    Magics Team - ECMWF 2006

    Started: Fri 29-Sep-2006

    Changes:

*/

#ifndef BoxPlotItem_H
#define BoxPlotItem_H

#include "BoxPlotBasicItem.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "BoxPlotBoxAttributes.h"
#include "BoxPlotBoxBorderAttributes.h"
#include "BoxPlotMedianAttributes.h"
#include "BoxPlotWhiskerBorderAttributes.h"
#include "BoxPlotWhiskerBoxAttributes.h"
#include "BoxPlotWhiskerLineAttributes.h"

namespace magics {


class BoxPlotBox : public NoBoxPlotBox, public BoxPlotBoxAttributes {
public:
    BoxPlotBox() {}
    virtual ~BoxPlotBox() override {}


    void set(const XmlNode& node) override { BoxPlotBoxAttributes::set(node); }
    void set(const map<string, string>& map) override { BoxPlotBoxAttributes::set(map); }
    bool accept(const string& node) override { return BoxPlotBoxAttributes::accept(node); }
    BoxPlotBox* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotBox();
    }

    void operator()(BasicGraphicsObjectContainer&, const CustomisedPoint&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotBox[]"; }

private:
    //! Copy constructor - No copy allowed
    BoxPlotBox(const BoxPlotBox&);
    //! Overloaded << operator to copy - No copy allowed
    BoxPlotBox& operator=(const BoxPlotBox&);
};


class BoxPlotBoxBorder : public NoBoxPlotBoxBorder, public BoxPlotBoxBorderAttributes {
public:
    BoxPlotBoxBorder() {}
    virtual ~BoxPlotBoxBorder() override {}

    virtual void set(const XmlNode& node) override { BoxPlotBoxBorderAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { BoxPlotBoxBorderAttributes::set(map); }
    bool accept(const string& node) override { return BoxPlotBoxBorderAttributes::accept(node); }
    virtual NoBoxPlotBoxBorder* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotBoxBorder();
    }


    virtual void operator()(Polyline&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotBoxBorder[]"; }
};


class BoxPlotMedian : public NoBoxPlotMedian, public BoxPlotMedianAttributes {
public:
    BoxPlotMedian() {}
    virtual ~BoxPlotMedian() override {}

    virtual void set(const XmlNode& node) override { BoxPlotMedianAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { BoxPlotMedianAttributes::set(map); }
    bool accept(const string& node) override { return BoxPlotMedianAttributes::accept(node); }

    virtual BoxPlotMedian* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotMedian();
    }


    virtual void operator()(BasicGraphicsObjectContainer&, Polyline*) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotMedian[]"; }
};


class BoxPlotWhiskerBorder : public NoBoxPlotWhiskerBorder, public BoxPlotWhiskerBorderAttributes {
public:
    BoxPlotWhiskerBorder() {}
    virtual ~BoxPlotWhiskerBorder() override {}

    virtual void set(const XmlNode& node) override { BoxPlotWhiskerBorderAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { BoxPlotWhiskerBorderAttributes::set(map); }
    bool accept(const string& node) override { return BoxPlotWhiskerBorderAttributes::accept(node); }

    virtual NoBoxPlotWhiskerBorder* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerBorder();
    }

    void operator()(Polyline&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotWhiskerBorder[]"; }
};


class BoxPlotWhiskerBox : public NoBoxPlotWhisker, public BoxPlotWhiskerBoxAttributes {
public:
    BoxPlotWhiskerBox() {}
    virtual ~BoxPlotWhiskerBox() override {}

    virtual void set(const XmlNode& node) override { BoxPlotWhiskerBoxAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { BoxPlotWhiskerBoxAttributes::set(map); }
    virtual BoxPlotWhiskerBox* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerBox();
    }
    bool accept(const string& node) override { return BoxPlotWhiskerBoxAttributes::accept(node); }


    void top(BasicGraphicsObjectContainer&, const CustomisedPoint&) const override;
    void bottom(BasicGraphicsObjectContainer&, const CustomisedPoint&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotWhiskerBox[]"; }
};

class BoxPlotWhiskerLine : public NoBoxPlotWhisker, public BoxPlotWhiskerLineAttributes {
public:
    BoxPlotWhiskerLine() {}
    virtual ~BoxPlotWhiskerLine() override {}

    virtual void set(const XmlNode& node) override { BoxPlotWhiskerLineAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { BoxPlotWhiskerLineAttributes::set(map); }
    bool accept(const string& node) override { return BoxPlotWhiskerLineAttributes::accept(node); }
    virtual BoxPlotWhiskerLine* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerLine();
    }

    void top(BasicGraphicsObjectContainer&, const CustomisedPoint&) const override;
    void bottom(BasicGraphicsObjectContainer&, const CustomisedPoint&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "BoxPlotWhiskerLine[]"; }
};


}  // namespace magics

#endif
