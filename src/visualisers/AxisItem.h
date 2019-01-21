/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisItem.h
    \brief Definition of the Template class AxisItem.

    Magics Team - ECMWF 2005

    Started: Tue 11-Oct-2005

    Changes:

*/

#ifndef AxisItem_H
#define AxisItem_H

#include "AutoVector.h"
#include "DateTime.h"
#include "magics.h"


namespace magics {

class Axis;
class MagFont;

class AxisItem {
public:
    AxisItem(double position, const string& label, int level);
    AxisItem(double position, const string& label, int level, const string& colour, double height);
    AxisItem(double x, const string& fmt);
    virtual ~AxisItem();
    virtual AxisItem* clone() const {
        AxisItem* item = new AxisItem(position_, label_, level_, colour_, height_);
        return item;
    }
    virtual const string& id() const { return label_; }

    virtual void format(const string&, int) const {}

    virtual bool sunday() const { return false; }
    virtual bool runday() const { return false; }

    virtual bool date() const { return false; }

    virtual bool isFirst() { return false; }
    virtual bool isLast() { return false; }

    double position() const { return position_; }
    void position(double position) { position_ = position; }

    const string& label() const { return label_; }
    const string& colour() const { return colour_; }
    void colour(const string& colour) { colour_ = colour; }
    void resetColour() const { colour_ = "undef"; }
    double height() const { return height_; }
    void height(double height, const string& font, const string& style) const {
        height_ = height;
        font_   = font;
        style_  = style;
    }
    void label(const string& label) { label_ = label; }

    void setFont(MagFont& font);

    int level() { return level_; }
    void level(int level) { level_ = level; }
    virtual void id(const string&) {}

    virtual bool isTick() const { return true; }
    virtual bool isLabel() const { return true; }
    virtual bool isGrid() const { return true; }
    virtual bool isMinorTick() const { return false; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    double position_;
    mutable string label_;
    int level_;
    mutable string colour_;
    mutable double height_;
    mutable string font_;
    mutable string style_;

private:
    //! Copy constructor - No copy allowed
    AxisItem(const AxisItem&);
    //! Overloaded << operator to copy - No copy allowed
    AxisItem& operator=(const AxisItem&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisItem& p) {
        p.print(s);
        return s;
    }
};


class AxisHyperItem : public AxisItem {
public:
    AxisHyperItem(double, vector<double>&);
    ~AxisHyperItem();
};

class AxisTickItem : public AxisItem {
public:
    AxisTickItem(double pos, const string& format) : AxisItem(pos, format) { label_ = ""; }
    ~AxisTickItem() {}
    bool isLabel() const { return false; }
};

class AxisDateItem : public AxisItem {
public:
    AxisDateItem(double position, DateTime date, int level = 0, const string& colour = "undef", double height = 0.3);
    ~AxisDateItem() {}
    AxisItem* clone() const {
        AxisDateItem* item = new AxisDateItem(position_, date_, level_, colour_, height_);
        return item;
    }
    const string& id() const { return id_; }
    void format(const string&, int) const;

    string label(const Axis&) const { return label_; }

    bool sunday() const;
    bool runday() const;

    bool date() const { return true; }
    void id(const string& id) { id_ = id; }

    bool isTick() const { return false; }
    bool isGrid() const { return false; }

protected:
    DateTime date_;
    mutable string defaultColour_;
    mutable string id_;
};

class AxisMinorTickItem : public AxisItem {
public:
    AxisMinorTickItem(double pos) : AxisItem(pos, "") { label_ = ""; }
    ~AxisMinorTickItem() {}
    bool isTick() const { return false; }
    bool isLabel() const { return false; }
    bool isGrid() const { return false; }
    bool isMinorTick() const { return true; }
};


typedef AutoVector<AxisItem> AxisItems;


}  // namespace magics
#endif
