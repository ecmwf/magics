/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTable.h
    \brief Definition of the Template class ColourTable.

    Magics Team - ECMWF 2005

    Started: Wed 6-Apr-2005

    Changes:

*/

#ifndef ColourTable_H
#define ColourTable_H

#include "Colour.h"
#include "magics.h"


namespace magics {


class ColourTableEntry {
public:
    ColourTableEntry(const Colour& colour) : min_(INT_MAX), max_(INT_MIN), colour_(colour) {}
    ~ColourTableEntry() {}
    // void visit(LegendEntryList&) const;
    void value(double val) {
        if (min_ > val)
            min_ = val;
        if (val > max_)
            max_ = val;
    }

    const Colour& colour() const { return colour_; }

    double red() const { return colour_.red(); }
    double blue() const { return colour_.blue(); }
    double green() const { return colour_.green(); }
    double alpha() const { return colour_.alpha(); }

    bool operator==(const string& other) const { return colour_ == other; }


protected:
    double min_;
    double max_;
    Colour colour_;
    void print(ostream&) const;
    friend ostream& operator<<(ostream& s, const ColourTableEntry& p) {
        p.print(s);
        return s;
    }
};

class ColourTable : public magvector<ColourTableEntry> {
public:
    typedef magvector<ColourTableEntry>::const_iterator ColourIterator;
    ColourTable();
    virtual ~ColourTable();
    virtual void prepare();

    // void visit(LegendEntryList&) const;
    void push_pack(const Colour& colour) { push_back(ColourTableEntry(colour)); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourTable& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
