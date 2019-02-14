/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolAdvancedTableMode.cc
    \brief Definition of the Template class SymbolAdvancedTableMode.

    Magics Team - ECMWF 2004

    Started: Wed 21-Jan-2004

    Changes:

*/

#ifndef SymbolAdvancedTableMode_H
#define SymbolAdvancedTableMode_H

#include "magics.h"

#include "IntervalMap.h"
#include "SymbolAdvancedTableModeAttributes.h"
#include "SymbolMode.h"

namespace magics {


class SymbolAdvancedTableMode : public SymbolMode,
                                public SymbolAdvancedTableModeAttributes,
                                public LevelSelectionInterface,
                                public ColourTechniqueInterface,
                                public HeightTechniqueInterface,
                                public OutLayerTechniqueInterface {
public:
    SymbolAdvancedTableMode();
    virtual ~SymbolAdvancedTableMode();
    virtual void prepare();

    bool accept(double) const;
    SymbolProperties operator()(double) const;

    void visit(Data&, LegendVisitor&);
    void visit(Data&, HistoVisitor&);


    void set(const map<string, string>& map) {
        SymbolAdvancedTableModeAttributes::set(map);
        SymbolMode::set(map);
        prepare();
    }
    void set(const XmlNode& node) {
        SymbolAdvancedTableModeAttributes::set(node);
        SymbolMode::set(node);
        prepare();
    }


    virtual bool accept(const string& node) { return SymbolAdvancedTableModeAttributes::accept(node); }

    virtual SymbolMode* clone() const {
        SymbolAdvancedTableMode* object = new SymbolAdvancedTableMode();
        object->copy(*this);
        return object;
    }
    void adjust(double, double, bool, const Transformation&, double);
    void copy(const SymbolAdvancedTableMode& other) {
        SymbolAdvancedTableModeAttributes::copy(other);
        SymbolModeAttributes::copy(other);
    }

    int getCount() const { return count_; }
    int getTolerance() const { return tolerance_; }
    double getReference() const { return reference_; }
    double getInterval() const { return interval_; }
    double getMin() const { return min_; }
    double getMax() const { return max_; }
    const Colour& getMinColour() const { return *minColour_; }
    const Colour& getMaxColour() const { return *maxColour_; }
    const string& getDirection() const { return direction_; }
    stringarray getColours() const { return colours_; }
    ListPolicy getPolicy() const { return colour_policy_; }
    floatarray getList() const { return list_; }
    double getMinHeight() const { return height_min_; }
    double getMaxHeight() const { return height_max_; }
    floatarray getHeights() const { return heights_; }
    ListPolicy getHeightPolicy() const { return height_policy_; }
    int getLevels() const { return nbLevels_; }
    float getMinOutlayer() const { return outlayer_min_; }
    float getMaxOutlayer() const { return outlayer_max_; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    IntervalMap<SymbolProperties> map_;
    int nbLevels_;


    typedef bool (SymbolAdvancedTableMode::*TextHandler)(const string&, const SymbolProperties&,
                                                         vector<Text*>& text) const;
    static map<string, TextHandler> textHandlers_;

private:
    //! Copy constructor - No copy allowed
    SymbolAdvancedTableMode(const SymbolAdvancedTableMode&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolAdvancedTableMode& operator=(const SymbolAdvancedTableMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolAdvancedTableMode& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
