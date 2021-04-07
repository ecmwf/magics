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
    virtual ~SymbolAdvancedTableMode() override;
    virtual void prepare() override;

    bool accept(double) const;
    SymbolProperties operator()(double) const override;

    void visit(Data&, LegendVisitor&) override;
    void visit(Data&, HistoVisitor&) override;


    void set(const map<string, string>& map) override {
        SymbolAdvancedTableModeAttributes::set(map);
        SymbolMode::set(map);
        prepare();
    }
    void set(const XmlNode& node) override {
        SymbolAdvancedTableModeAttributes::set(node);
        SymbolMode::set(node);
        prepare();
    }


    virtual bool accept(const string& node) override { return SymbolAdvancedTableModeAttributes::accept(node); }

    virtual SymbolMode* clone() const override {
        SymbolAdvancedTableMode* object = new SymbolAdvancedTableMode();
        object->copy(*this);
        return object;
    }
    void adjust(double, double, bool, const Transformation&, double) override;
    void copy(const SymbolAdvancedTableMode& other) {
        SymbolAdvancedTableModeAttributes::copy(other);
        SymbolModeAttributes::copy(other);
    }

    int getCount() const override { return count_; }
    int getTolerance() const override { return tolerance_; }
    double getReference() const override { return reference_; }
    double getInterval() const override { return interval_; }
    double getMin() const override { return min_; }
    double getMax() const override { return max_; }
    const Colour& getMinColour() const override { return *minColour_; }
    const Colour& getMaxColour() const override { return *maxColour_; }
    const string& getDirection() const override { return direction_; }
    stringarray getColours() const override { return colours_; }
    ListPolicy getPolicy() const override { return colour_policy_; }
    floatarray getList() const override { return list_; }
    double getMinHeight() const override { return height_min_; }
    double getMaxHeight() const override { return height_max_; }
    floatarray getHeights() const override { return heights_; }
    ListPolicy getHeightPolicy() const override { return height_policy_; }
    int getLevels() const { return nbLevels_; }
    float getMinOutlayer() const override { return outlayer_min_; }
    float getMaxOutlayer() const override { return outlayer_max_; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
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
