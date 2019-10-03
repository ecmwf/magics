/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolyShadingMethod.h
    \brief Definition of the Template class PolyShadingMethod.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:cc

*/


#include "DotPolyShadingMethod.h"
#include "HatchPolyShadingMethod.h"
#include "IsoPlot.h"
#include "LevelSelection.h"
#include "PolyShadingTechnique.h"
using namespace magics;

void PolyShadingMethod::operator()(magics::Polyline& poly) const {
    int index = poly.index();
    if (index < 0)
        return;
    poly.setFilled(true);
    poly.setStroke(true);
    poly.setFilled(true);
    if (index >= colours_.size()) {
        poly.setColour(colours_.back());
        poly.setFillColour(colours_.back());
    }
    else {
        poly.setColour(colours_[index]);
        poly.setFillColour(colours_[index]);
    }
    FillShadingProperties* shading = new FillShadingProperties();
    poly.setShading(shading);
};

void PolyShadingMethod::visit(LegendVisitor& legend, const ColourTechnique& colour) {
    MagLog::dev() << "Create legend information"
                  << "\n";
    LegendEntryBuilder helper(legend, this, colour);
    std::adjacent_find(colour.begin(), colour.end(), helper);
    if (colour.size() == 1) {
        helper(*colour.begin(), *colour.begin());
    }
    legend.last();  // Flag the last entry as being the last! To get a nice labelling in countinuous mode!!!
    helper.setLabels();
}

int PolyShadingMethod::index(double value) {
    if (same(value, last_))
        return indexes_.size() - 1;
    return indexes_.find(value, -1);
}

int PolyShadingMethod::rightIndex(double value) {
    if (same(value, first_))
        return 0;
    if (same(value, last_))
        return -1;
    return indexes_.find(value, -1);
}

int PolyShadingMethod::leftIndex(double value) {
    if (value < first_)
        return -1;
    if (same(value, first_))
        return -1;
    if (same(value, last_))
        return indexes_.size() - 1;
    int index = indexes_.find(value, -1);
    return (index == -1) ? -1 : index - 1;
}

void PolyShadingMethod::prepare(LevelSelection& levels, const ColourTechnique& colours) {
    if (levels.empty())
        return;
    first_ = levels.front();
    last_  = levels.back();

    LevelSelection::const_iterator from  = levels.begin();
    LevelSelection::const_iterator level = levels.begin();
    level++;

    indexes_.clear();
    colours_.clear();

    int index = 0;
    for (; level != levels.end(); ++level) {
        indexes_.insert(make_pair(Interval(*from, *level), index));
        colours_.push_back(colours.right(*from));
        from++;
        index++;
    }
}


void DotPolyShadingMethod::prepare(LevelSelection& levels, const ColourTechnique& colours) {
    if (levels.empty())
        return;

    float step = (max_density_ - min_density_) / (levels.size() - 1);
    first_     = levels.front();
    last_      = levels.back();

    LevelSelection::const_iterator from  = levels.begin();
    LevelSelection::const_iterator level = levels.begin();
    level++;
    float density = min_density_;
    int index     = 0;
    indexes_.clear();
    colours_.clear();
    dots_.clear();
    for (; level != levels.end(); ++level) {
        indexes_.insert(make_pair(Interval(*from, *level), index));
        colours_.push_back(colours.right(*from));
        dots_.push_back(density);
        from++;
        index++;
        density += step;
    }
}


void DotPolyShadingMethod::operator()(magics::Polyline& poly) const {
    DotShadingProperties* shading = new DotShadingProperties();

    int index = poly.index();

    shading->size_    = size_;
    shading->density_ = dots_[index];

    poly.setFilled(true);
    poly.setFillColour(colours_[index]);
    poly.setStroke(false);
    poly.setShading(shading);
}


void HatchPolyShadingMethod::prepare(LevelSelection& levels, const ColourTechnique& colours) {
    int index = 1;
    if (index_ >= 7 || index_ < 0) {
        MagLog::warning() << "index should be < 7--> reset to 1 " << endl;
        index_ = 1;
    }
    first_ = levels.front();
    last_  = levels.back();

    LevelSelection::const_iterator from  = levels.begin();
    LevelSelection::const_iterator level = levels.begin();
    indexes_.clear();
    colours_.clear();
    hatches_.clear();

    level++;
    int i = 0;

    for (; level != levels.end(); ++level) {
        indexes_.insert(make_pair(Interval(*from, *level), i));
        colours_.push_back(colours.right(*from));
        hatches_.push_back((index_) ? index_ : index);
        index++;
        i++;
        from++;
        if (index == 7)
            index = 1;
    }
}

void HatchPolyShadingMethod::operator()(magics::Polyline& poly) const {
    int index = poly.index();

    HatchShadingProperties* shading = new HatchShadingProperties();
    shading->index_                 = hatches_[index];

    shading->density_   = density_;
    shading->thickness_ = thickness_;
    poly.setFilled(true);
    poly.setFillColour(colours_[index]);
    poly.setStroke(false);
    poly.setShading(shading);
}
