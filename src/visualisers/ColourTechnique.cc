/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTechnique.cc
    \brief Implementation of the Template class ColourTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/


#include "ColourTechnique.h"
#include <limits>
#include "LegendVisitor.h"
#include "LevelSelection.h"
#include "MagConfig.h"
#include "Polyline.h"

using namespace magics;

ColourTechnique::ColourTechnique() : policy_(ListPolicy::LASTONE), 
                minOutOfBound_(false), maxOutOfBound_(false)
                {}


ColourTechnique::~ColourTechnique() {}

/*!
 Class information are given to the output-stream.
*/
void ColourTechnique::print(ostream& out) const {
    out << "ColourTechnique[";
    out << "]";
}

void ColourTechnique::prepare(LevelSelection& out, LevelSelection& levels, bool rainbow) {
    if (levels.empty())
        return;

    
    clear();
    bands_.clear();
    ColourTable table;



    minOutOfBound_ = out.minOutOfBond();
    maxOutOfBound_ = out.maxOutOfBond();

     

    if (rainbow)
        set(out, levels, table, levels.size() + 1);
    else {
        
        LevelSelection newlevels;
        int count = levels.size();
        if (out.minOutOfBond()) count--;
           
        if (out.maxOutOfBond()) count--;
        
    
        
        set(out, levels, table, count);
        if (out.minOutOfBond()) {
            Colour min_colour = magCompare(oob_min_colour_, "automatic") ? 
                        table.front().colour() : Colour(oob_min_colour_);
            table.insert(table.begin(), min_colour);
        }
         if (out.maxOutOfBond()) {
            Colour max_colour = magCompare(oob_max_colour_, "automatic") ? 
                                        table.back().colour() : Colour(oob_max_colour_);
            table.push_back(max_colour);
        }
    }


    // cout << "ColourTechnique::after(" << endl;
    // for (auto l = levels.begin(); l != levels.end(); ++l)
    //     cout << *l << endl;

    // cout << "-------------------" << endl;
    

    if (table.empty())
        table.push_back(Colour("none"));
    ColourTable::ColourIterator colour = table.begin();

    Colour left("NONE");
    Colour right = colour->colour();

    double min = std::numeric_limits<double>::max();

    double previous = 0;
    int index       = 0;

    for (LevelSelection::const_iterator val = levels.begin(); val != levels.end(); ++val) {
        if (rainbow) {
            (*this)[*val] = ColourInfo(index, *val, right, right);
        }
        else {
            (*this)[*val] = ColourInfo(index, *val, left, right);
        }
        ranges_[*val] = std::make_pair(previous, *val);
        previous      = *val;
        index++;
        if (min != std::numeric_limits<double>::max()) {
            bands_.insert(make_pair(Interval(min, *val), left));
        }
        min  = *val;
        left = right;
        colour++;
        if (colour == table.end()) {
            right = table.back().colour();
            colour--;
        }
        else
            right = colour->colour();
    }
    if (!rainbow)
        bands_.insert(make_pair(Interval(levels.back(), levels.back() + EPSILON), left));

    

    MagLog::dev() << levels.back() << "<<" << left << "<<" << levels.back() + EPSILON << endl;
}

Colour ColourTechnique::operator()(double value) const {
    const_iterator info = find(value);
    if (info == end()) {
        return Colour(-1, -1, -1);
    }
    Colour colour = info->second.left_;


    return info->second.left_;
}

Colour ColourTechnique::left(double value) const {
    const_iterator info = find(value);
    if (info == end()) {
        // MagLog::warning() << "canot find a colour for " << value << "\n";
        return Colour(-1, -1, -1);
    }
    return info->second.left_;
}

Colour ColourTechnique::right(double value) const {
    const_iterator info = find(value);
    if (info == end()) {
        // MagLog::warning() << "canot find a colour for " << value << "\n";
        return Colour(-1, -1, -1);
    }

    return info->second.right_;
}
double ColourTechnique::leftRange(double value) const {
    map<double, pair<double, double> >::const_iterator info = ranges_.find(value);
    if (info == ranges_.end()) {
        // MagLog::warning() << "canot find a colour for " << value << "\n";
        return INT_MAX;
    }
    return info->second.first;
}

double ColourTechnique::rightRange(double value) const {
    map<double, pair<double, double> >::const_iterator info = ranges_.find(value);
    if (info == ranges_.end()) {
        // MagLog::warning() << "canot find a colour for " << value << "\n";
        return INT_MAX;
    }
    return info->second.second;
}

Colour ColourTechnique::colour(double value) const {
    return bands_.find(value, Colour("none"));
    ;
}


void ColourTechnique::colours(vector<string>& colours) const {
    for (IntervalMap<Colour>::const_iterator band = bands_.begin(); band != bands_.end(); ++band)
        colours.push_back(band->second.name());
}

void ColourTechnique::visit(LegendVisitor& legend) {
    for (IntervalMap<Colour>::const_iterator band = bands_.begin(); band != bands_.end(); ++band) {
        double min    = band->first.min_;
        double max    = band->first.max_;
        Polyline* box = new Polyline();
        box->setColour(Colour("black"));
        box->setFilled(true);
        box->setFillColour(band->second);
        // MagLog::dev()<< "From " << min << " to " << max << "-->" << band->second << endl;
        FillShadingProperties* shading = new FillShadingProperties();


        box->setShading(shading);


        legend.add(new BoxEntry(min, max, box));
    }
}
PaletteColourTechnique::PaletteColourTechnique() {
}

PaletteColourTechnique::~PaletteColourTechnique() {}

#include "ColourTableDefinitionCompute.h"

// technique_ -> rgb/hcl/hsl
// technique_direction_ -> clockwise/anti_clockwise/shortest/longest



void PaletteColourTechnique::set(LevelSelection&, LevelSelection& in, ColourTable& table, int nb)  {
    PaletteLibrary library;
    vector<string> colours;
    Palette palette;
    string name = palette_;   
    library.find(name, palette);

  
    if (palette.colours_.empty()) {
        MagLog::warning() << "Could not load palette " << palette_ << ": using a default one " << endl;
        colours.push_back("blue");
        colours.push_back("green");
        colours.push_back("yellow");
        colours.push_back("orange");
        colours.push_back("red");
    }

    if ( name != palette_ ) {
        list_policy_ = ColourListPolicy::DYNAMIC;
    }

    if ( reverse_ )
        std::reverse(palette.colours_.begin(), palette.colours_.end()); 

    ColourTableDefinitionCompute helper;
    helper.set(palette.colours_, table, nb, list_policy_, palette.method_);

}


void PaletteColourTechnique::print(ostream& out) const {
    out << "GradientsColourTechnique[";
    out << "]";
}

void PaletteColourTechnique::set(const ColourTechniqueInterface& attributes) {}
