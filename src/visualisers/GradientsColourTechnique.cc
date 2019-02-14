/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GradientsColourTechnique.cc
    \brief Implementation of the Template class GradientsColourTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/

#include "GradientsColourTechnique.h"
#include "ColourTableDefinitionCompute.h"
#include "LevelSelection.h"

using namespace magics;

GradientsColourTechnique::GradientsColourTechnique() {}

GradientsColourTechnique::~GradientsColourTechnique() {}

void GradientsColourTechnique::set(LevelSelection& out, LevelSelection& in, ColourTable& table, int nb) const {
    // First make sure that
    ColourTableDefinitionCompute helper;


    if (colours_.size() < 2) {
        MagLog::warning() << " No enough colours given to the gradients method" << endl;
        return;
    }
    vector<double> stops = in;
    if (stops.empty()) {
        MagLog::warning() << " No intervals given to the gradients method, guessing ..." << endl;
        double min       = in.front();
        double max       = in.back();
        double increment = (max - min) / (colours_.size() - 1);
        for (double i = 0; i < colours_.size(); i++)
            stops.push_back(min + (i * increment));
    }

    vector<double>::const_iterator val = stops.begin();
    vector<int>::const_iterator step   = steps_.begin();
    string stop_method                 = lowerCase(stop_method_);

    out.clear();
    in.clear();

    // ColourTable colours;
    int last = colours_.size() - 1;

    vector<int> colours;


    for (int col = 1; col < colours_.size(); ++col) {
        string left  = colours_[col - 1];
        string right = colours_[col];
        int istep    = (steps_.empty()) ? 10 : *step;

        // right
        ColourTableDefinitionCompute helper(left, right, technique_, technique_direction_);

        int from, to, nbcols;

        if (stop_method == "right") {
            if (col == last) {
                nbcols = istep;
                from   = 0;
                to     = nbcols;
            }
            else {
                nbcols = istep + 1;
                from   = 0;
                to     = nbcols - 1;
            }
        }
        else if (stop_method == "left") {
            if (col == 1) {
                nbcols = istep;
                from   = 0;
                to     = nbcols;
            }
            else {
                nbcols = istep + 1;
                from   = 1;
                to     = nbcols;
            }
        }
        else if (stop_method == "ignore") {
            if (col == 1) {
                nbcols = istep + 1;
                from   = 0;
                to     = nbcols - 1;
            }
            else if (col == last) {
                nbcols = istep + 1;
                from   = 1;
                to     = nbcols;
            }
            else {
                nbcols = istep + 2;
                from   = 1;
                to     = nbcols - 1;
            }
        }
        else {
            nbcols = istep;
            from   = 0;
            to     = nbcols;
        }

        ColourTable workingtable;

        helper.set(workingtable, nbcols + 1);


        for (int c = from; c < to; ++c)
            table.push_back(workingtable[c]);

        // Next block
        if (!steps_.empty()) {
            ++step;
            if (step == steps_.end())
                --step;
        }
    }


    step    = steps_.begin();
    int col = 0;

    // Now the interval ...
    for (int stop = 1; stop < stops.size(); ++stop) {
        double from = stops[stop - 1];
        double to   = stops[stop];
        int istep   = (steps_.empty()) ? 10 : *step;

        in.push_back(from);
        out.push_back(from);

        double inc = (to - from) / (istep);

        for (int i = 1; i < istep; i++) {
            in.push_back(from + (i * inc));
            out.push_back(from + (i * inc));
        }

        if (!steps_.empty()) {
            ++step;
            if (step == steps_.end())
                --step;
        }
    }
    in.push_back(stops.back());
    out.push_back(stops.back());
}

/*!
 Class information are given to the output-stream.
*/
void GradientsColourTechnique::print(ostream& out) const {
    out << "GradientsColourTechnique[";
    out << "]";
}

void GradientsColourTechnique::set(const ColourTechniqueInterface& attributes) {}
