/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LegendMethod.cc
    \brief Implementation of the Template class LegendMethod.

    Magics Team - ECMWF 2006

    Started: Tue 9-May-2006

    Changes:

*/

#include "LegendMethod.h"
#include "LegendVisitor.h"


using namespace magics;

LegendMethod::LegendMethod() {}


LegendMethod::~LegendMethod() {}

/*!
 Class information are given to the output-stream.
*/
void LegendMethod::print(ostream& out) const {
    out << "LegendMethod[";
    out << "]";
}

void LegendMethod::row(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) {
    PaperPoint middle(x, y);
    entry.set(middle, task);

    legend.setJustification(MLEFT);
    legend.push_back(entry.leftTextBox(middle));  // WE attach the text on the rigth of the sumbol!
}

void LegendMethod::column(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) {
    PaperPoint middle(x, y);
    entry.set(middle, task);

    legend.setJustification(MLEFT);
    legend.push_back(entry.leftTextBox(middle));  // WE attach the text on the rigth of the sumbol!
}

void ContinuousLegendMethod::row(LegendEntry& entry, double x, double y, Text& legend,
                                 BasicGraphicsObjectContainer& task) {
    if (labelCount_ % label_frequency_ != 0)
        entry.notext();
    PaperPoint middle(x, y);
    entry.rowBox(middle, task);
    if (labelCount_ % label_frequency_ == 0)
        if (entry.needContinuousText(legend)) {
            middle.y_ -= 0.5;
            legend.push_back(middle);  // We attach the text on the top middle of the symbol!
        }

    labelCount_++;
}

void ContinuousLegendMethod::column(LegendEntry& entry, double x, double y, Text& legend,
                                    BasicGraphicsObjectContainer& task) {
    if (labelCount_ % label_frequency_ != 0)
        entry.notext();
    entry.columnBox(PaperPoint(x, y), task);
    if (labelCount_ % label_frequency_ == 0)

        if (entry.needContinuousText(legend)) {
            legend.push_back(PaperPoint(x + 0.25, y));  // WE attach the text on the right of the sumbol!
        }
    labelCount_++;
}
void HistogramLegendMethod::row(LegendEntry& entry, double x, double y, Text&, BasicGraphicsObjectContainer& out) {
    if (labelCount_ % label_frequency_ != 0)
        entry.notext();
    PaperPoint middle(x, y);
    Colour colour = histo_border_ ? *histo_border_colour_ : Colour("automatic");
    ostringstream m;
    m << "magics_" << histo_mean_marker_;
    entry.histogramInformation(this);
    entry.rowHisto(middle, out, colour);
    labelCount_++;
}

void HistogramLegendMethod::column(LegendEntry& entry, double x, double y, Text&, BasicGraphicsObjectContainer& out) {
    if (labelCount_ % label_frequency_ != 0)
        entry.notext();
    PaperPoint middle(x, y);
    Colour colour = histo_border_ ? *histo_border_colour_ : Colour("automatic");
    ostringstream m;
    m << "magics_" << histo_mean_marker_;
    entry.histogramInformation(this);
    entry.columnHisto(middle, out, colour);
    labelCount_++;
}
