/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolPlotting.cc
    \brief Implementation of the Template class SymbolPlotting.

    Magics Team - ECMWF 2004

    Started: Mon 19-Jan-2004

    Changes:

*/

#include "SymbolPlotting.h"
#include "Factory.h"

#include "MagicsFormat.h"
#include "ProgressObject.h"

using namespace magics;


SymbolPlotting::SymbolPlotting() {}

SymbolPlotting::~SymbolPlotting() {}

/*!
 Class information are given to the output-stream.
*/
void SymbolPlotting::print(ostream& out) const {
    out << "SymbolPlotting[";
    SymbolPlottingAttributes::print(out);
    out << "]";
}

double SymbolPlotting::height(const Transformation& transformation, double height) {
    if (scaling_method_ == false)
        return height;

    // get Area !

    return transformation.ratio() * scaling_level_0_ * scaling_factor_;
}


void SymbolPlotting::operator()(const PaperPoint& point, BasicGraphicsObjectContainer& out) const {
    try {
        if (point.missing())
            return;
        if ((*mode_).accept(point.value()) == false)
            return;

        SymbolProperties properties = (*mode_)(point.value());

        string value;

        if (magCompare(type_, "number") || magCompare(type_, "both")) {
            ostringstream nice;

            nice << MagicsFormat(format_, point.value());
            if (point.name() != "")
                nice << "[" << point.name() << "]";
            value = nice.str();
        }


        if (magCompare(type_, "marker_text")) {
            value = point.name();
            if (point.name() == "")
                value = "_FORCE_EMPTY_TEXT_";
        }


        map<SymbolProperties, Symbol*>::const_iterator symb = symbols_.find(properties);
        if (symb != symbols_.end()) {
            symb->second->push_back(point, value);
            return;
        }


        Symbol* symbol = properties.symbol(type_);


        symbols_[properties] = symbol;

        symbol->push_back(point, value);
    }
    catch (...) {
    }
}

struct Print {
    void operator()(const UserPoint& point) { MagLog::debug() << point << "\n"; }
};


struct SortHelper {
    SortHelper() {}
    ~SortHelper() {}
    MAGICS_NO_EXPORT bool operator()(const Symbol* first, const Symbol* second) {
        return first->size() > second->size();
    }
};


void SymbolPlotting::operator()(Data& data, BasicGraphicsObjectContainer& out) {
    mode_->parent(this);
    mode_->prepare();
    symbols_.clear();
    vector<string> check;
    check.push_back("text");
    check.push_back("number");
    check.push_back("marker");
    check.push_back("both");
    check.push_back("marker_text");

    bool valid = false;

    for (vector<string>::iterator c = check.begin(); c != check.end(); ++c) {
        valid = magCompare(*c, type_);
        if (valid)
            break;
    }
    if (!valid) {
        MagLog::warning() << type_ << " not yet implemented : reset to marker " << endl;
        type_ = "marker";
    }
    mode_->set(type_);


    try {
        const Transformation& transformation = out.transformation();

        // If we need to connect the symbols with a line, we need all the poinst
        // to enable proper clipping of the line! Othewise wee just nedde to get the point
        // from the visible area.

        PointsHandler& points = data.points(transformation, connect_);

        // Some Mode need to know the min and max of the data, in order to adjust the
        // computation of the levels
        (*mode_).adjust(points.min(), points.max(), scaling_method_, transformation, scaling_factor_);
        if (legend_only_)
            return;

        points.setToFirst();
        while (points.more()) {
            PaperPoint xy = transformation(points.current());
            (*this)(xy, out);


            points.advance();
        }

        // WE sort: send the longest one first!
        vector<Symbol*> work;
        for (map<SymbolProperties, Symbol*>::iterator symbol = symbols_.begin(); symbol != symbols_.end(); ++symbol)
            work.push_back(symbol->second);

        std::sort(work.begin(), work.end(), SortHelper());

        // Now we feed the task...     f
        for (vector<Symbol*>::iterator symbol = work.begin(); symbol != work.end(); ++symbol) {
            if (!(*symbol)->empty()) {
                (*symbol)->boundingbox(out.transformation().getPCBoundingBox());
                out.push_back(*symbol);
            }
        }

        for (vector<Text*>::iterator text = texts_.begin(); text != texts_.end(); ++text) {
            out.push_back(*text);
        }
    }
    catch (MagicsException&) {
        // do nothing!
    }
}

void SymbolPlotting::visit(Data& data, LegendVisitor& legend) {
    MagLog::debug() << " SymbolPlotting::visit to create a legend ... "
                    << "\n";
    if (!legend_)
        return;
    (*mode_).visit(data, legend);
}

void SymbolPlotting::visit(Data& data, HistoVisitor& histo) {
    MagLog::debug() << " SymbolPlotting::visit to create a histogram! ... "
                    << "\n";

    mode_->visit(data, histo);
}
