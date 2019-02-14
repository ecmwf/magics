/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CellShading.cc
    \brief Implementation of the Template class CellShading.

    Magics Team - ECMWF 2005

    Started: Tue 30-Aug-2005

    Changes:

*/

#include "CellShading.h"
#include <algorithm>
#include <limits>
#include "Image.h"
#include "IsoPlot.h"
#include "Symbol.h"

using namespace magics;


CellShading::CellShading() : shading_("grid"), adaptive_(false) {}


CellShading::~CellShading() {}

int CellShading::index(double value) {
    return map_.find(value, -1);
}

void CellShading::operator()(magics::Polyline* poly) const {
    int index = poly->index();
    if (index < 0)
        return;

    poly->setStroke(false);
    poly->setFilled(true);
    if (index >= colours_.size())
        poly->setFillColour(colours_.back());
    else
        poly->setFillColour(colours_[index]);
    FillShadingProperties* shading = new FillShadingProperties();
    poly->setShading(shading);
}

void CellShading::operator()(IsoPlot* iso, MatrixHandler& data, BasicGraphicsObjectContainer& parent) {
    // Here we have to work using the parentput projection.

    const Transformation& projection = parent.transformation();

    double minc = projection.getMinPCX();
    double maxc = projection.getMaxPCX();
    double minr = projection.getMinPCY();
    double maxr = projection.getMaxPCY();

    double width  = parent.absoluteWidth();   // in cm
    double height = parent.absoluteHeight();  // in cm
    shading_      = "grid";

    PaperPoint middle((maxc - minc) / 2, (maxr - minr) / 2);

    adaptive_ = false;
    shading_  = "cell";
    MagLog::debug() << "minx=" << minc << endl;
    MagLog::debug() << "maxx=" << maxc << endl;
    MagLog::debug() << "miny=" << minr << endl;
    MagLog::debug() << "maxy=" << maxr << endl;
    MagLog::debug() << "width=" << width << endl;
    MagLog::debug() << "height=" << height << endl;
    MagLog::debug() << "resolution=" << resolution_ << endl;

    int rows     = int(height * resolution_);
    int columns  = int(width * resolution_);
    double stepr = (maxr - minr) / (rows - 1);
    double stepc = (maxc - minc) / (columns - 1);
    MagLog::debug() << "stepy=" << stepr << endl;
    MagLog::debug() << "stepx=" << stepc << endl;
    MagLog::debug() << "rows=" << rows << endl;
    MagLog::debug() << "columns=" << columns << endl;

    UserPoint point, point1;
    double value;
    PaperPoint middle2(middle.x() + stepc, middle.y() + stepr);
    projection.revert(middle, point);
    projection.revert(middle2, point1);

    UserPoint nextdata(point.x() + data.XResolution(), point.y() + data.YResolution());
    double distance_plot = projection.distance(point, point1);
    double distance_data = projection.distance(point, nextdata);

    if (magCompare(resolution_method_, "adaptive")) {
        if (distance_plot < distance_data) {
            iso->isoline(data, parent);
            MagLog::info() << "Magics will use grid shading" << endl;
            adaptive_ = true;
            return;
        }
    }

    Image* image = new Image();
    image->set(rows, columns);
    MagLog::debug() << "Creation of a bitmap [rows, columns] --> [" << rows << ", " << columns << "]" << endl;

    double lat = maxr;
    double lon = minc;
    for (int row = 0; row < rows; row++) {
        lon = minc;
        for (int column = 0; column < columns; column++) {
            projection.revert(PaperPoint(lon, lat), point);
            lon += stepc;
            if (point.x_ == -1000 && point.x_ == -1000) {
                image->push_back(0);
                continue;
            }

            value = (magCompare(method_, "nearest")) ? data.nearest(point.y(), point.x())
                                                     : data.interpolate(point.y(), point.x());

            image->push_back(map_.find(value, 0));
        }
        lat -= stepr;
    }

    ColourTable table;
    vector<Colour>::const_iterator colour = colours_.begin();
    for (int i = 0; i <= *max_element(image->begin(), image->end()); i++) {
        table.push_back(*colour);
        if (++colour == colours_.end())
            colour = colours_.begin();
    }

    PaperPoint pp(minc, maxr);
    image->setOrigin(pp);
    MagLog::debug() << "origin--->" << pp << endl;
    image->setWidth(maxc - minc);
    image->setHeight(maxr - minr);
    image->setColourTable(table);

    parent.push_back(image);
}

DumpShading::DumpShading() {}

DumpShading::~DumpShading() {}

void DumpShading::operator()(IsoPlot*, MatrixHandler& data, BasicGraphicsObjectContainer& parent) {
    // Here we have to work using the parentput projection.

    const Transformation& projection = parent.transformation();

    double minc = projection.getMinPCX();
    double maxc = projection.getMaxPCX();
    double minr = projection.getMinPCY();
    double maxr = projection.getMaxPCY();

    Image* image = new Image();
    image->set(data.rows(), data.columns());

    for (int row = 0; row < data.rows(); row++) {
        for (int column = 0; column < data.columns(); column++) {
            image->push_back(map_.find(data(row, column), 0));
        }
    }

    ColourTable table;
    vector<Colour>::const_iterator colour = colours_.begin();
    for (int i = 0; i <= *max_element(image->begin(), image->end()); i++) {
        table.push_back(*colour);
        if (++colour == colours_.end())
            colour = colours_.begin();
    }

    PaperPoint pp(minc, maxr);
    image->setOrigin(pp);
    MagLog::debug() << "origin--->" << pp << endl;

    image->setColourTable(table);
    image->setWidth(maxc - minc);
    image->setHeight(maxr - minr);

    parent.push_back(image);
}

bool CellShading::prepare(LevelSelection& levels, const ColourTechnique& technique) {
    // First Interval ...
    map_.clear();
    colours_.clear();
    map_[Interval(INT_MIN, levels.front())] = 0;
    colours_.push_back(Colour("none"));
    for (unsigned int i = 0; i < levels.size() - 1; i++) {
        map_[Interval(levels[i], levels[i + 1])] = i + 1;
        colours_.push_back(technique.right(levels[i]));
    }
    map_[Interval(levels.back(), std::numeric_limits<double>::max())] = levels.size();
    colours_.push_back(Colour("none"));
    return false;
}

#include "PolyShadingTechnique.h"

void CellShading::visit(LegendVisitor& node, const ColourTechnique&) {
    // MagLog::dev() << "Create legend information"  << "\n";
    // LegendEntryBuilder helper(legend, colours_);

    // std::adjacent_find(colours_.begin(), colours_.end(), helper);

    // if ( colours_.size() == 1 ) {
    // 	helper(*colours_.begin(), *colours_.begin());
    // }
    bool first = true;
    for (IntervalMap<int>::const_iterator interval = map_.begin(); interval != map_.end(); ++interval) {
        magics::Polyline* box = new magics::Polyline();

        double min = interval->first.min_;
        double max = interval->first.max_;

        // We ignore the first and the last entries: no interest in the legend!
        if (interval->second == 0)
            continue;
        if (interval->second == int(map_.size() - 1))
            continue;

        box->setFilled(true);
        box->setFillColour(colours_[interval->second]);

        FillShadingProperties* shading = new FillShadingProperties();
        box->setShading(shading);


        LegendEntry* entry = new BoxEntry(min, max, box);
        if (first) {
            first = false;
            entry->first();
        }
        for (vector<double>::iterator val = node.values_list_.begin(); val != node.values_list_.end(); ++val) {
            if (min <= *val && *val < max) {
                string text = tostring(*val);
                entry->userText(text, "user");
                break;
            }
        }

        if (node.values_list_.size() && same(node.values_list_.back(), max)) {
            string text = tostring(max);
            entry->userText(text, "user");
            // Try to detect the last entry
        }


        node.add(entry);
    }
    node.last();
}


/*!
 Class information are given to the parentput-stream.
*/

void CellShading::print(ostream& out) const {
    out << "CellShading";
}


void CellShading::colour(double val, Colour& colour) {
    colour = this->colours_[this->map_.find(val, 0)];
}


CellArray* CellShading::array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation,
                              int width, int height, float resolution, const string& technique) {
    if (adaptive_) {
        shading_ = "grid";
        return new GridArray(matrix, range, transformation, width, height, resolution, "middle");
    }
    else {
        shading_ = "cell";
        return new CellArray(matrix, range, transformation, width, height, resolution, technique);
    }
}
