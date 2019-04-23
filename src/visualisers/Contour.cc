/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Contour.cc
    \brief Implementation of the Template class Contour.

    Magics Team - ECMWF 2004

    Started: Wed 3-Mar-2004

    Changes:

*/

#include "Contour.h"
#include "ContourLibrary.h"
#include "HistoVisitor.h"
#include "Layer.h"
#include "Layout.h"
#include "MetaData.h"
#include "Text.h"
#include "Timer.h"

using namespace magics;


Contour::Contour() : matrix_(0), styleInfo_(0) {}


Contour::~Contour() {
    if (matrix_)
        delete (matrix_);
    if (styleInfo_)
        delete (styleInfo_);
}

/*!
 Class information are given to the output-stream.
*/

void Contour::print(ostream& out) const {
    out << "Contour[";
    ContourAttributes::print(out);
    out << "]";
}


class MatrixTreshold : public MatrixHandler {
public:
    MatrixTreshold(const AbstractMatrix& matrix, double min, double max) :
        MatrixHandler(matrix),
        min_(min),
        max_(max) {}
    double operator()(int row, int column) const {
        double val = this->matrix_(row, column);
        if (same(val, this->matrix_.missing()))
            return val;
        if (val < min_)
            return min_;
        if (val > max_)
            return max_;
        return val;
    }
    double min_;
    double max_;
};


void Contour::operator()(Data& data, BasicGraphicsObjectContainer& parent) {
    try {
        ParameterManager::set("contour_automatic_library_path", library_path_);
        ContourLibrary* library = MagTranslator<string, ContourLibrary>()(setting_);

        MetaDataCollector request, needAttributes;
        MagDef attributes;

        bool legend_only = contour_->legend_only_;
        if (predefined_.size()) {
            library->getStyle(predefined_, attributes);


            set(attributes);
            auto text = attributes.find("contour_legend_text");
            if (text != attributes.end())
                ParameterManager::set("contour_legend_text", text->second);
        }
        else {
            library->askId(request);
            data.visit(request);

            if (library->checkId(request, needAttributes)) {
                data.visit(needAttributes);
                needAttributes["theme"] = theme_;
                library->getStyle(needAttributes, attributes, *styleInfo_);
                if (!legend_)
                    attributes["legend"] = "off";
                set(attributes);
            }
            else {
                request["theme"] = theme_;
                styleInfo_       = new StyleEntry();

                library->getStyle(request, attributes, *styleInfo_);


                attributes["contour_legend_only"] = contour_->legend_only_;

                if (!legend_)
                    attributes["legend"] = "off";
                if (metadata_only_)
                    attributes["contour_legend_only"] = "on";
                set(attributes);
                /*
                for (auto s = attributes.begin(); s != attributes.end(); ++s)
                    cout << s->first << "-->" << s->second << endl;
                */
                auto text = attributes.find("contour_legend_text");
                if (text != attributes.end())
                    ParameterManager::set("contour_legend_text", text->second);
            }
        }

        contour_->legend_only_ = legend_only;

        delete library;


        data.getReady(parent.transformation());
        if (!data.valid()) {
            throw MagicsException("Invalid data for contouring");
        }
        MatrixHandler* box = data.matrix().getReady(parent.transformation());
        if (!box) {
            throw MagicsException("Invalid data for contouring");
        }
        if (!box->rows() || !box->columns()) {
            (*this->contour_)(data, parent);
            (*this->grid_)(data, parent);
            matrix_ = 0;
            delete box;
            return;
        }

        matrix_ = (*this->method_).handler(*box, parent);
        // matrix_ = box;

        if (this->floor_ != -INT_MAX || this->ceiling_ != INT_MAX)
            matrix_ = new MatrixTreshold(*matrix_, this->floor_, this->ceiling_);

        {
            Timer timer("setMinMax", "setMainMax");
            double min, max;
            {
                Timer timer("Max", "Max");
                min = matrix_->min();
            }
            {
                Timer timer("MIN", "MIN");
                max = matrix_->max();
            }
            (*this->contour_).adjust(min, max);
        }
        {
            Timer timer("CONTOUR", "CONTOUR");
            (*this->contour_)(*matrix_, parent);
        }
        (*this->contour_)(data, parent);
        if (magCompare(this->grid_->getType(), "akima"))
            (*this->grid_)(*matrix_, parent);
        else
            (*this->grid_)(data, parent);
        (*this->hilo_)(*matrix_, parent);


        // We do not need the box anymore!
        delete box;
    }
    catch (MagicsException& e) {
        throw e;  // forwarding exception
    }
}


void Contour::visit(Data& data, HistoVisitor& visitor) {
    if (!matrix_)
        return;
    contour_->visit(data, data.points(*visitor.dataLayoutTransformation(), false), visitor);
}


static SimpleObjectMaker<EcChartLibrary, ContourLibrary> ecchart("ecchart");
static SimpleObjectMaker<NoContourLibrary, ContourLibrary> off("off");
static SimpleObjectMaker<WebLibrary, ContourLibrary> style_name("style_name");
static SimpleObjectMaker<WebLibrary, ContourLibrary> ecmwf("ecmwf");


void Contour::visit(Data& data, LegendVisitor& legend) {
    if (!this->legend_)
        return;
    contour_->visit(data, legend);
}

void Contour::visit(MetaDataVisitor& visitor) {
    if (styleInfo_)
        visitor.add(styleInfo_);
    styleInfo_ = 0;
}
