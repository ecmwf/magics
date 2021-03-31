/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "SkewtGrid.h"
#include "Layer.h"
#include "Layout.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "SciMethods.h"
#include "Text.h"
#include "Transformation.h"
using namespace magics;

SkewtGrid::SkewtGrid() {}


SkewtGrid::~SkewtGrid() {}

static void step(set<double>& values, set<double>& labels, double from, double to, double ref, double step, int freq) {
    if (ref > to)
        return;
    int l = 0;
    for (double v = ref; v < to; v += step) {
        values.insert(v);

        if (l % freq == 0) {
            labels.insert(v);
        }
        l++;
    }
    if (ref < from)
        return;
    l = 0;
    for (double v = ref; v >= from; v -= step) {
        values.insert(v);

        if (l % freq == 0) {
            labels.insert(v);
        }
        l++;
    }
}

void SkewtGrid::visit(DrawingVisitor& out) {
    const Transformation& tephi = out.transformation();
    // vector<double> tempe;
    double maxpcx = (tephi.getMaxPCX() + (tephi.getMinPCX() * .25)) / 1.25;
    // double minpcx = tephi.getMinPCX();
    // PaperPoint lr(maxpcx, tephi.getMinPCY());
    // UserPoint xy;
    // tephi.revert(lr, xy);

    double tmin;
    double tmax;
    double pmin;
    double pmax;

    tephi.boundingBox(tmin, pmin, tmax, pmax);

    double tfactor = 10;
    tmax           = int(tmax / tfactor) * tfactor + tfactor;
    tmin           = int(tmin / tfactor) * tfactor - tfactor;

    double pfactor = 100;
    pmax           = int(pmax / pfactor) * pfactor + pfactor;
    if (pmin < 100.) {
        pfactor = 10;
    }
    pmin = int(pmin / pfactor) * pfactor;

    double thmin = magics::theta(tmin + 273.15, pmax * 100.) - 237.15;
    thmin        = -100;
    double thmax = magics::theta(tmax + 273.15, pmin * 100.) - 237.15;
    thmax        = +450;

    // Isotherms are 45 degree lines
    if (isotherm_) {
        std::set<double> isotherms;
        std::set<double> labels;

        MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
        font.colour(*isotherm_label_colour_);

        step(isotherms, labels, tmin, tmax, isotherm_reference_, isotherm_interval_, isotherm_label_frequency_);

        for (std::set<double>::iterator t = isotherms.begin(); t != isotherms.end(); ++t) {
            Polyline poly;
            Colour colour = (*t != isotherm_reference_) ? *isotherm_colour_ : *isotherm_reference_colour_;
            poly.setColour(colour);
            poly.setThickness(isotherm_thickness_);
            poly.setLineStyle(isotherm_style_);

            PaperPoint xy1 = tephi(UserPoint(*t, pmin));
            PaperPoint xy2 = tephi(UserPoint(*t, pmax));
            // If no points are in the main area it is not clipped
            if (xy1.x() < maxpcx || xy2.x() < maxpcx) {
                poly.push_back(xy1);
                poly.push_back(xy2);
                tephi(poly, out.layout());
            }

            std::set<double>::iterator label = labels.find(*t);
            if (label == labels.end())
                continue;

            // labels at the bottom
            PaperPoint xy = tephi(UserPoint(*t, tephi.getMinY()));
            if (tephi.in(xy) && xy.x() <= maxpcx) {
                xy.y(0);  // bottom labels have their own box with y coordinates 0->100
                map<double, PaperPoint>::iterator labelIt = tBottomLabels_.find(*t);
                if (labelIt == tBottomLabels_.end()) {
                    tBottomLabels_.insert(make_pair(*t, xy));
                }
                else
                    tBottomLabels_[*t] = xy;
            }

            // labels at the top
            xy = tephi(UserPoint(*t, tephi.getMaxY()));
            if (tephi.in(xy) && xy.x() <= maxpcx) {
                xy.y(10);  // top labels have their own box with y coordinates 0->100
                map<double, PaperPoint>::iterator labelIt = tTopLabels_.find(*t);
                if (labelIt == tTopLabels_.end()) {
                    tTopLabels_.insert(make_pair(*t, xy));
                }
                else
                    tTopLabels_[*t] = xy;
            }
        }
    }

    // dry adiantic lines (theta)
    if (dry_adiabatic_) {
        std::set<double> dry;
        std::set<double> labels;

        MagFont font(dry_adiabatic_label_font_, dry_adiabatic_label_style_, dry_adiabatic_label_size_);
        font.colour(*dry_adiabatic_label_colour_);

        step(dry, labels, thmin, thmax, dry_adiabatic_reference_, dry_adiabatic_interval_,
             dry_adiabatic_label_frequency_);

        for (std::set<double>::iterator th = dry.begin(); th != dry.end(); ++th) {
            Polyline poly;
            poly.setLineStyle(dry_adiabatic_style_);
            poly.setColour(*dry_adiabatic_colour_);
            poly.setThickness(dry_adiabatic_thickness_);

            // double pl = -1;
            bool hasPointsIn = false;
            for (double p = pmin; p < pmax; p += 1) {
                double t = magics::temperatureFromTheta(*th + 273.15, p * 100) - 273.15;

                PaperPoint xy = tephi(UserPoint(t, p));
                if (xy.x() < maxpcx) {
                    hasPointsIn = true;
                }
                poly.push_back(xy);
            }

            // If no points are in the main area it is not clipped
            if (hasPointsIn)
                tephi(poly, out.layout());

            // std::set<double>::iterator label = labels.find(*th);
            // if( label == labels.end())
            //    continue;

            // Labels along the isotherm from the bottom left corner
            double tLabel = (tmax + tmin) / 2;
            UserPoint point(tLabel, magics::pressureFromTheta(*th + 273.15, tLabel + 273.15) / 100.);
            PaperPoint xy(tephi(point));
            if (tephi.in(xy)) {
                Text* text = new Text();
                text->setAngle(3.14 * 0.2);
                text->setVerticalAlign(MHALF);
                // text->addText("th=" + tostring(*th), font);
                text->addText(tostring(*th), font);
                text->setBlanking(true);
                text->push_back(xy);
                out.push_back(text);
            }
        }
    }


    // Isobars: horizontal lines
    if (isobar_) {
        std::set<double> isobars;
        std::set<double> labels;

        step(isobars, labels, pmin, pmax, isobar_reference_, isobar_interval_, isobar_label_frequency_);

        for (std::set<double>::iterator p = isobars.begin(); p != isobars.end(); ++p) {
            Polyline poly;
            poly.setColour(*isobar_colour_);
            poly.setLineStyle(isobar_style_);
            poly.setThickness(isobar_thickness_);

            PaperPoint xy = tephi(UserPoint(tmin, *p));
            double yy     = xy.y();
            xy            = PaperPoint(0, yy);
            poly.push_back(xy);
            xy = PaperPoint(100, yy);
            poly.push_back(xy);
            tephi(poly, out.layout());

            // Labels
            std::set<double>::iterator label = labels.find(*p);
            if (label != labels.end()) {
                // Left label
                xy.x(100);  // left labels have their own box with x coordinates 0->100

                map<double, PaperPoint>::iterator labelIt = pressureLeftLabels_.find(*p);
                if (labelIt == pressureLeftLabels_.end()) {
                    pressureLeftLabels_.insert(make_pair(*p, xy));
                }
                else
                    pressureLeftLabels_[*p] = xy;

                // Right label
                xy.x(0 + 10);  // right labels have their own box with x coordinates 0->100
                labelIt = pressureRightLabels_.find(*p);
                if (labelIt == pressureRightLabels_.end()) {
                    pressureRightLabels_.insert(make_pair(*p, xy));
                }
            }
        }

        // vertical dashed line where the info box starts in the right
        Polyline* axe = new Polyline();
        axe->setColour(Colour("black"));
        axe->setLineStyle(M_DASH);
        axe->setThickness(1);

        axe->push_back(PaperPoint(maxpcx, tephi.getMinPCY()));
        axe->push_back(PaperPoint(maxpcx, tephi.getMaxPCY()));
        out.push_back(axe);
    }

    // Humidity Mixing ratio Lines - up to 200 hPa
    if (mixing_ratio_) {
        vector<float> ratios;  // in g/kg
        ratios.push_back(0.1);
        ratios.push_back(0.2);
        ratios.push_back(0.5);
        ratios.push_back(0.7);
        ratios.push_back(1.);
        ratios.push_back(1.5);
        ratios.push_back(2.);
        ratios.push_back(3.);
        ratios.push_back(5.);
        ratios.push_back(7.);
        ratios.push_back(10.);
        ratios.push_back(15.);
        ratios.push_back(20.);
        ratios.push_back(30.);

        int grid     = 0;
        int labelCnt = 0;

        MagFont font(mixing_ratio_label_font_, mixing_ratio_label_style_, mixing_ratio_label_size_);
        font.colour(*mixing_ratio_label_colour_);

        for (vector<float>::iterator r = ratios.begin(); r != ratios.end(); ++r) {
            if (grid % mixing_ratio_frequency_)
                continue;

            grid++;
            Polyline poly;
            poly.setColour(*mixing_ratio_colour_);
            poly.setLineStyle(mixing_ratio_style_);
            poly.setThickness(mixing_ratio_thickness_);

            double pTop      = 200;
            bool hasPointsIn = false;
            for (double p = pTop; p < pmax; p += 10) {
                double t      = magics::temperatureFromMixingRatio(*r, p * 100);
                PaperPoint xy = tephi(UserPoint(t - 273.15, p));
                if (xy.x() < maxpcx)
                    hasPointsIn = true;

                poly.push_back(xy);
                if (labelCnt % mixing_ratio_label_frequency_)
                    continue;

                labelCnt++;
            }

            // If no points are in the main area it is not clipped
            if (hasPointsIn)
                tephi(poly, out.layout());

            // labels along the 950 hPa line
            double pLabel = 950;
            double t      = magics::temperatureFromMixingRatio(*r, pLabel * 100);
            PaperPoint xy = tephi(UserPoint(t - 273.15, pLabel));
            if (tephi.in(xy) && xy.x() <= maxpcx) {
                Text* text = new Text();
                text->addText(tostring(*r), font);
                text->setBlanking(true);
                text->setJustification(MCENTRE);
                text->setVerticalAlign(MHALF);
                text->push_back(xy);
                out.push_back(text);
            }
        }
    }

    if (saturated_adiabatic_) {
        MagFont font(saturated_adiabatic_label_font_, saturated_adiabatic_label_style_,
                     saturated_adiabatic_label_size_);
        font.colour(*saturated_adiabatic_label_colour_);

        std::set<double> sat;
        std::set<double> labels;

        step(sat, labels, thmin, thmax, saturated_adiabatic_reference_, saturated_adiabatic_interval_,
             saturated_adiabatic_label_frequency_);

        for (std::set<double>::iterator thetaw = sat.begin(); thetaw != sat.end(); ++thetaw) {
            if (*thetaw > 65.)
                continue;
            Polyline poly;
            poly.setColour(*saturated_adiabatic_colour_);
            poly.setLineStyle(saturated_adiabatic_style_);
            poly.setThickness(saturated_adiabatic_thickness_);
            double s = thetaEq(*thetaw + 273.15, *thetaw + 273.15, 1000 * 100);

            double pl        = -1;
            double pTop      = 200;
            bool hasPointsIn = false;
            for (double p = pTop; p < pmax; p += 1) {
                double t = magics::temperatureFromThetaEq(s, p * 100) - 273.15;

                PaperPoint xy = tephi(UserPoint(t, p));
                if (xy.x() < maxpcx) {
                    hasPointsIn = true;
                }
                poly.push_back(xy);

                /*if (t >= -40 )
                    poly.push_back(tephi(UserPoint(t, p)));
                else
                    pl = p;*/
            }

            // If no points are in the main area it is not clipped
            if (hasPointsIn)
                tephi(poly, out.layout());

            std::set<double>::iterator label = labels.find(*thetaw);
            if (label == labels.end())
                continue;

            // Labels are rendered along the 200hPa line
            double tTop   = magics::temperatureFromThetaEq(s, pTop * 100) - 273.15;
            PaperPoint xy = tephi(UserPoint(tTop, pTop));
            if (tephi.in(xy) && xy.x() <= maxpcx) {
                Text* text = new Text();
                text->addText(tostring(*thetaw), font);
                text->push_back(xy);
                out.push_back(text);
            }
        }
    }

    // wind area
    if (1) {
        MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
        font.colour(*isotherm_label_colour_);

        for (int i = 10; i < 25; i += 10) {
            // vertical dashed line and labels at the bottom
            Polyline poly;
            Colour colour = *isotherm_colour_;
            poly.setColour(colour);
            poly.setThickness(isotherm_thickness_);
            poly.setLineStyle(M_DASH);

            // poly.push_back(tephi(UserPoint(1000.+i, pmin)));
            // poly.push_back(tephi(UserPoint(1000.+i, pmax)));

            for (double p = pmin; p <= pmax; p += 10) {
                poly.push_back(tephi(UserPoint(1000. + i, p)));
            }

            tephi(poly, out.layout());

            UserPoint pt(1000 + i, tephi.getMaxPCY());
            PaperPoint xy = tephi(pt);
            xy.y(tephi.getMaxPCY() * .5);
            infoLabels_.insert(make_pair(i, xy));
        }

        std::set<double> isobars;
        std::set<double> labels;

        step(isobars, labels, pmin, pmax, isobar_reference_, isobar_interval_, isobar_label_frequency_);

        for (std::set<double>::iterator p = isobars.begin(); p != isobars.end(); ++p) {
            Polyline poly;
            poly.setColour(*isobar_colour_);
            poly.setLineStyle(M_DASH);
            poly.setThickness(isobar_thickness_);
            poly.push_back(tephi(UserPoint(1000, *p)));
            poly.push_back(tephi(UserPoint(1100, *p)));

            tephi(poly, out.layout());
        }
    }
}


/*!
 Class information are given to the output-stream.
*/
void SkewtGrid::print(ostream& out) const {
    out << "SkewtGrid[";
    TephiGridAttributes::print(out);
    out << "]";
}
void SkewtGrid::visit(LeftAxisVisitor& out) {
    MagFont font(isobar_label_font_, isobar_label_style_, isobar_label_size_);

    font.colour(*isobar_label_colour_);

    for (map<double, PaperPoint>::iterator label = pressureLeftLabels_.begin(); label != pressureLeftLabels_.end();
         ++label) {
        Text* text = new Text();
        text->setText(tostring(label->first));
        text->setFont(font);
        text->setBlanking(true);
        text->setJustification(MRIGHT);
        text->setVerticalAlign(MHALF);
        text->push_back(label->second);
        out.push_back(text);
    }
}
void SkewtGrid::visit(RightAxisVisitor& out) {
    MagFont font(isobar_label_font_, isobar_label_style_, isobar_label_size_);

    font.colour(*isobar_label_colour_);

    for (map<double, PaperPoint>::iterator label = pressureRightLabels_.begin(); label != pressureRightLabels_.end();
         ++label) {
        Text* text = new Text();
        text->setText(tostring(label->first));
        text->setFont(font);
        text->setBlanking(true);
        text->setJustification(MLEFT);
        text->setVerticalAlign(MHALF);
        text->push_back(label->second);
        out.push_back(text);
    }
}
void SkewtGrid::visit(BottomAxisVisitor& out) {
    MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
    font.colour(*isotherm_label_colour_);

    for (map<double, PaperPoint>::iterator label = tBottomLabels_.begin(); label != tBottomLabels_.end(); ++label) {
        Text* text = new Text();
        text->setText(tostring(label->first));
        text->setFont(font);
        text->setBlanking(true);
        text->push_back(label->second);
        out.push_back(text);
    }

    font = MagFont(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
    font.colour(*isotherm_label_colour_);

    for (map<double, PaperPoint>::iterator label = infoLabels_.begin(); label != infoLabels_.end(); ++label) {
        Text* text = new Text();
        text->setText(tostring(label->first));
        text->setFont(font);
        text->setBlanking(true);

        text->push_back(label->second);
        out.push_back(text);
    }
}

void SkewtGrid::visit(TopAxisVisitor& out) {
    MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
    font.colour(*isotherm_label_colour_);

    for (map<double, PaperPoint>::iterator label = tTopLabels_.begin(); label != tTopLabels_.end(); ++label) {
        Text* text = new Text();
        text->setText(tostring(label->first));
        text->setFont(font);
        text->setBlanking(true);
        text->push_back(label->second);
        out.push_back(text);
    }
}

void SkewtGrid::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors) {
    // First we create the layer!
    // and push It to the parent layer!
    StaticLayer* tephi = new NoDataLayer(this);
    // taylor->id(iconName_);
    // taylor>name(iconName_);
    layer.add(tephi);

    for (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
        tephi->set(*visitor);
        (*visitor)->visit(*this);
    }
}
