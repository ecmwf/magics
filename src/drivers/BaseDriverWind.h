/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BaseDriverWind.h
    \brief Implementation of wind field methods of driver base class.

    Magics Team - ECMWF 2005

    Started: March 2005

    Changes:

*/


namespace magics {

class rotate {
public:
    rotate() : angle_(0) {}
    rotate(MFloat a, MFloat ratio = 1.) : angle_(a), ratio_(ratio) {}
    void operator()(PaperPoint& p) {
        MFloat x = p.x() * cos(angle_) + (p.y() / ratio_) * -sin(angle_);
        MFloat y = (p.x() * ratio_) * sin(angle_) + p.y() * cos(angle_);
        p.x(x);
        p.y(y);
    }

private:
    MFloat angle_;
    MFloat ratio_;
};

class translate {
public:
    translate() : point_() {}
    translate(PaperPoint point) : point_(point) {}
    void operator()(PaperPoint& p) {
        MFloat x = p.x() + point_.x();
        MFloat y = p.y() + point_.y();
        p.x(x);
        p.y(y);
    }

private:
    PaperPoint point_;
};

}  // namespace magics

using namespace magics;

/*!
  \brief Wind arrow render method for ALL drivers.

  This method should be used by all Magics++ drivers to render wind fields.
*/
MAGICS_NO_EXPORT void BaseDriver::renderWindArrow(const Arrow& arrow) const {
    const MFloat base    = 0.7;
    const MFloat sca     = (fabs(arrow.getScale()) < 0.001) ? 1. : arrow.getScale();
    const MFloat scaling = convertCM(1.) / (sca * ((coordRatioX_ + coordRatioX_) * .5));

    const unsigned int arrPoNo = arrow.size();
    if (arrPoNo < 1)
        return;

    const MFloat thickness  = (arrow.getThickness() == 1) ? 1 : 2. * arrow.getThickness();
    const LineStyle style   = arrow.getStyle();
    const ArrowPosition pos = arrow.getArrowPosition();

    setNewColour(arrow.getColour());
    Arrow::const_iterator arr = arrow.begin();
    int index                 = arrow.getHeadIndex();
    const MFloat ratio        = (coordRatioY_ == 0) ? 1. : coordRatioX_ / coordRatioY_;

    for (unsigned int pts = 0; pts < arrPoNo; pts++) {
        const double angle = setAngleY(arr->angle());
        double norm        = arr->norm() * scaling;

        // The next line was making very small wind artificially bigger!
        // if(fabs(norm) < 0.01) norm = scaling;

        const double norm2 = norm * base;
        double xx          = 0.;
        vector<PaperPoint> line;

        if (pos != M_HEAD_ONLY) {
            (pos == M_TAIL) ? line.push_back(PaperPoint(0., 0.)) : line.push_back(PaperPoint(-0.5 * norm, 0.));
            xx = (pos == M_TAIL) ? norm : 0.5 * norm;
            if ((index == 2) || (index == 1))
                xx = (pos == M_TAIL) ? norm2 : (base - 0.5) * norm;

            line.push_back(PaperPoint(xx, 0));
            for_each(line.begin(), line.end(), rotate(angle, ratio));
            for_each(line.begin(), line.end(), translate(arr->point_));
            xx = (pos == M_TAIL) ? norm : 0.5 * (norm);  // reset length

            // Arrow base
            const int old_currentColourIndex = currentLineStyle_;
            currentLineStyle_                = setLineParameters(style, thickness);
            renderPolyline2(line);
            currentLineStyle_ = old_currentColourIndex;
        }

        // Arrow head
        const MFloat bx = (1.0 - base) * norm;                                 // a third of the length of arrow
        const MFloat by = (1.0 - base) * ratio * norm * arrow.getHeadRatio();  // 10% of the length

        if (index > 3 || index < 0) {
            MagLog::warning() << "Wind: Wind arrow head index " << index << "out of range (0-3)! Use 2 as default"
                              << endl;
            index = 0;
        }

        line.clear();
        if (index == 0) {
            line.push_back(PaperPoint(xx - bx, -by));
            line.push_back(PaperPoint(xx, 0.));
            line.push_back(PaperPoint(xx - bx, by));
            for_each(line.begin(), line.end(), rotate(angle, ratio));
            for_each(line.begin(), line.end(), translate(arr->point_));
            renderPolyline(line);
        }
        else {
            line.push_back(PaperPoint(xx, 0.));
            line.push_back(PaperPoint(xx - bx, -by));
            line.push_back(PaperPoint(xx - bx, by));
            line.push_back(PaperPoint(xx, 0.));
            for_each(line.begin(), line.end(), rotate(angle, ratio));
            for_each(line.begin(), line.end(), translate(arr->point_));

            if (index == 1)
                renderSimplePolygon(line);
            else
                renderPolyline(line);
        }
        ++arr;
    }
}  // end BaseDriver::renderWindArrow()


MAGICS_NO_EXPORT void BaseDriver::renderWindFlag(const Flag& flag) const {
    const MFloat thickness    = (flag.getThickness() == 1) ? 1 : 2. * flag.getThickness();
    const LineStyle style     = flag.getStyle();
    const MFloat length       = convertCM(flag.getLength() / coordRatioX_);
    const string marker       = flag.getOriginMarker();
    const MFloat markerHeight = convertCM(flag.getOriginHeight() * .5 / coordRatioX_);
    setNewColour(flag.getColour());
    const unsigned int flaPoNo = flag.size();
    Flag::const_iterator fla   = flag.begin();

    Symbol origin;

    for (unsigned int pts = 0; pts < flaPoNo; pts++) {
        vector<PaperPoint> line;
        const MFloat angle = setAngleY(fla->angle());
        line.push_back(PaperPoint(-markerHeight, 0.));
        line.push_back(PaperPoint(-length, 0.));
        const MFloat ratio = (coordRatioY_ == 0) ? 1. : coordRatioX_ / coordRatioY_;
        for_each(line.begin(), line.end(), rotate(angle, ratio));
        for_each(line.begin(), line.end(), translate(fla->point_));
        MFloat len = fla->norm();

        MFloat lev1  = 1.25;  // 2.5
        MFloat lev2  = 3.75;  // 5.
        MFloat lev3  = 23.;   // 25.
        MFloat slev1 = 2.5;
        MFloat slev2 = 5.;
        MFloat slev3 = 25.;

        if (flag.getConvention() == KNOTS) {
            len *= 1.94384466;
            lev1  = 3.;
            lev2  = 7.;
            lev3  = 47.;
            slev1 = 5.;
            slev2 = 10.;
            slev3 = 50.;
        }

        if (markerHeight > 0.)
            origin.push_back(PaperPoint(fla->point_));

        const int old_currentColourIndex = currentLineStyle_;
        currentLineStyle_                = setLineParameters(style, thickness);
        renderPolyline2(line);
        //		currentLineStyle_ = old_currentColourIndex;

        MFloat barbFraction     = 0.;
        int i                   = 0;
        const MFloat lengthY    = setY(length * ratio);
        const MFloat barbHeight = setFlagY((flag.getHemisphere() == NORTH) ? (0.4 * lengthY) : (-0.4 * lengthY));
        bool fl                 = false;

        if (len < lev2)
            i++;
        while (len > (lev1)) {
            if (len > lev3) {
                barbFraction = 1.0;
                len -= slev3;
                fl = true;
            }
            else if (len >= lev2) {
                barbFraction = 1.0;
                len -= slev2;
                fl = false;
            }
            else {
                barbFraction = 0.6;
                len -= slev1;
                fl = false;
            }

            const MFloat dx   = sin(RAD(30.)) * barbFraction * 0.2 * length;  // angle of flags
            const MFloat step = i * (0.1 * length);

            if (!fl) {
                currentLineStyle_ = setLineParameters(style, thickness);
                line.clear();
                line.push_back(PaperPoint(-(length - step), 0.));
                line.push_back(PaperPoint(-(length - step + dx), barbHeight * barbFraction));

                for_each(line.begin(), line.end(), rotate(angle, ratio));
                for_each(line.begin(), line.end(), translate(fla->point_));
                renderPolyline2(line);
            }
            else {
                line.clear();
                line.push_back(PaperPoint(-(length - step), 0.));
                line.push_back(PaperPoint(-(length - step), barbHeight * barbFraction));
                line.push_back(PaperPoint(-(length - step - (0.1 * length)), 0.));
                for_each(line.begin(), line.end(), rotate(angle, ratio));
                for_each(line.begin(), line.end(), translate(fla->point_));
                renderSimplePolygon(line);
                fl = false;
                i++;
            }
            i++;
        }  // end while
        currentLineStyle_ = old_currentColourIndex;
        ++fla;
    }  // end for

    if (markerHeight > 0. && !magCompare(marker, "none")) {
        origin.setSymbol(marker);
        origin.setHeight(flag.getOriginHeight());
        origin.setColour(flag.getColour());
        renderSymbols(origin);
    }
}
