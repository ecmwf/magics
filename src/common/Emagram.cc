/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagJSon.h"
#include "SciMethods.h"
#include <Emagram.h>
#include <Polyline.h>
#include <MagClipper.h>

using namespace magics;

//=====================================================
//
// Emagram
//
//=====================================================

Emagram::Emagram()
{
}

Emagram::~Emagram()
{
}

void Emagram::print(ostream& out) const
{
    out << "Emagram[";
    out << "]";
}

#define sinus -0.7071
#define cosinus 0.7071

//the right edge of the real skew-t area. To the right
//is the info area to plot the wind profile.
static double maxpcx;

void Emagram::init()
{
    //When default input visualiser values are specified: we assing a meaningful default
    if (x_min_ == 0 && x_max_ == 100) //temperature (K)
    {
        x_min_ = -90;
        x_max_ = 50;
    }

    if (y_min_ == 0 && y_max_ == 100) //pressure (hPa)
    {
        y_min_ = 1060.;
        y_max_ = 100;
    }

    if (x_min_ < -300) {
        throw MagicsException("Tephigram: invalid minimum temperature");
    }

    if (x_max_ > 400) {
        throw MagicsException("Tephigram: invalid maximum temperature");
    }

    if (y_min_ > 1500) {
        throw MagicsException("Tephigram: invalid bottom pressure");
    }

    if (y_max_ <= 0.0001) {
        throw MagicsException("Tephigram: invalid top pressure");
    }

    if (x_min_ >= x_max_) {
        throw MagicsException("Tephigram: minimum temperature cannot be greater than maximum temperature");
    }

    if (y_min_ <= y_max_) {
        throw MagicsException("Tephigram: top pressure cannot be greater than bottom pressure");
    }

    //To have a proper ascpect ratio we always work with a 100x100 rectangle
    minPCX_ = 0;
    maxPCX_ = 100;

    minPCY_ = 0;
    maxPCY_ = 100;

    // now we add 25% to the right for the info area
    maxpcx = maxPCX_;

    MagLog::dev() << "useful area width in emagram: " << maxpcx << endl;
    maxPCX_ += (maxPCX_ - minPCX_) * (annotation_width_ / 100.);
    MagLog::dev() << "witdth set for emagram in grid " << maxpcx << endl;
}

const double KAPPA = 0.285611;

//Converts (t,p) coordinates to paper coordinates
PaperPoint Emagram::operator()(const UserPoint& xy) const
{
    //We are in the wind box to the right
    if (xy.x() >= 1000) {
        double x = (maxPCX_ - maxpcx) * (xy.x() - 1000.) + maxpcx;
        double y = (maxPCY_ - minPCY_) * (log(y_min_) - log(xy.y())) / (log(y_min_) - log(y_max_));
        return PaperPoint(x, y, xy.value());
    }

    //Main emagram area
    double x = (maxpcx - minPCX_) * (xy.x() - x_min_) / (x_max_ - x_min_);
    double y = (maxPCY_ - minPCY_) * (log(y_min_) - log(xy.y())) / (log(y_min_) - log(y_max_));
    return PaperPoint(x, y, xy.value());
}

PaperPoint Emagram::operator()(const PaperPoint& pt) const
{
#if 0
    // UserPoint X = temperature in deg Y = Pressure in hPa
    // First we calculate theta and we rotate!
    double tempe = pt.x() *cosinus + pt.y()*sinus;
    double theta = - (pt.x() * sinus) + pt.y()*cosinus;

    double p = magics::pressureFromTheta(theta+273.15, tempe+273.15)/100;

    return PaperPoint(tempe, p);
#endif
    return PaperPoint(pt);
}

//Converts paper coordinates to (t,p) coordinates
void Emagram::revert(const PaperPoint& pt, UserPoint& point) const
{
    double t = x_min_ + (x_max_ - x_min_) * (pt.x() - minPCX_) / (maxpcx - minPCX_);
    double p = exp(log(y_min_) - pt.y() * (log(y_min_) - log(y_max_)) / (maxPCY_ - minPCY_));

    point.x_ = t;
    point.y_ = p;
}

void Emagram::revert(const vector<std::pair<double, double> >& in, vector<std::pair<double, double> >& out) const
{
    out.reserve(in.size());
    for (vector<std::pair<double, double> >::const_iterator it = in.begin(); it != in.end(); ++it) {
        double t = x_min_ + (x_max_ - x_min_) * (it->first - minPCX_) / (maxpcx - minPCX_);
        double p = exp(log(y_min_) - it->second * (log(y_min_) - log(y_max_)) / (maxPCY_ - minPCY_));
        out.push_back(make_pair(t, p));
    }
}

bool Emagram::needShiftedCoastlines() const
{
    return false;
}

void Emagram::setMinMaxX(double min, double max)
{
    if (min > 1000 || max > 1000)
        return;
    setMinX(min);
    setMaxX(max);
    init();
}

void Emagram::setMinMaxY(double min, double max)
{
    // Careful, Emagram are in pressure levels...
    if (min < 50.) {
        MagLog::warning() << " Top Pressure reset to 50." << endl;
        min = 50.;
    }

    setMinY(max);
    setMaxY(min);
    init();
}

void Emagram::aspectRatio(double& width, double& height)
{
    Transformation::aspectRatio(width, height);
}

void Emagram::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax) const
{
    vector<std::pair<double, double> > geo;
    vector<std::pair<double, double> > xy;

    double xpcmax = maxpcx;
    double xpcmin = minPCX_;
    double ypcmax = maxPCY_;
    double ypcmin = minPCY_;
    xmin = x_min_;
    xmax = x_max_;
    ymin = y_max_;
    ymax = y_min_;

#if 0
    return;

    const double xs = (xpcmax- xpcmin)/99.;
    const double ys = (ypcmax- ypcmin)/99.;
    // Walk along the boundary...
    double x,y;
    for (int i = 0; i < 100; i++) {
        x = xpcmin +(i*xs);
        for (int i = 0; i < 100; i++) {
            y = ypcmin +(i*ys);
            double tempe =x *cosinus + y*sinus;
            double theta = - (x * sinus) + y*cosinus;
            double p = magics::pressureFromTheta(theta+273.15, tempe+273.15)/100;

            if ( xmin > tempe) xmin = tempe;
            if ( xmax < tempe) xmax = tempe;
            if ( ymin > p) ymin = p;
            if ( ymax < p) ymax = p;
        }
    }
#endif
}

double Emagram::getMinX() const
{
    return -1.5;
}

double Emagram::getMinY() const
{
    return y_min_;
}

double Emagram::getMaxX() const
{
    return 1.5;
    ;
}

double Emagram::getMaxY() const
{
    return y_max_;
}

void Emagram::setMinX(double x)
{
    if (x < x_min_)
        x_min_ = x;
}

void Emagram::setMinY(double y)
{
    if (y > y_min_)
        y_min_ = y;
}

void Emagram::setMaxX(double x)
{
    if (x > x_max_)
        x_max_ = x;
}

void Emagram::setMaxY(double y)
{
    if (y < y_max_)
        y_max_ = y;
}

double Emagram::getMinPCX() const
{

    return minPCX_;
}

double Emagram::getMinPCY() const
{

    return minPCY_;
}

double Emagram::getMaxPCX() const
{

    return maxPCX_;
}

double Emagram::getMaxPCY() const
{
    return maxPCY_;
}

double Emagram::getMaxTestPCX() const
{
    return maxpcx;
}

Polyline& Emagram::getPCBoundingBox() const
{
    if (PCEnveloppe_->empty()) {
        PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
        PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMaxPCY()));
        PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMaxPCY()));
        PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMinPCY()));
        PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
    }

    return *PCEnveloppe_;
}

Polyline& Emagram::getUserBoundingBox() const
{
    if (userEnveloppe_->empty()) {
        userEnveloppe_->push_back(PaperPoint(x_min_, y_min_));
        userEnveloppe_->push_back(PaperPoint(x_min_, y_max_));
        userEnveloppe_->push_back(PaperPoint(x_max_, y_max_));
        userEnveloppe_->push_back(PaperPoint(x_max_, y_min_));
        userEnveloppe_->push_back(PaperPoint(x_min_, y_min_));
    }

    return *userEnveloppe_;
}

void Emagram::setDefinition(const string& json)
{
    if (json.empty())
        return;

    MagJSon helper;
    helper.interpret(json);

    XmlNode node = **helper.tree_.begin();

    node.name("Emagram");
    set(node);
}

static void toxml2(string& out, const map<string, string>& def)
{
    ostringstream os;
    string sep = "";
    for (map<string, string>::const_iterator entry = def.begin(); entry != def.end(); ++entry) {
        os << sep << "\"" << entry->first << "\" : \"" << entry->second << "\"";
        sep = ",\n";
    }

    out = os.str();
}

void Emagram::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
    map<string, string> def;
    def["subpage_map_projection"] = "Emagram";

    def["x_min"] = tostring(ll.x_);
    def["x_max"] = tostring(ur.x_);
    def["y_min"] = tostring(ll.y_);
    def["y_max"] = tostring(ur.y_);

    ::toxml2(out, def);

    out = "{" + out + "}";

    MagJSon helper;

    helper.interpret(out);
}

void Emagram::operator()(const Polyline& from, BasicGraphicsObjectContainer& out) const
{
    if (from.empty())
        return;
    PaperPoint ll(getMinPCX(), getMinPCY());
    PaperPoint ur(maxpcx, getMaxPCY());
    vector<Polyline*> lines;

    MagClipper::clip(from, ll, ur, lines);

    for (auto line = lines.begin(); line != lines.end(); ++line)
        out.push_back(*line);

   
}

bool Emagram::in(const PaperPoint& point) const
{

    return MagClipper::in(getPCBoundingBox(), point);
}
