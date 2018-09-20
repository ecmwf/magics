/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Tephigram.cc
    \brief Implementation of Tephigram.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 12 16:01:47 2008

*/

#include "MagClipper.h"
#include "MagJSon.h"
#include "SciMethods.h"
#include <Polyline.h>
#include <Tephigram.h>

using namespace magics;

/*!
  \brief Constructor
  
  \todo what does still need implmenting? can debug message be removed?
*/
Tephigram::Tephigram()
{
}

/*!
  \brief Destructor
*/
Tephigram::~Tephigram()
{
}

void Tephigram::print(ostream& out) const
{
    out << "Tephigram[";
    out << "]";
}

#define sinus -0.7071
#define cosinus 0.7071
static double maxpcx;
void Tephigram::init()
{
    if (x_min_ == 0 && x_max_ == 100) {
        x_min_ = -90;
        x_max_ = 50;
    }

    if (y_min_ == 0 && y_max_ == 100) {
        y_min_ = 1060.;
        y_max_ = 200;
    }

    vector<double> x, y;

    double tmin = (x_min_ + x_max_) / 2.;
    double pmin = std::max(y_min_, y_max_);
    double pmax = std::min(y_min_, y_max_);
    double thmin = magics::theta(tmin + 273.15, pmin * 100.) - 273.15;
    double thmax = magics::theta(tmin + 273.15, pmax * 100.) - 273.15;
    double tmax = temperatureFromTheta(thmax + 273.15, pmax * 100.) - 273.15;

    if (x_min_ < -300 || tmin < -300) {
        throw MagicsException("Tephigram: invalid minimum temperature");
    }

    if (x_max_ > 400 || tmax > 400) {
        throw MagicsException("Tephigram: invalid maximum temperature");
    }

    if (y_min_ > 1500 || pmin > 1500) {
        throw MagicsException("Tephigram: invalid bottom pressure");
    }

    if (y_max_ <= 0.0001 || pmax <= 0.0001) {
        throw MagicsException("Tephigram: invalid top pressure");
    }

    if (x_min_ >= x_max_) {
        throw MagicsException("Tephigram: minimum temperature cannot be greater than maximum temperature");
    }

    if (y_min_ <= y_max_) {
        throw MagicsException("Tephigram: top pressure cannot be greater than bottom pressure");
    }

    minPCX_ = ((tmin * cosinus) - (thmin * sinus));
    minPCY_ = (tmin * sinus) + (thmin * cosinus);
    maxPCX_ = ((tmax * cosinus) - (thmax * sinus));
    maxPCY_ = (tmax * sinus) + (thmax * cosinus);

    // now we add 25% to the right for the info panel!
    maxpcx = maxPCX_;

    MagLog::dev() << "useful in tephi " << maxpcx << endl;
    maxPCX_ += (maxPCX_ - minPCX_) * (annotation_width_ / 100.);
    MagLog::dev() << "set in tephi in grid " << maxpcx << endl;
}

const double KAPPA = 0.285611;
PaperPoint Tephigram::operator()(const UserPoint& xy) const
{

    double p = (same(xy.y(), 0)) ? 1. : xy.y();
    if (xy.x() >= 1000) { // x = x
        // y = p
        double coefficient = pow(100000. / (p * 100), KAPPA);
        double y = (maxpcx + (273.15 * (cosinus - sinus))) * ((sinus + (coefficient * cosinus))) / (cosinus - (coefficient * sinus)) - 2713.15 * (sinus + cosinus);
        MagLog::dev() << p << "-->" << y << "??? " << minPCY_ << "<<" << maxPCY_ << endl;
        double x = ((maxPCX_ - maxpcx) / 20) * (xy.x() - 1000) + maxpcx;

        return PaperPoint(x, y, xy.value());
    }
    // UserPoint X = temperature in deg Y = Pressure in hPa
    // First we calculate theta and we rotate!
    double tempe = xy.x();

    double theta = magics::theta(tempe + 273.15, p * 100.) - 273.15;
    double x = ((tempe * cosinus) - (theta * sinus));
    double y = (tempe * sinus) + (theta * cosinus);

    return PaperPoint(x, y, xy.value());
}

PaperPoint Tephigram::operator()(const PaperPoint& pt) const
{

    // UserPoint X = temperature in deg Y = Pressure in hPa
    // First we calculate theta and we rotate!
    double tempe = pt.x() * cosinus + pt.y() * sinus;
    double theta = -(pt.x() * sinus) + pt.y() * cosinus;

    double p = magics::pressureFromTheta(theta + 273.15, tempe + 273.15) / 100;

    return PaperPoint(tempe, p);
}

void Tephigram::revert(const PaperPoint& pt, UserPoint& point) const
{
    // UserPoint X = temperature in deg Y = Pressure in hPa
    // First we calculate theta and we rotate!
    double tempe = pt.x() * cosinus + pt.y() * sinus;
    double theta = -(pt.x() * sinus) + pt.y() * cosinus;

    double p = magics::pressureFromTheta(theta + 273.15, tempe + 273.15) / 100;

    point.x_ = tempe;
    point.y_ = p;
}

bool Tephigram::needShiftedCoastlines() const
{
    return false;
}
void Tephigram::setMinMaxX(double min, double max)
{
    if (min > 1000 || max > 1000)
        return;
    setMinX(min);
    setMaxX(max);
    init();
}

void Tephigram::setMinMaxY(double min, double max)
{
    // Careful, Tephigram are in pressure levels...
    if (min < 50.) {
        MagLog::warning() << " Top Pressure reset to 50." << endl;
        min = 50.;
    }

    setMinY(max);
    setMaxY(min);
    init();
}
void Tephigram::aspectRatio(double& width, double& height)
{

    Transformation::aspectRatio(width, height);
}

void Tephigram::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax) const
{

    vector<std::pair<double, double> > geo;
    vector<std::pair<double, double> > xy;

    double xpcmax = maxpcx;
    double xpcmin = minPCX_;
    double ypcmax = maxPCY_;
    double ypcmin = minPCY_;
    xmin = DBL_MAX;
    xmax = DBL_MIN;
    ymin = DBL_MAX;
    ymax = DBL_MIN;

    const double xs = (xpcmax - xpcmin) / 99.;
    const double ys = (ypcmax - ypcmin) / 99.;
    // Walk along the boundary...
    double x, y;
    for (int i = 0; i < 100; i++) {
        x = xpcmin + (i * xs);
        for (int i = 0; i < 100; i++) {
            y = ypcmin + (i * ys);
            double tempe = x * cosinus + y * sinus;
            double theta = -(x * sinus) + y * cosinus;
            double p = magics::pressureFromTheta(theta + 273.15, tempe + 273.15) / 100;

            if (xmin > tempe)
                xmin = tempe;
            if (xmax < tempe)
                xmax = tempe;
            if (ymin > p)
                ymin = p;
            if (ymax < p)
                ymax = p;
        }
    }
}

double Tephigram::getMinX() const
{

    return -1.5;
}

double Tephigram::getMinY() const
{

    return y_min_;
}

double Tephigram::getMaxX() const
{
    return 1.5;
    ;
}

double Tephigram::getMaxY() const
{

    return y_max_;
}

void Tephigram::setMinX(double x)
{
    if (x < x_min_)
        x_min_ = x;
}

void Tephigram::setMinY(double y)
{
    if (y > y_min_)
        y_min_ = y;
}

void Tephigram::setMaxX(double x)
{
    if (x > x_max_)
        x_max_ = x;
}

void Tephigram::setMaxY(double y)
{
    if (y < y_max_)
        y_max_ = y;
}

double Tephigram::getMinPCX() const
{

    return minPCX_;
}

double Tephigram::getMinPCY() const
{

    return minPCY_;
}

double Tephigram::getMaxPCX() const
{

    return maxPCX_;
}

double Tephigram::getMaxPCY() const
{
    return maxPCY_;
}
double Tephigram::getMaxTestPCX() const
{
    return maxpcx;
}
Polyline& Tephigram::getPCBoundingBox() const
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

Polyline& Tephigram::getUserBoundingBox() const
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

void Tephigram::setDefinition(const string& json)
{

    if (json.empty())
        return;

    MagJSon helper;
    helper.interpret(json);

    XmlNode node = **helper.tree_.begin();

    node.name("Tephigram");

    set(node);
}
void toxml2(string& out, const map<string, string>& def)
{
    ostringstream os;
    string sep = "";
    for (map<string, string>::const_iterator entry = def.begin(); entry != def.end(); ++entry) {
        os << sep << "\"" << entry->first << "\" : \"" << entry->second << "\"";
        sep = ",\n";
    }

    out = os.str();
}
void Tephigram::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
    map<string, string> def;
    def["subpage_map_projection"] = "Tephigram";

    def["x_min"] = tostring(ll.x_);
    def["x_max"] = tostring(ur.x_);
    def["y_min"] = tostring(ll.y_);
    def["y_max"] = tostring(ur.y_);

    ::toxml2(out, def);

    out = "{" + out + "}";

    MagJSon helper;

    helper.interpret(out);
}



void Tephigram::revert(const vector<std::pair<double, double> >& in, vector<std::pair<double, double> >& out) const
{
    out.reserve(in.size());
    for (vector<std::pair<double, double> >::const_iterator p = in.begin(); p != in.end(); ++p) {
        double tempe = p->first * cosinus + p->second * sinus;
        double theta = -(p->first * sinus) + p->second * cosinus;

        double pt = magics::pressureFromTheta(theta + 273.15, tempe + 273.15) / 100;
        MagLog::dev() << " tempe = " << tempe << "  Pressure = " << pt << endl;
        out.push_back(make_pair(tempe, pt));
    }
}

void Tephigram::operator()(const Polyline& from, BasicGraphicsObjectContainer& out) const
{
    if (from.empty())
        return;

    PaperPoint ll, ur;
    bool grid = false;
    for (unsigned i = 0; i < from.size(); i++) {
        if (from.get(i).x() < maxpcx) {
            grid = true;
            break;
        }
    }
    if (grid) {
        ll = PaperPoint(getMinPCX(), getMinPCY());
        ur = PaperPoint(maxpcx, getMaxPCY());
    } else {
        ll = PaperPoint(maxpcx, getMinPCY());
        ur = PaperPoint(maxPCX_, getMaxPCY());
    }

    vector<Polyline*> lines;

    MagClipper::clip(from, ll, ur, lines);

    for (auto line = lines.begin(); line != lines.end(); ++line)
    	out.push_back(*line);

    /*
		//boost::geometry::model::box<PaperPoint> box(ll, ur);
		//boost::geometry::correct(box);
		if ( from.closed() ) {
			deque<PaperPoint> line;

			for (unsigned i = 0; i < from.size(); i++) {
				line.push_back(from.get(i));
				


			}

			//boost::geometry::correct(line);
			vector<deque<PaperPoint> > result;
			//boost::geometry::intersection(box, line, result);

			// Now we feed the graphic container!

			for (vector<deque<PaperPoint> >::iterator l = result.begin(); l != result.end(); l++)
			{
				Polyline* poly = from.getNew();

				for (deque<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
					poly->push_back(*point);

				if ( !poly->empty() )
					out.push_back(poly);
			}
		}
		else {
			vector<PaperPoint> line;

			for (unsigned i = 0; i < from.size(); i++) {
				line.push_back(from.get(i));
				
				
			}
			//boost::geometry::correct(line);
			vector<vector<PaperPoint> > result;
			//boost::geometry::intersection(box, line, result);

			// Now we feed the graphic container!

			for (vector<vector<PaperPoint> >::iterator l = result.begin(); l != result.end(); l++)
			{
				Polyline* poly = from.getNew();

				for (vector<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
					poly->push_back(*point);

				if ( !poly->empty() )
					out.push_back(poly);
			}
		}
*/
}

bool Tephigram::in(const PaperPoint& point) const
{
    return MagClipper::in(getPCBoundingBox(), point);
}
