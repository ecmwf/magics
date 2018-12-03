/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Polyline.cc
    \brief Implementation of polyline graphics class (template).
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/
#include "Polyline.h"
#include "MagClipper.h"
#include "Transformation.h"

using namespace magics;

Polyline::Polyline()
{
}

Polyline::~Polyline()
{
}

void Polyline::reserve(double x)
{
    //polygon_.reserve(x);
}

bool Polyline::reproject(BasicGraphicsObjectContainer& out) const
{
    const Transformation& transformation = out.transformation();
    transformation(*this, out);
    return false;
}

Polyline* Polyline::getNew() const
{
    Polyline* poly = new Polyline();
    poly->copy(*this);
    return poly;
}

void Polyline::print(ostream& out) const
{
    out << "Polyline[";
    out << ", nb_points = " << this->size();
    if (this->size() < 20) {
        out << " Outer [";
        string sep = "";
        const unsigned int nb = size();
        for (unsigned int i = 0; i < nb; i++) {
            out << sep << get(i);
            sep = ", ";
        }
        out << "]";
    } else {
        unsigned int nb = size();
        out << " Outer[" << get(0) << ", " << get(1) << ", " << get(2);
        out << "...." << get(nb - 3) << ", " << get(nb - 2) << ", " << get(nb - 1);
        out << "(" << nb << " elements)]";
    }
    out << "]";
}



void Polyline::redisplay(const BaseDriver& driver) const
{
    if (polygon_.size() > 1)
        driver.redisplay(*this);
}

void Polyline::newHole()
{
    holes_.push_back(deque<PaperPoint>());
}

void Polyline::push_back_hole(const PaperPoint& point)
{
    holes_.back().push_back(point);
}

Polyline::Holes::const_iterator Polyline::beginHoles() const
{
    return holes_.begin();
}

Polyline::Holes::const_iterator Polyline::endHoles() const
{
    return holes_.end();
}

void Polyline::hole(Holes::const_iterator hole, vector<double>& x, vector<double>& y) const
{
    x.reserve(hole->size());
    y.reserve(hole->size());

    for (deque<PaperPoint>::const_iterator h = hole->begin(); h != hole->end(); ++h) {
        x.push_back(h->x_);
        y.push_back(h->y_);
    }
}

void Polyline::hole(Holes::const_iterator hole, Polyline& poly) const
{

    for (deque<PaperPoint>::const_iterator h = hole->begin(); h != hole->end(); ++h) {
        poly.push_back(*h);
    }
}

struct SouthCleaner {
    SouthCleaner() {}

    bool operator()(PaperPoint& point)
    {
        return point.y_ < -89;
    }
};
struct LonFinder : std::unary_function<PaperPoint, bool> {
    LonFinder() {}

    bool operator()(PaperPoint& point) const
    {
        return (same(point.x_, -180.));
    }
};

void Polyline::southClean(bool add)
{
    if ( !add ) {
        auto from = std::remove_if(polygon_.begin(), polygon_.end(), SouthCleaner());
        polygon_.erase(from, polygon_.end());
    }
    // rotate ..
    auto it = std::find_if(polygon_.begin(), polygon_.end(), LonFinder());
    if (it != polygon_.end()) {
        std::rotate(polygon_.begin(), it, polygon_.end());
        // if ( add ) {
        //     PaperPoint front = polygon_.front();
        //     PaperPoint back = polygon_.back();

        //     polygon_.push_front(PaperPoint(front.x_, -100.));
        //     polygon_.front().flagBorder();
        //     polygon_.push_front(PaperPoint(back.x_, -100.));
        //     polygon_.front().flagBorder();
        //     polygon_.push_back(PaperPoint(back.x_, -100.));
        //     polygon_.back().flagBorder();
        //     return;
        // }
    }
    // Not South pole .. we try to force closing
    close();

}

void Polyline::newHole(const Polyline& poly)
{
    holes_.push_back(deque<PaperPoint>());
    for (auto point = poly.begin(); point != poly.end(); ++point) {
        holes_.back().push_back(*point);
    }
}

struct ReprojectHelper {
    ReprojectHelper(const Transformation& transformation)
        : transformation_(transformation)
    {
    }
    const Transformation& transformation_;
    bool operator()(PaperPoint& point)
    {
        return !transformation_.fast_reproject(point.x_, point.y_);
    }
};



void Polyline::reproject(const Transformation& transformation)
{
	
    
    auto from = std::remove_if(polygon_.begin(), polygon_.end(), ReprojectHelper(transformation));
    polygon_.erase(from, polygon_.end());

    // Now the holes!
    for (Holes::iterator hole = holes_.begin(); hole != holes_.end(); ++hole) {
        for (auto h = hole->begin(); h != hole->end(); ++h) {
            transformation.fast_reproject(h->x_, h->y_);
        }
    }
}

Polyline* Polyline::clone() const
{

    Polyline* to = getNew();

    for (auto point = begin(); point != end(); ++point) {
        to->push_back(*point);
    }

    // Now the holes!
    for (Holes::const_iterator hole = holes_.begin(); hole != holes_.end(); ++hole) {
        to->newHole();
        for (auto h = hole->begin(); h != hole->end(); ++h) {
            to->push_back_hole(*h);
        }
    }

    return to;
}

void Polyline::intersect(const Polyline& poly, vector<Polyline*>& out) const
{
    // Use of a MagClipper
    MagClipper::clip(poly, *this, out);
}

void Polyline::clip(const Polyline& poly, vector<Polyline>& out) const
{
    // Use of a MagClipper
    assert(false);
}

void feed(const deque<PaperPoint>& points, const Polyline& box, vector<Polyline*>& out)
{
	Polyline* poly = new Polyline();
	for ( auto p = points.begin(); p != points.end(); ++p) {
		if ( !box.in(*p) || p->border() ) {
			if ( poly->size() ) {
				out.push_back(poly);
				poly = new Polyline();
			}
		}
		else 
			poly->push_back(*p);
	}
	if ( poly->size() ) {
		out.push_back(poly);
	}
	else 
		delete poly;
}

void Polyline::clip(const Polyline& poly, vector<Polyline*>& out) const
{
    feed(polygon_, poly, out);
    for (Holes::const_iterator hole = holes_.begin(); hole != holes_.end(); ++hole) {
    	feed(*hole, poly, out);
    }
}

// Is the poyline included in the "other" polyline"
bool Polyline::in(const Polyline& other)
{
    assert(false);
    return false;
    
}

// Is the pointincluded in the polyline"
bool Polyline::in(const PaperPoint& point) const
{
    return MagClipper::in(*this, point);
}

void Polyline::push_front(Polyline& other)
{
    other.polygon_.pop_back();
    polygon_.insert(polygon_.begin(),
        other.polygon_.begin(), other.polygon_.end());
}

void Polyline::push_back(Polyline& other)
{
    other.polygon_.pop_front();
    polygon_.insert(polygon_.end(),
        other.polygon_.begin(), other.polygon_.end());
}

/*! \brief Routine to check (and correct) integrity of the inner holes of polygons read from shape files.
*/
void Polyline::correct()
{
    
}
bool Polyline::sanityCheck()
{
    assert(false);
    
}

double PaperPoint::distance(const PaperPoint& other) const
{
    
    return sqrt( ((x_-other.x_) * (x_ - other.x_)) + ((y_-other.y_) * (y_ - other.y_)) );
   
}

void Polyline::box(const PaperPoint& ll, const PaperPoint& ur)
{
    push_back(ll);
    push_back(ll.x(), ur.y());
    push_back(ur);
    push_back(ur.x(), ll.y());
    push_back(ll);
   
}
bool Polyline::concatenate(Polyline&)
{
    assert(false);
}
void Polyline::intersection(Polyline&)
{
    assert(false);
}
bool Polyline::within(const PaperPoint& point) const
{
    return MagClipper::in(*this, point);
}