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
	//polygon_.outer().reserve(x);
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
	  if ( this->size() < 20 ) {
            out << " Outer [";
            string sep = "";
	    const unsigned int nb = size();
            for (unsigned int i = 0; i < nb; i++) {
                out << sep << get(i);
                sep = ", ";
            }
            out << "]";
        }
        else {
            unsigned int nb = size();
            out << " Outer[" << get(0) << ", " << get(1) << ", " << get(2);
            out << "...." << get(nb-3) << ", " << get(nb-2)  << ", " << get(nb-1);
            out << "(" << nb << " elements)]";
        }
	out << "]";
}



Polyline* Polyline::simplify(double factor)
{
	BoostPoly simplified;
	boost::geometry::simplify(polygon_, simplified, factor);
	polygon_ = simplified;
	return this;
}




void Polyline::redisplay(const BaseDriver& driver) const 
{
    if ( polygon_.outer().size() > 1 ) driver.redisplay(*this);
}

void Polyline::newHole()
{
	polygon_.inners().push_back(BoostPoly::ring_type());
}

void Polyline::push_back_hole(const PaperPoint& point)
{
	polygon_.inners().back().push_back(point);
}


Polyline::Holes::const_iterator Polyline::beginHoles() const
{
	return polygon_.inners().begin();
}

Polyline::Holes::const_iterator Polyline::endHoles() const
{
	return polygon_.inners().end();
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

struct SouthCleaner
{
	SouthCleaner() {}

	bool operator()(PaperPoint& point) {
		return point.y_ < -89;
	}
};
struct LonFinder : std::unary_function<PaperPoint,bool>
{
	LonFinder() {}

	bool operator()(PaperPoint& point)  const {
		return ( same(point.x_, -180.)  );
	}
};

void Polyline::southClean()
{
	
	MagLine::iterator from = std::remove_if (polygon_.outer().begin(), polygon_.outer().end(), SouthCleaner());
	polygon_.outer().erase(from, polygon_.outer().end());	
	// rotate ..
	MagLine::iterator it = std::find_if (polygon_.outer().begin(), polygon_.outer().end(), LonFinder() );
  	if ( it != polygon_.outer().end()) {
    	std::rotate(polygon_.outer().begin(), it, polygon_.outer().end());
  	}

} 

void Polyline::newHole(const Polyline& poly)
{
	polygon_.inners().push_back(BoostPoly::ring_type());
	for (MagLine::const_iterator point = poly.begin(); point != poly.end(); ++point) {
		polygon_.inners().back().push_back(*point);
	}
}


struct ReprojectHelper
{
	ReprojectHelper(const Transformation& transformation) : transformation_(transformation) {}
	const Transformation& transformation_;
	bool operator()(PaperPoint& point) {
		return !transformation_.fast_reproject(point.x_, point.y_);
	}
};

void Polyline::reproject(const Transformation& transformation)
{
	
	MagLine::iterator from = std::remove_if (polygon_.outer().begin(), polygon_.outer().end(), ReprojectHelper(transformation));
	polygon_.outer().erase(from, polygon_.outer().end());


// Now the holes!
	for (Holes::iterator hole = polygon_.inners().begin(); hole != polygon_.inners().end(); ++hole)  {
		for (MagLine::iterator h = hole->begin(); h != hole->end(); ++h) {
			transformation.fast_reproject(h->x_, h->y_);
		}
	}
}

Polyline*  Polyline::clone() const
{

	Polyline* to = getNew();

	for (MagLine::const_iterator point = begin(); point != end(); ++point) {
		to->push_back(*point);
	}

// Now the holes!
	for (Holes::const_iterator hole = polygon_.inners().begin(); hole != polygon_.inners().end(); ++hole)  {
		to->newHole();
		for (MagLine::const_iterator h = hole->begin(); h != hole->end(); ++h) {
			to->push_back_hole(*h);
		}

	}

	return to;
}

void Polyline::intersect(const Polyline& poly, vector<Polyline> & out) const
{

	vector<BoostPoly> clip;
	try {
		boost::geometry::intersection(this->polygon_, poly.polygon_, clip);

		for (vector<BoostPoly>::iterator c = clip.begin(); c != clip.end(); ++c) {
			out.push_back(Polyline());
			out.back().copy(*this);
			out.back().polygon_ = *c;
		}
	}
	catch(...) {
		MagLog::debug() << " pb intersect! " << endl;
		MagLog::debug() << *this << endl;
		MagLog::debug() << " with-> " << endl;
		MagLog::debug() << poly << endl;
		out.push_back(poly);
	}
}

void Polyline::clip(const Polyline& poly, vector<Polyline>& out) const
{
//  convert the outer in a stringline! and then clip...
	boost::geometry::model::linestring<PaperPoint> line;

	for (deque<PaperPoint>::const_iterator p = poly.polygon_.outer().begin();
			p != poly.polygon_.outer().end(); ++p) {
		if ( p->x_ == begin()->x_ )
			line.push_back(PaperPoint(p->x_+1, p->y_));
		else
			line.push_back(*p);

	}

	vector<boost::geometry::model::linestring<PaperPoint> > clip;
	boost::geometry::intersection(line, polygon_,  clip);
	vector<boost::geometry::model::linestring<PaperPoint> >::iterator c;
	for (c = clip.begin(); c != clip.end(); ++c) {
		out.push_back(Polyline());
		out.back().copy(poly);
		for (boost::geometry::model::linestring<PaperPoint>::iterator p = c->begin(); p != c->end(); ++p) {
			out.back().polygon_.outer().push_back(*p);
		}

	}
}

// Is the poyline included in the "other" polyline"
bool Polyline::in(const Polyline& other)
{
	return !boost::geometry::disjoint(this->polygon_, other.polygon_);
}

// Is the pointincluded in the polyline"
bool Polyline::in(const PaperPoint& point)
{
	return boost::geometry::covered_by(point, this->polygon_);
}

void Polyline::push_front(Polyline& other)
{
	other.polygon_.outer().pop_back();
	polygon_.outer().insert(polygon_.outer().begin(),
			other.polygon_.outer().begin(), other.polygon_.outer().end());
}

void Polyline::push_back(Polyline& other)
{
	other.polygon_.outer().pop_front();
		polygon_.outer().insert(polygon_.outer().end(),
				other.polygon_.outer().begin(), other.polygon_.outer().end());
}

/*! \brief Routine to check (and correct) integrity of the inner holes of polygons read from shape files.
*/
void Polyline::correct()
{
	boost::geometry::correct(polygon_);
}
bool Polyline::sanityCheck()
{
    // input polygon _should_ have an outer ring larger than all its inner
    // (hole) rings. This routine enforces this rule and ensures all the
    // orientations are correct (i.e. outer ring clockwise, inners all
    // anti-clockwise).
    //
    // output flag true if input polygon modified.
	boost::geometry::correct(polygon_);
    bool io_rbModified = false;
    if (polygon_.outer().empty())
        return io_rbModified;

    // we construct a "largest" polygon by iterating through the input.
    // the largest ring we find will be assigned to its outer,
    // the remaining rings will all be assigned to its inner holes.

    BoostPoly largest = polygon_;
    largest.inners().resize(0);

    for (int i = 0; i < polygon_.inners().size(); i++)
    {
        // Note that anticlockwise "inner" polygons that are mistakenly in the "outer"
        // position will return negativve areas. By taking the highest area we
        // guard against inner polygons as outer rings and guard against any
        // erroneous clockwise inner polygons that we might encounter.
        if (boost::geometry::area(polygon_.inners()[i]) > boost::geometry::area(largest.outer()))
        {
            // we have an inner ring larger than the current outer ring.

            // move the outer ring to the set of inner rings
            largest.inners().push_back(largest.outer());
            // assign the larger ring as a new outer ring
            boost::geometry::assign(largest, polygon_.inners()[i]);

            io_rbModified = true;
        }
        else
        {
            // simple copy of the inner ring
            largest.inners().push_back(polygon_.inners()[i]);
        }

    }

    // if any swapping has taken place, correct and assign the output polygon
    if (io_rbModified)
    {
        // set the orientations using the boost method
        boost::geometry::correct(largest);
        polygon_ = largest;
    }

    return io_rbModified;

}


double PaperPoint::distance(const PaperPoint& other) const
{
	return boost::geometry::distance(*this, other);
}
void Polyline::box(const PaperPoint& ll, const PaperPoint& ur)
{
	push_back(ll);
	push_back(ll.x(), ur.y());
	push_back(ur);
	push_back(ur.x(), ll.y());
	push_back(ll);
	boost::geometry::correct(polygon_);
}
bool Polyline::concatenate(Polyline&)
{
	return false;
}
void Polyline::intersection(Polyline&)
{

}
