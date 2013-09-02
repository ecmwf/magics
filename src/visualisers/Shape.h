/*! \file Shape.h
    \brief Definition of the Template class Shape.
    
    Magics Team - ECMWF 2010
    
    Started: Fri 12-Mar-2010
    
    Changes:
    
*/

#ifndef Shape_H
#define Shape_H

#include "magics.h"
#include "Colour.h"

namespace magics {



struct Shape: public list<pair<double, double> >
{
	Shape();
    Shape(const Colour& colour, double val);
	~Shape() {}
	void push_back(double x, double y);
	double value_;
	Colour  colour_;
	double minx_;
	double maxx_;
	double miny_;	
	double maxy_;
	
	
	bool intersect(Shape& other) {
		if ( minx_ > other.maxx_ ) return false;
		if ( maxx_ < other.minx_ ) return false;
		if ( miny_ > other.maxy_ ) return false;
		if ( maxy_ < other.miny_ ) return false;
		return true;
	}

	void clean() {
		clear();
		
		minx_ = numeric_limits<double>::max();
		maxx_ = -numeric_limits<double>::max();
		miny_ = numeric_limits<double>::max();
		maxy_ = -numeric_limits<double>::max();
	}
	
	void reduce();
  
	
	void intersection(Shape& other);
	
    bool concatenate(Shape& other);
	
    void simplify(iterator from);
	void  reduce(iterator);
    
	iterator next(iterator);
	iterator previous(iterator);
	void print(ostream& out)  const;

	friend ostream& operator<<(ostream& out,const Shape& s) 
	{
	 
	  s.print(out);
	 
	  return out; 
	}
};


} // namespace magics
#endif
