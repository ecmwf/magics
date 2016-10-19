/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Clipper.h
    \brief Definition of the clipping.
    \author Meteorological Visualisation Section, ECMWF

    Started: 2004

*/ 

#ifndef Clipper_H
#define Clipper_H

#include "magics.h"
#include "PolyCoast.h"
#include "Transformation.h"

namespace magics {

template <class P> 
class Push 
{
public:
	Push(Polyline& poly) : poly_(poly) {}
	~Push() {}
	void operator()(P& point) { poly_.push_back(point); }
protected:
	Polyline& poly_;
};


template <class P>
bool clockwise(Polyline& c) {
        double area = 0 ;
        if(c.size()) c.push_back(c[0]);

        for(unsigned int j = 0; j < c.size(); j++)
        {
                if(j) area += ( c[j-1].x() * c[j].y() -
                                c[j].x()   * c[j-1].y() );
        }
        if(c.size()) c.pop_back();
        return area < 0;
}

template <class P>
class Empty
{
public:
   Empty() {}
   ~Empty() {}
   bool operator()(const Polyline* poly)
   {
      if ( poly->empty() ) {
        delete poly;
        return true;
      }
      return false;
   }
};


template <class C>
class Clipper
{
public:
	Clipper(const Transformation& transformation) : 
		lastX_(INT_MIN), lastY_(INT_MIN), 
		lastIn_(false),  current_(0), 
		transformation_(transformation),
		missing_(true) 
	{
		left_ = std::min(transformation_.getMinPCX(), transformation_.getMaxPCX());
		right_ = std::max(transformation_.getMinPCX(), transformation_.getMaxPCX());
		top_ = std::max(transformation_.getMinPCY(), transformation_.getMaxPCY());
		bottom_ = std::min(transformation_.getMinPCY(), transformation_.getMaxPCY());
	}
	~Clipper() {}
	
	void feed(C& out) { 
		for(  vector<Polyline* >::iterator line = helper_.begin(); line != helper_.end(); ++line) {
			if ( (*line)->getShading() ) {
				(*line)->setFilled(true);
			
			}
			out.push_back(*line);
		}
	}
	void adjust(double left, double bottom, double right, double top) {
		left_ = left;
		right_ = right;
		top_ = top;
		bottom_ = bottom;
	}

    bool pointin_;
    void close(PolyCoast& poly)
    {
    	// Fisrt we reproject! 
    	current_ = poly.getShade();
    	
    	if ( current_ ) { 
    		vector<UserPoint>& geo = poly.coastlines();
    		pointin_ = false;
    		for ( vector<UserPoint>::const_iterator point = geo.begin(); point != geo.end(); ++point)    		
    			close(transformation_(*point));
    		current_->push_back(current_->front());   	
        	 helper_.push_back(current_);
    	}

    	current_ = poly.getContour();
    	vector<UserPoint>& geo = poly.coastlines();     	
    	for ( vector<UserPoint>::const_iterator point = geo.begin(); point != geo.end(); ++point)    		
    		(*this)( transformation_(*point));   	    		
    	  if ( !current_->empty() ) {
    		  helper_.push_back(current_); 
    	  }
    	  else 
    		  delete current_; // Was allocated for nothing!
    }
	

    void operator()(const Polyline& poly)
    {
    	//if  ( poly.allMissing() ) return;
        current_ = new Polyline();   
    	current_->copy(poly);  	
    	lastX_ = INT_MIN;
    	lastY_ = INT_MIN; 
		lastIn_ = false;
    	value_ = poly.front().value();
    	allmissing_ = poly.allMissing();
        static int i = 0;
        i++;


    	for ( Polyline::const_iterator point = poly.begin(); point != poly.end(); ++point) {

    	 	(*this)(*point);
    	}
    	
    	
    	if ( !transformation_.concatenate(helper_, current_) ) 
            	helper_.push_back(current_);
    }
   

	void ignoreMissing(bool missing = true) { missing_ = !missing; }
 
	MAGICS_NO_EXPORT bool in(double x, double y) const
	{
		return ((y <= top_ && y >= bottom_) && (x >= left_ && x <= right_));
	}

#define A(x1, y1, x2, y2) (y2 -y1)/(x2-x1)
#define B(a, x, y) (y - a * x)
#define X(a, b, y) ((y - b)/a)
#define Y(a, b, x) ((a * x) + b)

    MAGICS_NO_EXPORT void push_intersection(double x, double y) 
    {
        if (lastX_ == INT_MIN) return;

        double a = (lastX_ == x) ? 0 : A(lastX_, lastY_, x, y);
        double b = B(a, x, y);

        // Intersect left?
        if  ( (x < left_ && lastX_ > left_) || (lastX_ < left_ && x > left_) )
	{
            current_->push_back(PaperPoint(left_, Y(a, b, left_), value_, allmissing_));
            return;
        }
        // Intersect top?
    if  ( (y < top_ && lastY_ > top_) || ( lastY_ > bottom_ && lastY_ < top_ && y > top_) )
	{
        double nx = (!a) ? x : X(a, b, top_);
        
        if ( nx > right_ ) nx = right_;
        if ( nx < left_ ) nx = left_;
		
        current_->push_back(PaperPoint(nx, top_, value_, allmissing_));
		
                return;
	}

        // Intersect right?
        if ( (x < right_ && lastX_ > right_) || (lastX_ < right_ && x > right_) )
	{
            current_->push_back(PaperPoint(right_, Y(a, b, right_), value_, allmissing_));
            return;
        }

        // intersect bottom?
        if ( (y < bottom_ && lastY_ > bottom_ && lastY_ < top_ ) || ( lastY_ < bottom_ && y > bottom_) )
	{
            if (!a ) current_->push_back(PaperPoint(x, bottom_));
            else current_->push_back(PaperPoint(X(a, b, bottom_), bottom_, value_, allmissing_));
            return;
        }
    }

    MAGICS_NO_EXPORT void operator()(const PaperPoint& point)
    {
    	if (missing_ && point.missing () && !current_->empty()) {
            if ( !transformation_.concatenate(helper_, current_) ) 
            	helper_.push_back(current_);
    		Polyline* poly = new Polyline();
            poly->copy(*current_);            
            current_ = poly;
            //out_.push_back(current_);
            return;
    	}

        if ( in(point.x(), point.y()) ) {
            if (lastIn_ ) {
                current_->push_back(point);
                lastX_ = point.x();
                lastY_ = point.y();
				lastIn_ = true;
                return;
            }
            else {
                // We are entering the area ..
                // find intersection -->Push intersection + point... 
            	current_->clear();
                lastIn_ = true;
                push_intersection(point.x(), point.y());
                current_->push_back(point);
                lastX_ = point.x();
                lastY_ = point.y();
		
                return; 
            }
            return;
        }
	
        // Point is out 
        
        if (current_->empty()) {
            lastX_ = point.x();
            lastY_ = point.y();
            lastIn_ = false; 
	    
            return;
        }
        // Why?
//        if (current_->size() == 1 ) {
//        	current_->clear();
//            lastX_ = point.x();
//            lastY_ = point.y();
//            lastIn_ = false; 
//	    
//            return;
//        }
        
        if (!lastIn_) { 
             lastX_ = point.x();
             lastY_ = point.y();
            return;
        }
        else {
            // push_back intersection
            push_intersection(point.x(), point.y());
            lastX_ = point.x();
            lastY_ = point.y();
            // create new Polyline...
           Polyline* poly = new Polyline();
            if ( !transformation_.concatenate(helper_, current_) ) 
            	helper_.push_back(current_);
            poly->copy(*current_);  
            current_ = poly;
            lastIn_ = false;
        }
        return;
     }
    
    //! Only used for coastlines
    MAGICS_NO_EXPORT void close(const PaperPoint& point)
    {
        if ( in(point.x(), point.y()) ) {
           if ( !pointin_ ) {
              push_intersection(point.x(), point.y());
           }
           current_->push_back(point);

           pointin_ = true;
        }
    	
        // first point is out!
        else {
          if ( pointin_) {
             // We have to calculate the intersction
             lastX_ = current_->back().x();
             lastY_ = current_->back().y();
             push_intersection(point.x(), point.y());
          }
          else {
             double x =  point.x();
             double y =  point.y();
             lastX_ = x;
             lastY_ = y;

             x = std::min(x, right_);
             x = std::max(x, left_);
             y = std::min(y, top_);
             y = std::max(y, bottom_);
             current_->push_back(PaperPoint(x, y, value_, allmissing_));
          }
          pointin_ = false;
        }
    }
    
    MAGICS_NO_EXPORT void clip(const PaperPoint& point)
    {
        // ONly used for coastlines!
        if ( in(point.x(), point.y()) ) {
          current_->push_back(point);
        }
        else {
          if ( !current_->empty() ) {
              helper_.push_back(current_);
              current_ = current_->getNew();
          }
        }
    }

private:
    double left_;
    double bottom_;
    double right_;
    double top_;
    double lastX_;
    double lastY_;
    bool lastIn_;

    Polyline* current_;
    const Transformation& transformation_;
    vector<Polyline* > helper_;
    bool missing_;

    double value_;
    bool allmissing_;
};

}
#endif
