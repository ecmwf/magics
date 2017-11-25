/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PointsHandler.h
    \brief Definition of the Template class PointsHandler.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 20-Jan-2004
    
    Changes:
    
*/



#include "magics.h"

#include "PointsHandler.h"

#include <stack>


using namespace magics;







    //! Method to set the current position to the first point.(abstract)    
void BoxPointsHandler::setToFirst()  {
	this->handler_.setToFirst();
    	if (this->handler_.more() == false) {
    		more_ = false;
    		return;
    	}

    	advance(); 
    	
    }
    

    
    //! Method to advance
void BoxPointsHandler::advance()   {
	    // First we delete the previous one.



    	if ( !duplicates_.empty() ) {
    		current_ = duplicates_.top();

    		duplicates_.pop();
    		more_ = true;
    		return;
    	}
    	while ( this->handler_.more() ) {
    		current_ = this->handler_.current();
    		this->handler_.advance();
    		
    		transformation_.wraparound(current_, duplicates_);

    		while ( !duplicates_.empty() ) {
    			// point are falled msiing if they are our=tside the view!
    			current_ = duplicates_.top();

    			duplicates_.pop();
    			if ( current_.missing() && filter_ ) // we go to the next point!
    				continue;
    			else {
    				more_ = true;
    				return;
    			}
    		}   	
    	}
    	more_ = false;
    }
  

    //! Method to set the current position to the first point.(abstract)
void ThinningPointsHandler::setToFirst()  {
	   this->handler_.setToFirst();
    	if ( this->handler_.more() == false ) {
    		more_ = false;
    		return;
    	}


    	    	current_ = this->handler_.current();

    	    	xfreq_=std::max(1, xfreq_);
    	    	yfreq_=std::max(1, yfreq_);
    	    	// we have to prepare the list
    	    	while ( this->handler_.more() ) {
    	    		current_ = this->handler_.current();
    	    		double x = current_.x();
    	    		double y = current_.y();

    	    		map<double, map<double, UserPoint> >::iterator row = data_.find(y);
    	    		if ( row == data_.end() ) {
    	    			data_.insert(make_pair(y, map<double, UserPoint>()));
    	    			row = data_.find(y);
    	    		}
    	    		row->second.insert(make_pair(x, current_));

    	    		this->handler_.advance();
    	    	}
    	    	y_ = data_.begin();


    	    	if ( y_ == data_.end() ) {
    	    		more_ = false;
    	    		return;
    	    	}
    	    	x_ =  y_->second.begin();
    	    	if ( x_ == y_->second.end() ) {
    	    		more_ = false;
    	    		return;
    	    	}
    	    	current_ = x_->second;
    	    	more_ = true;
    }



    //! Method to advance
void ThinningPointsHandler::advance()   {
    	// look on the y_...
    	int j = 0;
    	while (j++ < xfreq_ ) {
    		x_++;
    		if ( x_ == y_->second.end() ) {
    			int i = 0;
    			while (i++ < yfreq_ ) {
    				y_++;
    				if ( y_ == data_.end() ) {
    					more_ = false;
    					return;
    				}
    				x_ = y_->second.begin();
    				j = xfreq_;
    			}
    		}
    	}
    	if ( y_ == data_.end() || x_ ==  y_->second.end() ) {
    		more_ = false;
    		return;
    	}

    	more_ = true;
    	current_ = x_->second;
    }




