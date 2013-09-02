/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file PointsHandler.h
    \brief Definition of the Template class PointsHandler.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 20-Jan-2004
    
    Changes:
    
*/

#ifndef PointsHandler_H
#define PointsHandler_H

#include "magics.h"

#include "BasePointsHandler.h"
#include "Transformation.h"

namespace magics {


class PointsHandler: public AbstractPoints {

public:
	PointsHandler(AbstractPoints& handler):handler_(handler) {}
	virtual ~PointsHandler() {}
    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst() { handler_.setToFirst(); }
    //! Method to test the end of collection.
    virtual bool more()        { return handler_.more(); }
    //! Method to return the current value
    virtual const UserPoint& current()  { return handler_.current(); }
    //! Method to advance
    virtual void advance()   { handler_.advance(); }
    virtual bool empty()   {  handler_.setToFirst(); return handler_.more() == false; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "PointsHandler on " << handler_; }
     AbstractPoints& handler_;

private:
    //! Copy constructor - No copy allowed
	PointsHandler(const PointsHandler&);
    //! Overloaded << operator to copy - No copy allowed
	PointsHandler& operator=(const PointsHandler&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PointsHandler& p)
		{ p.print(s); return s; }

};

class BatchPointsHandler : public PointsHandler
{
public:
	BatchPointsHandler(AbstractPoints& handler, int count):
		PointsHandler(handler),  count_(count), more_(0), last_(false) {}
	virtual ~BatchPointsHandler() {}
    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst()  {}
    //! Method to test the end of collection.
    virtual bool more()
    {
         if ( last_ ) {
            last_ = false;
            return false;
        }
        //MagLog::debug() << "batch " << more_ << "\n";
        if (more_++ < count_) return this->handler_.more();  
       
        last_ = true;
        return true;
    }
    //! Method to return the current value
    virtual const UserPoint& current()  { return this->handler_.current(); }
    //! Method to advance
    virtual void advance()   { this->handler_.advance(); }
  
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "BatchPointsHandler(" << count_ << "items) on " << this->handler_; }
 
     int count_;
     mutable int more_;
     mutable bool last_;
     
private:
    //! Copy constructor - No copy allowed
	BatchPointsHandler(const BatchPointsHandler&);
    //! Overloaded << operator to copy - No copy allowed
	BatchPointsHandler& operator=(const BatchPointsHandler&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BatchPointsHandler& p)
		{ p.print(s); return s; }

};



class BoxPointsHandler : public PointsHandler
{
public:
	BoxPointsHandler(AbstractPoints& handler, const Transformation& transformation, bool filter) :
		PointsHandler(handler),
		transformation_(transformation),
		filter_(filter) {}
	virtual ~BoxPointsHandler() {}
	
    //! Method to set the current position to the first point.(abstract)    
    virtual void setToFirst() ;
    
    //! Method to test the end of collection.
    virtual bool more()
    {
    	return more_;
        
    }
    
    //! Method to return the current value
    virtual UserPoint& current()  { return current_; }
    
    //! Method to advance
    virtual void advance() ;
  
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "BocPointsHandler() on " << this->handler_; }
    
     const Transformation& transformation_;
     mutable std::stack<UserPoint>            duplicates_;

     mutable UserPoint               current_;
     mutable bool more_;
     bool filter_; // Do no send the point if they are outside the transformation view!

};

class ThinningPointsHandler : public PointsHandler
{
public:
	ThinningPointsHandler(AbstractPoints& handler, int xfreq, int yfreq):
		PointsHandler(handler),
		xfreq_(xfreq), yfreq_(yfreq) {
	}
	virtual ~ThinningPointsHandler() {}

    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst();

    //! Method to test the end of collection.
    virtual bool more()
    {
    	return more_;

    }

    //! Method to return the current value
    virtual const UserPoint& current()  { return current_; }

    //! Method to advance
    virtual void advance() ;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "BocPointsHandler() on " << this->handler_; }

	 mutable int xfreq_;
	 mutable int yfreq_;
	 mutable map<double, map<double, UserPoint> > data_;
	 map<double, map<double, UserPoint> >::iterator y_;
	 map<double, UserPoint>::iterator x_;
     mutable UserPoint               current_;
     mutable bool more_;

};
    
} // namespace magics

#endif
