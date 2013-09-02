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

/*! \file SymbolInput.h
    \brief Definition of the Template class SymbolInput.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 19-Apr-2005
    
    Changes:
    
*/

#ifndef SymbolInput_H
#define SymbolInput_H

#include "magics.h"
#include <stack>


#include "Data.h"
#include "SymbolInputAttributes.h"


namespace magics {


class SymbolInput: 
	public Data,
	public SymbolInputAttributes {

public:
	SymbolInput() {}
	virtual ~SymbolInput() {}
	
	 // Implements the set method ... 
    void set(const map<string, string>& map ) { SymbolInputAttributes::set(map); }
   

    virtual void decodePoints() { 
    	if ( value_.empty() )
		  	for (doublearray::iterator x = x_.begin(); x != x_.end(); x++)
    			value_.push_back(0);
    	
    	doublearray::iterator x = x_.begin();
    	doublearray::iterator y = y_.begin();
    	doublearray::iterator v = value_.begin();
    	
    	while ( x != x_.end() && y != y_.end() && v != value_.end() ) {
    		points_.push_back(new UserPoint(*x, *y, *v));
    		x++;
    		y++;
    		v++;
    	} 	
    }
    
    virtual void decodePoints(const Transformation& projection) { 
        	if ( value_.empty() )
    		  	for (doublearray::iterator x = x_.begin(); x != x_.end(); x++)
        			value_.push_back(0);
        	
        	doublearray::iterator x = x_.begin();
        	doublearray::iterator y = y_.begin();
        	doublearray::iterator v = value_.begin();
        	
        	while ( x != x_.end() && y != y_.end() && v != value_.end() ) {
        		UserPoint point(*x, *y, *v);
        		std::stack<UserPoint> points;
        		projection.wraparound(point, points);
        		while ( !points.empty() ) {
        			points_.push_back(new UserPoint(points.top()));
        			points.pop();
        		}
        		x++;
        		y++;
        		v++;
        	} 	
        }
  
    
    PointsHandler& points() {
    	decodePoints(); 
    	
    	this->pointsHandlers_.push_back(new PointsHandler(points_));
    	return  (*this->pointsHandlers_.back());
    }   
    
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& out)
    {  
        if ( x_.empty() ) return;
        if ( y_.empty() ) return;
        if ( speed_.empty() ) return;
        if ( direction_.empty() ) return;  
        doublearray::iterator x = x_.begin();
    	doublearray::iterator y = y_.begin();
        doublearray::iterator speed = speed_.begin();
        doublearray::iterator direction = direction_.begin();
        doublearray::iterator v = value_.empty() ? speed_.begin() : value_.begin();
        doublearray::iterator vend = value_.empty() ? speed_.end() : value_.end();
        while ( x != x_.end() && y != y_.end()  && speed != speed_.end() 
                && direction != direction_.end() && v != vend ) {
            CustomisedPoint* point = new CustomisedPoint(*x, *y, "");
            double pi = 3.14/180.; 
            double a = 90 - (*direction);
            a *= pi;
            double xc = *speed * -1 * cos(a);
            double yc = *speed*-1* sin(a);
            point->insert(make_pair("x_component", xc));
            point->insert(make_pair("y_component", yc));
            point->insert(make_pair("colour_component", *v));
            ++x;
            ++y;
            ++speed;
            ++direction;
            ++v; 
            out.push_back(point);
            
        }    
        cout << "MINX" << *min_element(x_.begin(), x_.end()) << endl;
        cout << "MaxX" << *max_element(x_.begin(), x_.end()) << endl;
        cout << "MINY" << *min_element(y_.begin(), y_.end()) << endl;
        cout << "MaxY" << *max_element(y_.begin(), y_.end()) << endl;
        
    }
    
    
      
              
    PointsHandler& points(const Transformation& transformation) {
    	decodePoints(transformation); 
    	
    	this->pointsHandlers_.push_back(new PointsHandler(points_));
    	return  (*this->pointsHandlers_.back());
    } 

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
    {
    	customisedPoints(t, n, out);
    }
    PointsHandler& points(const Transformation& t, bool) { return points(t); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const 
	 	{ 
	 		out << "SymbolInput[";
	 		SymbolInputAttributes::print(out);
	 		out << "]";
	 	}
	 PointsList points_;
	
private:
    //! Copy constructor - No copy allowed
	SymbolInput(const SymbolInput&);
    //! Overloaded << operator to copy - No copy allowed
	SymbolInput& operator=(const SymbolInput&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SymbolInput& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
