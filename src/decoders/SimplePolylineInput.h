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

/*! \file SimplePolylineInput.h
    \brief Definition of the Template class SimplePolylineInput.
    
    Magics Team - ECMWF 2007
    
    Started: Mon 19-Mar-2007
    
    Changes:
    
*/

#ifndef SimplePolylineInput_H
#define SimplePolylineInput_H

#include "magics.h"

#include "SimplePolylineInputAttributes.h"
#include "Data.h"
#include "UserPoint.h"

namespace magics {

class SimplePolylineInput: 
        public SimplePolylineInputAttributes, 
        public Data,
        public PointsList {

public:
	SimplePolylineInput();
	virtual ~SimplePolylineInput();
    virtual void decode();
    void set(const map<string, string>& map ) { SimplePolylineInputAttributes::set(map); }
	void set(const XmlNode& node ) { SimplePolylineInputAttributes::set(node); }

  
    PointsHandler& points()  {
    	decode();
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    } 

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
    {
    }
    PointsHandler& points(const Transformation&, bool) { return points();  }


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	SimplePolylineInput(const SimplePolylineInput&);
    //! Overloaded << operator to copy - No copy allowed
	SimplePolylineInput& operator=(const SimplePolylineInput&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SimplePolylineInput& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
