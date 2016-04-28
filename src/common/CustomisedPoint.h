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

/*! \file CustomisedPoint.h
    \brief Definition of the Template class CustomisedPoint.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 19-May-2005
    
    Changes:
    
*/

#ifndef CustomisedPoint_H
#define CustomisedPoint_H

#include "magics.h"
#include "VectorOfPointers.h"
#include "DateTime.h"

namespace magics {

class CustomisedPoint : public map<string, double> {

public:
	CustomisedPoint(): latitude_(0), longitude_(0), identifier_(""), missing_(false) {}
	CustomisedPoint(double lon, double lat, string ident) :
		latitude_(lat), longitude_(lon), identifier_(ident), missing_(false){}
	virtual ~CustomisedPoint() {}
	
	double latitude() const { return latitude_; }
	void latitude(double latitude) { latitude_ = latitude; }
	
	double longitude() const { return longitude_; }
	void longitude(double longitude) { longitude_ = longitude; }
	
	const DateTime& reference() const { return reference_; }
	void reference(DateTime ref) { reference_ = ref; }
	
	const DateTime& base() const { return base_; }
	void base(DateTime base) { base_ = base; }
	
	const DateTime& valid() const { return valid_; }
	void valid(DateTime valid) { valid_ = valid; }
	
	double referenceStep() const { return valid_- reference_; }
	double step() const { return valid_- base_; }
	
	string identifier() const { return identifier_; }
	void identifier(const string& identifier) { identifier_ = identifier; }

	string type() const { return type_; }
	void type(const string& type) { type_ = type; }
	bool missing() const { return missing_; }
	void missing(bool missing) { missing_  = missing; }

	double distance(double lat, double lon) const {
		return sqrt( (lat-latitude_)*(lat-latitude_) +(lon-longitude_)*(lon-longitude_) );
	}
	void copyProperties(const CustomisedPoint& other) {
		for ( const_iterator value = other.begin(); value != other.end(); ++ value) {
			 insert(make_pair(value->first, value->second));
		}
	}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const
	 { 
	 	out << "CustomisedPoint[";
	 	out << "latitude=" << latitude_;
	 	out << ", longitude=" << longitude_;
	 	out << ", identifier=" << identifier_;
	 	for ( const_iterator value = begin(); value != end(); ++ value) 
	 		out << ", " << value->first << "=" << value->second;
	 	out << "]";
	 }  
	 double  latitude_;
	 double  longitude_;
	 string   identifier_;
	 string   type_;
     DateTime base_;
     DateTime valid_;
     DateTime reference_;
     bool     missing_;
     
private:
    //! Copy constructor - No copy allowed
	CustomisedPoint(const CustomisedPoint&);
    //! Overloaded << operator to copy - No copy allowed
	CustomisedPoint& operator=(const CustomisedPoint&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CustomisedPoint& p)
		{ p.print(s); return s; }

};


typedef  VectorOfPointers< vector<CustomisedPoint*> > CustomisedPointsList;  

} // namespace magics
#endif
