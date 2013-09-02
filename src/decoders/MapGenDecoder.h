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

/*! \file MapGenDecoder.h
    \brief Definition of the Template class MapGenDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 12-Dec-2005
    
    Changes:
    
*/

#ifndef MapGenDecoder_H
#define MapGenDecoder_H

#include "magics.h"

#include "MapGenDecoderAttributes.h"
#include "Data.h"
#include "Decoder.h"
#include "UserPoint.h"

namespace magics {
	
class XmlNode;


class MapGenDecoder: public MapGenDecoderAttributes, 
		public Data, public Decoder, public vector<PointsList* >
{

public:
	MapGenDecoder();
	virtual ~MapGenDecoder();
	//! Method to access the data as a list of points : Used by psymb.

	virtual void decode();
	void set(const map<string, string>& map ) { MapGenDecoderAttributes::set(map); }
	void set(const XmlNode& node ) { MapGenDecoderAttributes::set(node); }

	PointsHandler& points()
	{ 
		decode();
		if (record_ > int(this->size()) ) record_= 1;
		if (record_ == -1 ) record_ = 1;
		this->pointsHandlers_.push_back(new PointsHandler(*(*this)[record_-1]));
		return *(this->pointsHandlers_.back());
	} 
	PointsHandler& points(const Transformation& )
	{
	        return points();
	}

	void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	 void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
	    {
	    	customisedPoints( n, out);
	    }
	    PointsHandler& points(const Transformation& t, bool) { return points(t); }
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	MapGenDecoder(const MapGenDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	MapGenDecoder& operator=(const MapGenDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MapGenDecoder& p)
		{ p.print(s); return s; }
};


} // namespace magics
#endif
