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

/*! \file GeoPointsDecoder.h
    \brief Definition of the Template class GeoPointsDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 12-Dec-2005
    
    Changes:
    
*/

#ifndef GeoPointsDecoder_H
#define GeoPointsDecoder_H

#include "magics.h"

#include "GeoPointsDecoderAttributes.h"
#include "Data.h"
#include "Decoder.h"
#include "UserPoint.h"

namespace magics {
	
class XmlNode;

class GeoPointsDecoder: public GeoPointsDecoderAttributes,
		public Data, public Decoder, public PointsList {

public:
	GeoPointsDecoder();
	virtual ~GeoPointsDecoder();
	   //! Method to access the data as a list of points : Used by psymb.
   
     virtual void decode(const Transformation&);
     virtual void decode();
     void set(const map<string, string>& map ) { GeoPointsDecoderAttributes::set(map); }
	 void set(const XmlNode& node ) { GeoPointsDecoderAttributes::set(node); }

  
    
	 PointsHandler& points()  {
	     	decode();
	     	pointsHandlers_.push_back(new PointsHandler(*this));
	     	return *(pointsHandlers_.back());
	     } 
    PointsHandler& points(const Transformation& transformation)  {
    	decode(transformation);
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    } 
    
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& );

    void add(const Transformation&, UserPoint&); 
    void add(const Transformation&, CustomisedPoint&);
    void yxdtlv2(const string&, const Transformation&);
    void xyv2(const string&, const Transformation&);
    void yxdtlv1(const string&);
    void xyv1(const string&s);

    void polar(const string&, const Transformation&);
    void lluv(const string&, const Transformation&); 
    void initInfo();
    void visit(MetaDataCollector&);
    void visit(ValuesCollector&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
       {
       	customisedPoints(t, n, out);
       }
       PointsHandler& points(const Transformation& t, bool) { return points(t); }
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 typedef void (GeoPointsDecoder::*SimpleDecode)(const string&);
	 typedef void (GeoPointsDecoder::*Decode)(const string&, const Transformation&);
	 std::map<string, Decode> formats_;
	 std::map<string, SimpleDecode> simple_formats_;
	 vector<CustomisedPoint*> customisedPoints_;
private:
    //! Copy constructor - No copy allowed
	GeoPointsDecoder(const GeoPointsDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	GeoPointsDecoder& operator=(const GeoPointsDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GeoPointsDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
