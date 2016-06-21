/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
