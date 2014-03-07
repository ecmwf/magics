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

/*! \file ObsJSon.h
    \brief Definition of the Template classBufrJSon.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2011
    
    Changes:
    
*/

#ifndef ObsJSon_H
#define ObsJSon_H

#include "magics.h"


#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "json_spirit.h"
#include "ObsJSonAttributes.h"

namespace magics {



class ObsJSon:
			public ObsJSonAttributes,
			public Decoder,
			public Data,
			public PointsList
{
public:
	ObsJSon();
	virtual ~ObsJSon();
	
	void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& );
	void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all);

	PointsHandler& points(const Transformation&, bool) { assert(false); }

	virtual void set(const map<string, string>& map) 	{ ObsJSonAttributes::set(map); }
	virtual void set(const XmlNode& node) { ObsJSonAttributes::set(node); }
	
	void decode();
	void  getInfo(const std::set<string>&, multimap<string, string>&);
	void visit(MetaDataVisitor&);
    CustomisedPoint* decode(json_spirit::Object&);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 vector<CustomisedPoint*> points_; 
	 std::set<string>    types_;
	 typedef void (ObsJSon::*Method)(const json_spirit::Value&, CustomisedPoint&);
	 map<string,  Method > methods_;

	 void latitude(const json_spirit::Value&, CustomisedPoint&);
	 void longitude(const json_spirit::Value&, CustomisedPoint&);
	 void type(const json_spirit::Value&, CustomisedPoint&);
	 void identifier(const json_spirit::Value&, CustomisedPoint&);
private:
    //! Copy constructor - No copy allowed
	ObsJSon(const ObsJSon&);
    //! Overloaded << operator to copy - No copy allowed
	ObsJSon& operator=(const ObsJSon&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ObsJSon& p)
		{ p.print(s); return s; }

};











} // namespace magics
#endif
