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

/*! \file EpsgramDecoder.h
    \brief Definition of the Template class EpsgramDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/

#ifndef EpsBufr_H
#define EpsBufr_H

#include "magics.h"


#include "EpsBufrAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "DateTime.h"

#include <limits>

namespace magics {



class EpsBufr:
			public Decoder,
			public Data,
			public PointsList,
			public EpsBufrAttributes {
public:
	EpsBufr();
	virtual ~EpsBufr();
	
	virtual void set(const map<string, string>& map) 	{EpsBufrAttributes::set(map); }
	virtual void set(const XmlNode& node) { EpsBufrAttributes::set(node); }
	
	virtual void visit(Transformation&);
	void visit(const XmlNode& node);
		
	virtual void decode();
  	
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	virtual PointsHandler& points();
	virtual void visit(TextVisitor&);
	virtual void visit(MetaDataVisitor&);
	void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool)
	{
		customisedPoints(n, out);
	}
	
		PointsHandler& points(const Transformation&, bool) { return  points(); }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 DateTime base_;
	 vector<CustomisedPoint*> points_; 
     double minstep_;
	 double maxstep_;
	 double miny_;
	 double maxy_;
	 double shift_;
	 
	 
private:
    //! Copy constructor - No copy allowed
	EpsBufr(const EpsBufr&);
    //! Overloaded << operator to copy - No copy allowed
	EpsBufr& operator=(const EpsBufr&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsBufr& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
