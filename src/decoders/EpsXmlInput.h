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

#ifndef EpsXmlDecoder_H
#define EpsXmlDecoder_H

#include "magics.h"

#include "EpsXmlInputAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "BasicSceneObject.h"
#include "XmlReader.h"
#include "DateTime.h"

#include <limits>

namespace magics {


class EpsXmlInput:
			public Decoder,
			public Data,
			public PointsList,
			public EpsXmlInputAttributes,
			public XmlNodeVisitor {
public:
	EpsXmlInput();
	virtual ~EpsXmlInput();
	
	virtual void set(const map<string, string>& map) 	{EpsXmlInputAttributes::set(map); }
	virtual void set(const XmlNode& node) { EpsXmlInputAttributes::set(node); }
	
	virtual void visit(Transformation&);
	void visit(const XmlNode& node);
		
	virtual void decode();
  	
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
       {
       	customisedPoints(n, out);
       }
       PointsHandler& points(const Transformation& t, bool) { NOTIMP;}
	
	virtual void visit(TextVisitor&);
	virtual void visit(MetaDataVisitor&);
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 DateTime base_;
	 vector<CustomisedPoint*> points_;
	 double minstep_;
	 double maxstep_;
	 double miny_;
	 double maxy_;
	 string   station_;
	 double latitude_;
	 double longitude_;
	 string   title_;
	 string   parameter_;
	 int dateOffset_;

	 
private:
    //! Copy constructor - No copy allowed
	EpsXmlInput(const EpsXmlInput&);
    //! Overloaded << operator to copy - No copy allowed
	EpsXmlInput& operator=(const EpsXmlInput&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsXmlInput& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
