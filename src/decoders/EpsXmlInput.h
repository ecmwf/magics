/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
