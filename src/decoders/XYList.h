/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XYList.h
    \brief Definition of the Template class XYList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 6-May-2004
    
    Changes:
    
*/

#ifndef XYList_H
#define XYList_H

#include "magics.h"
#include "MagException.h"

#include "Data.h"
#include "XYListAttributes.h"
#include "UserPoint.h"


namespace magics {


class XYList: public Data,
              public XYListAttributes, 
              public PointsList
{
public:
	XYList() {}
	virtual ~XYList() {}

	void prepare();
	void set(const map<string, string>& map ) { XYListAttributes::set(map); }
	void set(const XmlNode& node ) { XYListAttributes::set(node); }
	void visit(Transformation& transformation);


	PointsHandler& points(const Transformation&)  {
	    	prepare();
	    	this->pointsHandlers_.push_back(new PointsHandler(*this));
	    	return *(this->pointsHandlers_.back());
	    } 
	
	PointsHandler& points(){
		prepare();
		this->pointsHandlers_.push_back(new PointsHandler(*this));
		return *(this->pointsHandlers_.back());
	}

	virtual void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& );
	void getReady(const Transformation&);
	virtual void points(const Transformation&, vector<UserPoint>&);

	void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
	{
		customisedPoints(t, n, out);
	}
	PointsHandler& points(const Transformation& t, bool) { return points(t); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	XYList(const XYList&);
    //! Overloaded << operator to copy - No copy allowed
	XYList& operator=(const XYList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XYList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
