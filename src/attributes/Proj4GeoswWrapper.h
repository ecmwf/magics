
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileProj4GeoswAttributes.h
    \brief Definition of Proj4Geosw Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef Proj4GeoswWrapper_H
#define Proj4GeoswWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "Proj4Projection.h"


 
#include "Proj4ProjectionWrapper.h"






 

namespace magics {

class MagRequest;


class Proj4GeoswWrapper: public Proj4ProjectionWrapper

{
public:
//  --  constructor
    Proj4GeoswWrapper();
    Proj4GeoswWrapper(Proj4Geosw*);
//  --  destructor
    virtual ~Proj4GeoswWrapper();
    virtual void set(const MagRequest&);
    
    Proj4Geosw* me()   { return proj4geosw_; }
   	
   	virtual Proj4Geosw* object() { return proj4geosw_; }
	

	virtual void object(Proj4Geosw* o) { 
		// Remember to delete the previous object
		proj4geosw_ = o;
		Proj4ProjectionWrapper::object(o);
		

	}
    
  

protected:
    Proj4Geosw* proj4geosw_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const Proj4GeoswWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

