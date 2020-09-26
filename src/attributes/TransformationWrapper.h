
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileTransformationAttributes.h
    \brief Definition of Transformation Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef TransformationWrapper_H
#define TransformationWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "Transformation.h"








 

namespace magics {

class MagRequest;


 	
class TransformationWrapper 

{
public:
//  --  constructor
    TransformationWrapper();
    TransformationWrapper(Transformation*);
//  --  destructor
    virtual ~TransformationWrapper();
    virtual void set(const MagRequest&);
    
    Transformation* me()   { return transformation_; }
   	
   	virtual Transformation* object() { return transformation_; }
	

	virtual void object(Transformation* o) { 
		// Remember to delete the previous object
		transformation_ = o;
		

	}
    
  

protected:
    Transformation* transformation_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const TransformationWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

