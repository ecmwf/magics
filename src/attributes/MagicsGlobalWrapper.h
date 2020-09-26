
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileMagicsGlobalAttributes.h
    \brief Definition of MagicsGlobal Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef MagicsGlobalWrapper_H
#define MagicsGlobalWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "MagicsGlobal.h"








 

namespace magics {

class MagRequest;


 	
class MagicsGlobalWrapper 

{
public:
//  --  constructor
    MagicsGlobalWrapper();
    MagicsGlobalWrapper(MagicsGlobal*);
//  --  destructor
    virtual ~MagicsGlobalWrapper();
    virtual void set(const MagRequest&);
    
    MagicsGlobal* me()   { return magicsglobal_; }
   	
   	virtual MagicsGlobal* object() { return magicsglobal_; }
	

	virtual void object(MagicsGlobal* o) { 
		// Remember to delete the previous object
		magicsglobal_ = o;
		

	}
    
  

protected:
    MagicsGlobal* magicsglobal_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const MagicsGlobalWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

