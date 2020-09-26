
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileBinaryDriverAttributes.h
    \brief Definition of BinaryDriver Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef BinaryDriverWrapper_H
#define BinaryDriverWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "BinaryDriver.h"


 
#include "BaseDriverWrapper.h"






 

namespace magics {

class MagRequest;


class BinaryDriverWrapper: public BaseDriverWrapper

{
public:
//  --  constructor
    BinaryDriverWrapper();
    BinaryDriverWrapper(BinaryDriver*);
//  --  destructor
    virtual ~BinaryDriverWrapper();
    virtual void set(const MagRequest&);
    
    BinaryDriver* me()   { return binarydriver_; }
   	
   	virtual BinaryDriver* object() { return binarydriver_; }
	

	virtual void object(BinaryDriver* o) { 
		// Remember to delete the previous object
		binarydriver_ = o;
		BaseDriverWrapper::object(o);
		

	}
    
  

protected:
    BinaryDriver* binarydriver_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const BinaryDriverWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

