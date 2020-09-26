
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileOdaXYDecoderAttributes.h
    \brief Definition of OdaXYDecoder Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef OdaXYDecoderWrapper_H
#define OdaXYDecoderWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "OdaDecoder.h"








#include "BinningObjectWrapper.h"
 

namespace magics {

class MagRequest;


 	
class OdaXYDecoderWrapper 

{
public:
//  --  constructor
    OdaXYDecoderWrapper();
    OdaXYDecoderWrapper(OdaXYDecoder*);
//  --  destructor
    virtual ~OdaXYDecoderWrapper();
    virtual void set(const MagRequest&);
    
    OdaXYDecoder* me()   { return odaxydecoder_; }
   	
   	virtual OdaXYDecoder* object() { return odaxydecoder_; }
	

	virtual void object(OdaXYDecoder* o) { 
		// Remember to delete the previous object
		odaxydecoder_ = o;
		

	}
    
  

protected:
    OdaXYDecoder* odaxydecoder_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const OdaXYDecoderWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

