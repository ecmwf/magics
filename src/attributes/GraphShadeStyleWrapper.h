
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileGraphShadeStyleAttributes.h
    \brief Definition of GraphShadeStyle Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef GraphShadeStyleWrapper_H
#define GraphShadeStyleWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "GraphShadeStyle.h"








 

namespace magics {

class MagRequest;


 	
class GraphShadeStyleWrapper 

{
public:
//  --  constructor
    GraphShadeStyleWrapper();
    GraphShadeStyleWrapper(GraphShadeStyle*);
//  --  destructor
    virtual ~GraphShadeStyleWrapper();
    virtual void set(const MagRequest&);
    
    GraphShadeStyle* me()   { return graphshadestyle_; }
   	
   	virtual GraphShadeStyle* object() { return graphshadestyle_; }
	

	virtual void object(GraphShadeStyle* o) { 
		// Remember to delete the previous object
		graphshadestyle_ = o;
		

	}
    
  

protected:
    GraphShadeStyle* graphshadestyle_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const GraphShadeStyleWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

