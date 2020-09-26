
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileNoValuePlotAttributes.h
    \brief Definition of NoValuePlot Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef NoValuePlotWrapper_H
#define NoValuePlotWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "ValuePlot.h"


 
#include "ValuePlotBaseWrapper.h"






 

namespace magics {

class MagRequest;


class NoValuePlotWrapper: public ValuePlotBaseWrapper

{
public:
//  --  constructor
    NoValuePlotWrapper();
    NoValuePlotWrapper(NoValuePlot*);
//  --  destructor
    virtual ~NoValuePlotWrapper();
    virtual void set(const MagRequest&);
    
    NoValuePlot* me()   { return novalueplot_; }
   	
   	virtual NoValuePlot* object() { return novalueplot_; }
	

	virtual void object(NoValuePlot* o) { 
		// Remember to delete the previous object
		novalueplot_ = o;
		ValuePlotBaseWrapper::object(o);
		

	}
    
  

protected:
    NoValuePlot* novalueplot_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const NoValuePlotWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

