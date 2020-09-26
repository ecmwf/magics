
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileHistogramLegendMethodAttributes.h
    \brief Definition of HistogramLegendMethod Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef HistogramLegendMethodWrapper_H
#define HistogramLegendMethodWrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"



#include "LegendMethod.h"


 
#include "LegendMethodWrapper.h"






 

namespace magics {

class MagRequest;


class HistogramLegendMethodWrapper: public LegendMethodWrapper

{
public:
//  --  constructor
    HistogramLegendMethodWrapper();
    HistogramLegendMethodWrapper(HistogramLegendMethod*);
//  --  destructor
    virtual ~HistogramLegendMethodWrapper();
    virtual void set(const MagRequest&);
    
    HistogramLegendMethod* me()   { return histogramlegendmethod_; }
   	
   	virtual HistogramLegendMethod* object() { return histogramlegendmethod_; }
	

	virtual void object(HistogramLegendMethod* o) { 
		// Remember to delete the previous object
		histogramlegendmethod_ = o;
		LegendMethodWrapper::object(o);
		

	}
    
  

protected:
    HistogramLegendMethod* histogramlegendmethod_;


//  --  method
	virtual void print(ostream&) const;
	

private:
    string tag_;
	friend ostream& operator<<(ostream& s,const HistogramLegendMethodWrapper& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

