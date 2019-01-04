/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfOrcaInterpretor.h
    \brief Definition of the Template class NetcdfOrcaInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfOrcaInterpretor_H
#define NetcdfOrcaInterpretor_H

#include "magics.h"


#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "MagException.h"

namespace magics {

class NetcdfOrcaInterpretor: public NetcdfInterpretor {

public:
	NetcdfOrcaInterpretor();
	virtual ~NetcdfOrcaInterpretor();
    
    
    static NetcdfInterpretor* guess(const NetcdfInterpretor&);
    bool interpretAsPoints(PointsList&);
    bool interpretAsMatrix(Matrix**);
    void customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& out, int thinning);

    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
   
     
private:
    //! Copy constructor - No copy allowed
	NetcdfOrcaInterpretor(const NetcdfOrcaInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfOrcaInterpretor& operator=(const NetcdfOrcaInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfOrcaInterpretor& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
