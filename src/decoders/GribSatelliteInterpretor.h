/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file GribSatelliteInterpretor.h
    \brief Definition of the Template class GribSatelliteInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 18-Apr-2005
    
    Changes:
    
*/

#ifndef GribSatelliteInterpretor_H
#define GribSatelliteInterpretor_H

#include "magics.h"

#include "GribInterpretor.h"

namespace magics {

class GribSatelliteInterpretor: public GribInterpretor {

public:
	GribSatelliteInterpretor();
	virtual ~GribSatelliteInterpretor();
	
//	virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const;
    virtual void interpretAsVectors(const GribDecoder&, Matrix&, Matrix&) const 
    	{ throw  NotYetImplemented("Satellite representation", " Vectors"); }
    virtual void interpretAsMatrix(const GribDecoder&, Matrix**) const;

	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	GribSatelliteInterpretor(const GribSatelliteInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	GribSatelliteInterpretor& operator=(const GribSatelliteInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribSatelliteInterpretor& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
