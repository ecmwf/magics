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

/*! \file InputMatrixRegularInterpretor.h
    \brief Definition of the Template class InputMatrixRegularInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/

#ifndef GeoMatrixInterpretor_H
#define GeoMatrixInterpretor_H

#include "magics.h"

#include "InputMatrixInterpretorAttributes.h"
#include "InputMatrixInterpretor.h"

namespace magics {

class InputMatrixRegularInterpretor: public MatrixInterpretorAttributes, public InputMatrixInterpretor {

public:
	InputMatrixRegularInterpretor();
	virtual ~InputMatrixRegularInterpretor();
	
	virtual void set(const map<string, string>& map) { GeoMatrixInterpretorAttributes::set(map); }
    virtual void set(const XmlNode& node) { GeoMatrixInterpretorAttributes::set(node); }
    
    virtual void geoInterpret(Matrix&, const InputMatrix<UserPoint>&);
    virtual void xyInterpret(Matrix&, const InputMatrix<UserPoint>&);

    typedef void (InputMatrixRegularInterpretor::*Mapper)(); 
	std::map<string, Mapper> mappers_;
	
	
	


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

     void upperLeft();
     void lowerLeft();
     void upperRight();
     void lowerRight();     
     void upperLeftTransposed();
     void lowerLeftTransposed();
     void upperRightTransposed();
     void lowerRightTransposed();
     

private:
    //! Copy constructor - No copy allowed
	InputMatrixRegularInterpretor(const InputMatrixRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	InputMatrixRegularInterpretor& operator=(const InputMatrixRegularInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputMatrixRegularInterpretor& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
