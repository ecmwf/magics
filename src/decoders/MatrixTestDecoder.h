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

/*! \file MatrixTestDecoder.h
    \brief Definition of the Template class MatrixTestDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 25-Mar-2004
    
    Changes:
    
*/

#ifndef MatrixTestDecoder_H
#define MatrixTestDecoder_H

#include "magics.h"
#include "MagException.h"

#include "Data.h"
#include "Decoder.h"


namespace magics {

class MatrixTestDecoder: public Decoder, public Data {

public:
	MatrixTestDecoder();
	virtual ~MatrixTestDecoder();
	// implements Decoder interface
	void decode() {}

	virtual void set(const map<string, string>& ) {}
	
	 void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
	    {
	    	ASSERT(false);
	    }
	    PointsHandler& points(const Transformation& t, bool) { ASSERT(false); }
	
    
	PointsHandler& points() { throw MethodNotYetImplemented("MatrixTestDecoder::points()"); }
	virtual MatrixHandler& matrix()
	{
		decode();
		matrixHandlers_.push_back(new  MatrixHandler(matrix_));
		return *(matrixHandlers_.back());
	}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	Matrix matrix_;

private:
	//! Copy constructor - No copy allowed
	MatrixTestDecoder(const MatrixTestDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	MatrixTestDecoder& operator=(const MatrixTestDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MatrixTestDecoder& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
