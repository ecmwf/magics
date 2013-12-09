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

/*! \file InputMatrix.h
    \brief Definition of the Template class InputMatrix.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/

#ifndef InputMatrix_H
#define InputMatrix_H

#include "magics.h"

#include "InputMatrixAttributes.h"
#include "Data.h"

namespace magics {


class InputMatrix: public Data, public InputMatrixAttributes {

public:
	InputMatrix();
	virtual ~InputMatrix();
	
	virtual void set(const map<string, string>& map) { InputMatrixAttributes::set(map); }
	virtual void set(const XmlNode& node) { InputMatrixAttributes::set(node); }

    void getReady(const Transformation& transformation);

	virtual MatrixHandler& matrix();
	virtual MatrixHandler& xComponent();
	virtual MatrixHandler& yComponent();
	void prepareComponents();
	std::pair<double, double> sd2uv(double s, double d);
	virtual PointsHandler& points(const Transformation&);
	
	void customisedPoints(const BasicThinningMethod&, const Transformation&, const std::set<string>&, CustomisedPointsList& );
	void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
	{}
	PointsHandler& points(const Transformation& t, bool) { return points(t); }
	
	bool defined()
	{
	   return !field_.empty() || 
	           (
	             (!u_component_.empty() && !u_component_.empty() )    ||
	             (!wind_speed_.empty()  && !wind_direction_.empty() ) || 
	             simple_field_
	           );
	}
	void filter(Matrix&);
	void release();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     Matrix* matrix_;
     Matrix* u_;
     Matrix* v_;
     Matrix* speed_;
     Matrix* direction_;
     
     friend class InputMatrixRegularInterpretor;

private:
    //! Copy constructor - No copy allowed
	InputMatrix(const InputMatrix&);
    //! Overloaded << operator to copy - No copy allowed
	InputMatrix& operator=(const InputMatrix&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputMatrix& p)
		{ p.print(s); return s; }

};
} // namespace magics



#endif
