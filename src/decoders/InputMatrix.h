/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
	double scale(double);
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
	void visit(MetaDataCollector&);

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
