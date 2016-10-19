/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima474Method.h
    \brief Definition of the Template class Akima474Method.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 14-Apr-2004 
    
    Changes:
    
*/

#ifndef Akima474Method_H
#define Akima474Method_H

#include "magics.h"

#include "ContourMethod.h"
#include "Akima474MethodAttributes.h"

namespace magics {


class Akima474 : public MatrixHandler
{
public : 

    Akima474(const AbstractMatrix& matrix, const Akima474MethodAttributes&);

    ~Akima474() {}; 
    
   
    double operator()(int  i, int  j) const;
/*
    double interpolate(double  i, double  j) const { 
    	return   mono_.interpolate(i,j); 
   }  
*/
    int  rows() const {
	    return nrows_;
   }
    int  columns() const { 
	    return ncols_;
    }
    
    double regular_row(int i) const;
    double regular_column(int j) const;
    
    double row(int , int) const;
    double column(int, int) const;

    double missing() const { 
    	return mono_.missing(); 
    }

#if 0 
     //Code from Spring. Does not work for non-regular spaced
     //interval. Remove this later.

    // Calculate a Z value using Bicubic interpolation
    double InterpolateBicubicAt(double lin, double col) const;

    // Calculate a Z value using Bilinear interpolation
    double InterpolateAt (double lin, double col) const;

    // Calculate the weighting coefficients
    bool Def_polynom(double x, double* y, double* p) const;
#endif

    //New code. If it works, remove the old code
    // Rectangular-grid bivariate interpolation
    double itplbv(double xi, double yi) const;

    // Check missing values
    int CheckMissingValues(int col, int lin) const;

//TEST, REMOVE LATER
//void test_build_data();
//void test_build_data_hl();

private:

     MonotonicIncreasingMatrixHandler mono_; //Akima needs indexes in the monotonic increasing order
    // CacheMatrixHandler mono_;
     Akima474MethodAttributes    attr_;
     int                   nrows_;         // number of rows
     int                   ncols_;         // number of columns
     bool                  missingValues_; // True: data has missing values
//   magvector<double>   rowsAxis_;
//   magvector<double>   columnsAxis_;

};




class Akima474Method: public ContourMethod, public Akima474MethodAttributes {
public:
	Akima474Method() { MagLog::dev() << "Akima474Method::Akima474Method-->" << *this << "\n"; } 
	
	virtual ~Akima474Method() {}
	
	ContourMethod* clone() const {
    	Akima474Method* method = new Akima474Method();
    	method->copy(*this);
    	return method;
    }

    
    virtual void set(const map<string, string>& map) {
    	Akima474MethodAttributes::set(map);
    }
    
    virtual void set(const XmlNode& node) {
    	Akima474MethodAttributes::set(node);
    }
    virtual bool accept(const string& node) { return Akima474MethodAttributes::accept(node);; }

    virtual MatrixHandler* handler(const AbstractMatrix& matrix, const BasicGraphicsObjectContainer&) {
        return new Akima474(matrix,*this);
	}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { 
         out << "Akima474Method[";
         Akima474MethodAttributes::print(out);
         out << "]";
	 }  

private:
    //! Copy constructor - No copy allowed
	Akima474Method(const Akima474Method&); 
    //! Overloaded << operator to copy - No copy allowed
	Akima474Method& operator=(const Akima474Method&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Akima474Method& p)
		{ p.print(s); return s; }

};

} // namespace magics



#endif
