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

/*! \file Akima761Method.h
    \brief Definition of the Template class Akima761Method.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 11-Mar-2004
    
    Changes:
    
*/

#ifndef Akima761Method_H
#define Akima761Method_H

#include "magics.h"

#include "ContourMethod.h"
#include "Akima761MethodAttributes.h"
#include "PointsHandler.h"

// N1, N2, and N3 are the numbers of the data points used 
// to determine the first-, second-, and third-degree 
// polynomials
#define N1_ 3
#define N2_ 6
#define N3_ 10

namespace magics {

template <class P>
class Akima761 : public MatrixHandler<P>
{
public :

    Akima761(const AbstractPoints<P>&, const Akima761Attributes&);
    ~Akima761(); 

    // Matrix operators
    double operator()(int  i, int  j) const;
    int  rows() const      //output number of rows
    {
	    return nrows_;
    }
    int  columns() const   //output number of columns
    { 
	    return ncols_;
    }
    
    double row(int i) const;     //latitude value
    double column(int j) const;  //longitude value

    double missing() const          //missing value
    { 
    	return this->matrix_.missing(); 
    }

    // Initialization
    int Init (int,double*,double*,double*,int NROW=6);

    // Scaterred-data bivariate interpolation
    int SDBI3P(double,double,double&) const;


private:
     
     Akima761Attributes attr_;
     Matrix   matrix_;    // output matrix
     int   nrows_;     // number of rows
     int   ncols_;     // number of columns
     double minX_,maxX_, // bounding box
	      minY_,maxY_;

     double SWTOL_; // tolerance

     int     MD_;    // mode of computation, 1 for new XD-YD (default)
     int     NDP_;   // number of data points
     int     NROW_;  // number of entries per triangle (6 or 9)
     int     NT_;    // number of triangles (maximum is 2*NDP-5)
     int     NL_;    // number of border line segments (maximum is NDP)
     double  *XD_;   // X coordinates of the data points
     double  *YD_;   // Y coordinates of the data points
     double  *ZD_;   // Z coordinates of the data points

     // Auxiliary variables
     int     *LIST_;  // set of nodal indexes used in the triangulation
     int     *LPTR_;  // set of pointers (LIST indexes)
     int     *LEND_;  // set of pointers to adjacency lists
     int     *LIST1_; // copy of LIST_ 
     int     *LPTR1_; // copy of LPTR_
     int     *LEND1_; // copy of LEND_

     int     *IPT_[3]; // point numbers of the vertexes
     int     *IPL_[2]; // point numbers of the end points of the 
                       // (IL)th border line segment

     int    **LTRI_;   // internal work area
     int     *ITL_;    // internal work area

     int     *IORD_;   // degree of the polynomial used to compute PDD (NDP)
     int     *IDSQ_;   // internal work area (NDP)
     int     *IPC_[9]; // internal work area (9*NDP)
     int     *NCP_;    // internal work area (NDP)

     double *PDD_[5];  // store estimated zx,zy,zxx,zxy,zyy values (5*NDP)
     double *CF3_[9];  // internal work area (9*NDP)
     double *CFL1_[2]; // internal work area (2*NDP)
     double *DSQ_;     // internal work area (NDP)

     // Member functions

     // Initialize working arrays
     int InitWorkingArrays();

     //---------------------------------
     //Terralib functions
     //---------------------------------

    // Triangulates the data area in the x-y plane with 
    // a scattered data point set
    int SDTRAN(int&,int&);

    // Basic triangulation in the convex hull of a scattered
    // data point set in a plane
    int SDTRCH(int&,int&);

    // Removal of thin triangles along the border line of triangulation
    void SDTRTT(int&,int&);

    // Partial derivatives for bivariate interpolation and surface
    // fitting for scattered data
    void SDPD3P();

    // Selects, at each of the data points, nine data points closest to it
    void SDCLDP();

    // Computes coefficients of the third-degree polynomial for z(x,y)
    void SDCF3P();

    // Solution of a set of linear equations
    void SDLEQN(int,double[N3_][N3_],double*,double*,double&,double&);

    // Least squares fit of a linear surface (plane) to z(x,y) values
    void SDLS1P();

    // Estimates first and second partial derivatives at node K
    int GRADC(int,int,int*,int,double&,double&,double&,double&,double&);

    // Sets up the I-th row of an augmented regression matrix for a 
    // weighted least squares fit of a cubic function f(x,y) to a set 
    // of data values z, where f(XK,YK) = ZK
    void SETRO3(double,double,double,double,double,double,double,double,double,double,double*);

    // Constructs the Givens plane rotation
    void GIVENS(double&,double&,double&,double&);

    // Applies the Givens rotation to the 2 by N matrix
    void ROTATE(int,double,double,double*,double*);

    // Locating points in a scattered data point set
    void SDLCTN(int,double*,double*,int*,int*) const;

    // Compute the z value in a scattered data point set
    void SDPLNL(int,double*,double*,int*,int*,double*) const;

    //------------------------------------
    // Tripack functions
    //------------------------------------

     // Creates a Delaunay triangulation
     int TRMESH (int&);

     // Determines whether node N0 is to the left or to the right 
     // of the line through N1-N2 as viewed by an observer at N1 
     // facing N2
     bool LEFT (double,double,double,double,double,double);

     // Updates the data structure with the addition of a new node 
     // in position K
     int ADDNOD (int,double,double,int,int,int*,int&,double*,double*,int*,int*,int*,int&);

     // Locates a point P relative to a triangulation created by 
     // subroutine TRMESH or TRMSHR
     void TRFIND (int,double,double,double*,double*,int*,int*,int*,int&,int&,int&);

     // Returns the index (LIST pointer) of NB in C the adjacency 
     // list for N0, where LPL = LEND(N0)
     int LSTPTR (int,int,int*,int*);

     // Check if a triangle lies in a constraint region
     bool CRTRI (int,int*,int,int,int);

     // Returns the index, if any, of an exterior constraint curve 
     int INDXCC (int,int*,int,int*,int*);

     // Adds a boundary node to a triangulation of a set of points 
     // in the plane
     void BDYADD (int,int,int,int*,int*,int*,int&);

     // Inserts a node as a neighbor of N1 following N2
     void INSERT (int,int,int*,int*,int&);

     // Adds an interior node to a triangulation of a set of points
     // in the plane
     void INTADD (int,int,int,int,int*,int*,int*,int&);

     // Replaces a diagonal arc in a strictly convex quadrilateral 
     // with the other diagonal
     void SWAP (int,int,int,int,int*,int*,int*,int&);

     // Applies the circumcircle test to a quadrilateral defined 
     // by a pair of adjacent triangles
     bool SWPTST (int,int,int,int,double*,double*);

     // Converts a triangulation data structure from the linked list
     // created by subroutine TRMESH or TRMSHR to a triangle list
     int TRLIST (int,int*,int,int*,int*,int*,int,int&,int*);

     // Sets NPTS(L) to the index of the next node in the sequence -- 
     // the node, other than NPTS(1),...,NPTS(L-1), which is closest 
     // to NPTS(1)
     int GETNP (int,int*,int,double*,double*,int*,int*,int*,int,int*,double*);

     // Given a pair of line segments P1-P2 and P3-P4, returns 'true'
     // if and only if P1-P2 shares one or more points with P3-P4
     bool INTSEC (double,double,double,double,double,double,double,double);

};



template <class P>
class Akima761Method: public ContourMethod<P>, public Akima761MethodAttributes {
public:
	Akima761Method() { MagLog::dev() << "Akima761Method::Akima761Method-->" << *this << "\n"; } 
	virtual ~Akima761Method() {}
    
    ContourMethod<P>* clone() const { 
    	Akima761Method<P>* method = new Akima761Method<P>(); 
    	method->copy(*this);
    	return method;
    }
    
    virtual void set(const map<string, string>& map) {
    	Akima761MethodAttributes::set(map);
    }
    
    virtual void set(const XmlNode& node) {
    	Akima761MethodAttributes::set(node);
    }
    virtual bool accept(const string& node) { return Akima761MethodAttributes::accept(node);; }
    virtual bool needPoints() { return true; }
	virtual MatrixHandler<P>* handlePoints(const AbstractPoints<P>& points, const Layout&) {
        return new Akima761<P>(points,*this);
	}
    virtual MatrixHandler<P>* handler(const AbstractMatrix&, const , const BasicGraphicsObjectContainer&) {
        return 0;
	}
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { 
         out << "Akima761Method[";
         Akima761Attributes::print(out);
         out << "]";
	 }  

private:
    //! Copy constructor - No copy allowed
	Akima761Method(const Akima761Method&); 
    //! Overloaded << operator to copy - No copy allowed
	Akima761Method& operator=(const Akima761Method&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Akima761Method<P>& p)
		{ p.print(s); return s; }

};

} // namespace magics

#include "Akima761.cc"
#include "Tripack.cc"

#endif
