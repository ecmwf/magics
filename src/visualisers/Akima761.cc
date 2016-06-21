/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima761.cc
    Implementation of Akima761 class.
    
    Magics Team - ECMWF 2004
   
    Created: Fri 12-Mar-2004
    
*/

#include "Akima761Method.h"
#include "MagLog.h"
#include "Timer.h"
#include "PointsHandler.h"

using namespace magics;
template <class P> 
class PPrint
{
public:
	PPrint() {}
	~PPrint() {}
	void operator()(const P& point) {
		MagLog::debug() << "[" << point.x() << ", " << point.y() << "]\n";
	}
	
};


template <class P>	
//Akima761<P>::Akima761(const  AbstractPoints<P>& points, const Akima761Attributes& attr) : 
Akima761<P>::Akima761(const  AbstractPoints<P>& points, const Akima761Attributes& ) : 
	MatrixHandler<P>(matrix_)
{
    	MagLog::debug() << "Akima761 Constructor" << "\n";
    	
    	PPrint<P> tool;
    	const_cast<AbstractPoints<P>*>(&points)->for_each(tool);

	// Calculate bounding box
	minX_ = points.minX();
	maxX_ = points.maxX();
//	minY_ = const_cast<AbstractPoints<P>*>(&points)->minY();
	minY_ = points.minY();
	maxY_ = points.maxY();
MagLog::dev()<< minX_ << " " << maxX_ << " " << minY_ << " " << maxY_ << endl;

        // Compute matrix output sizes
        ncols_ = int( ((maxX_ - minX_) / attr_.getResolutionX()) + 1 );
        nrows_ = int( ((maxY_ - minY_) / attr_.getResolutionY()) + 1 );

	// retrieve data 
	// IT IS DUPLICATING MEMORY ALLOCATION
	// PLEASE HAVE A LOOK IT LATER ????????????????
	int size = points.size();
MagLog::dev()<< "size=" << size << endl;
	double* xxd = new double[size];
	double* yyd = new double[size];
	double* val = new double[size];

	points.setToFirst();
	int il=0;
	while ( const_cast<AbstractPoints<P>*>(&points)->more() )
	{
		xxd[il] = points.current().x();
		yyd[il] = points.current().y();
		val[il] = points.current().value();
		points.advance();
		il++;
	}

for (int i=0;i<size;i++)
	MagLog::dev()<< xxd[i] << ",  " << yyd[i] << ",  " << val[i] << "," << endl;
	// Initialize input data
        Init(size,xxd,yyd,val);

	MagLog::dev()<< "END CONSTRUCTOR" << endl;
}

template <class P>
double Akima761<P>::row(int i) const
{
//Remove later. Why this function is called so many times ???
//static long itest=0;
//MagLog::debug() << "Akima760 row=" << itest++ << "\n";

	return (i*attr_.getResolutionY() + minY_);
}

template <class P>
double Akima761<P>::column(int j) const
{ 
//Remove later. Why this function is called so many times ???
//static long jtest=0;
//MagLog::debug() << "Akima760 column=" << jtest++ << "\n";

	return (j*attr_.getResolutionX() + minX_);
}

template <class P>	
double Akima761<P>::operator()(int  i, int  j) const
{
	double zi;

	SDBI3P(column(j),row(i),zi);

	return zi;
}

//======================================================
//Terralib
// This is the Akima761 algorithm converted from Fortran to C++ (C style)
//
// The range of the array indexes follows the Fortran syntax 
// language, eg., from 1:N . This is because the algorithm uses 
// the negative value of the indexes to distinguish between 
// interior and boundary nodes.
//
// Aditional files: tripack.cc
//

template <class P>
int Akima761<P>::Init (int ndp,double* xd,double* yd,double* zd,int nrow)
{
     printf("\nAkima761 Init\n");

     int i;

     // Variables initialization
     NDP_  = ndp;
     MD_   = 1;
     NROW_ = nrow;
     if (NDP_ <= 9) // number of data points must be 10 or greater
     {
	   printf("\n*** Error 1: NDP = 9 or less, NDP = %d",NDP_);
	   return -1;
     }

     // Memory allocation
     int NDP1  = NDP_+1;  //c++ arrays
     int NROW1 = NROW_+1; //c++ arrays
     LIST_  = new int [NDP1*6];
     LPTR_  = new int [NDP1*6];
     LEND_  = new int [NDP1];
     LIST1_ = new int [NDP1*6];
     LPTR1_ = new int [NDP1*6];
     LEND1_ = new int [NDP1];
     ITL_   = new int [NDP_];
     IORD_  = new int [NDP_];
     IDSQ_  = new int [NDP_];
     NCP_   = new int [NDP_];

     for (i = 0; i < 3; i++)
	     IPT_[i] = new int [2*NDP_];

     for (i = 0; i < 2; i++)
	     IPL_[i] = new int [NDP_];

     for (i = 0; i < 9; i++)
	     IPC_[i] = new int [NDP_];

     LTRI_ = new int* [NROW1];
     for (i = 0; i < NROW1; i++)
	     LTRI_[i] = new int [2*NDP1];

     XD_  = new double [NDP1];
     YD_  = new double [NDP1];
     ZD_  = new double [NDP1];
     DSQ_ = new double [NDP_];

     for (i = 0; i < 5; i++)
	     PDD_[i] = new double [NDP_];

     for (i = 0; i < 9; i++)
	     CF3_[i] = new double [NDP_];

     for (i = 0; i < 2; i++)
	     CFL1_[i] = new double [NDP_];

     // Copy x,y,z coordinates
     memcpy(&XD_[1],xd,NDP_*sizeof(double));
     memcpy(&YD_[1],yd,NDP_*sizeof(double));
     memcpy(&ZD_[1],zd,NDP_*sizeof(double));

     // Initialize working arrays
     return InitWorkingArrays();
}

template <class P>
Akima761<P>::~Akima761()
{
     printf("\nAkima761 Destructor\n");

     int i;

     // release memory
     delete [] LIST_;   LIST_  = NULL;
     delete [] LPTR_;   LPTR_  = NULL;
     delete [] LEND_;   LEND_  = NULL;
     delete [] LIST1_;  LIST1_ = NULL;
     delete [] LPTR1_;  LPTR1_ = NULL;
     delete [] LEND1_;  LEND1_ = NULL;
     delete [] ITL_;    ITL_   = NULL;
     delete [] IORD_;   IORD_  = NULL;
     delete [] IDSQ_;   IDSQ_  = NULL;
     delete [] NCP_;    NCP_   = NULL;

     for (i = 0; i < 3; i++)
     {
	     delete [] IPT_[i];
	     IPT_[i] = NULL;
     }
     for (i = 0; i < 2; i++)
     {
	     delete [] IPL_[i];
	     IPL_[i] = NULL;
     }

     for (i = 0; i < 9; i++)
     {
	     delete [] IPC_[i];
	     IPC_[i] = NULL;
     }

     int NROW1 = NROW_ + 1;
     for (i = 0; i < NROW1; i++)
     {
	     delete [] LTRI_[i];
	     LTRI_[i] = NULL;
     }
     delete [] LTRI_; LTRI_ = NULL;

     delete [] XD_;   XD_  = NULL;
     delete [] YD_;   YD_  = NULL;
     delete [] ZD_;   ZD_  = NULL;
     delete [] DSQ_;  DSQ_ = NULL;

     for (i = 0; i < 5; i++)
     {
	     delete [] PDD_[i];
	     PDD_[i] = NULL;
     }
     for (i = 0; i < 9; i++)
     {
	     delete [] CF3_[i];
	     CF3_[i] = NULL;
     }
     for (i = 0; i < 2; i++)
     {
	     delete [] CFL1_[i];
	     CFL1_[i] = NULL;
     }
}

template <class P>
int Akima761<P>:: InitWorkingArrays()
{
      double PDX,PDXX,PDXY,PDY,PDYY;
      int    K,L,LNEW;
      int    LCC[1];

      // This routine must be called only once
      if (MD_ != 1)
      {
	   printf("\nInternal error in Akima761::Init, MD_ must be 1\n");
	   return 9;
      }
      MD_=3;


//    CALL TRMESH(NDP,XD,YD,IWK(1,1),IWK(1,7),IWK(1,13),LNEW,IERT)
      if ( TRMESH(LNEW) < 0 )
      {
	   printf("\nError detected in TRMESH\n");
	   return 9;
      }

//IS THIS ICOPY NEEDED ? IF IWK(1,1) DOES NOT CHANGE INSIDE SDTRAN
// THEN WE CAN DEFINE A UNIQUE VARIABLE LIST (THE SAME FOR LPTR AND LEND)
      //* Copies triangulation data structure to IWK(1,26).
//    CALL ICOPY(LNEW-1,IWK(1,1),IWK(1,26))
//    CALL ICOPY(LNEW-1,IWK(1,7),IWK(1,32))
//    CALL ICOPY(NDP,IWK(1,13),IWK(1,38))
      int NDP1 = NDP_ + 1;
      memcpy(LIST1_,LIST_,LNEW*sizeof(int)); //LNEW points to the first empty location,
      memcpy(LPTR1_,LPTR_,LNEW*sizeof(int)); //in c++ these arrays are not using position 0
      memcpy(LEND1_,LEND_,NDP1*sizeof(int));

      //IERT = SDTRAN(NDP,XD,YD,NT,&IWK[0][0],NL,&IWK[0][6],&IWK[0][0],&IWK[0][6],&IWK[0][12],&IWK[0][13],&IWK[0][8]);
      if (SDTRAN(NT_,NL_) > 0)
      {
	  printf("\nError detected in SDTRAN called by SDBI3P\n");
	  return 9;
      }

      //* Estimates partial derivatives at all data points
      // SDPD3P(NDP,XD,YD,ZD,WK(1,1),WK(1,6),WK(1,15),WK(1,17),IWK(1,9),IWK(1,10),IWK(1,19),IWK(1,39)      
      SDPD3P();

#if 0
MagLog::dev()<< " PDD" << endl;
for (K= 0; K < NDP_; K++)
	MagLog::dev()<< K << " " << PDD_[0][K] << " " << PDD_[1][K] << " " <<PDD_[2][K] << " " <<PDD_[3][K] << " " <<PDD_[4][K] << endl;

MagLog::dev()<< "IORD" << endl;
for (K= 0; K < NDP_; K++)
	MagLog::dev()<< K << " " << IORD_[K] << endl;
#endif

      //* If non-cubic order at node, replace with cubic from GRADC
      L = 0;
      for (K = 0; K < NDP_; K++)
      {
	   if (IORD_[K] < 3)
	   {
		   if ( GRADC(K+1,0,LCC,NDP_,PDX,PDY,PDXX,PDXY,PDYY) >= 0) //c++
		   {
		       //J = L/NDP;
		       //I = L-NDP*J;
		       //J = J + 1;
		       //WK[I+1,...,I+5][J] = ...
		       PDD_[0][K] = PDX;
		       PDD_[1][K] = PDY;
		       PDD_[2][K] = PDXX;
		       PDD_[3][K] = PDXY;
		       PDD_[4][K] = PDYY;
		   }
	     }
//	     L = L + 5;
      }

MagLog::dev()<< " PDD FINAL" << endl;
for (K= 0; K < NDP_; K++)
	MagLog::dev()<< K << " " << PDD_[0][K] << " " << PDD_[1][K] << " " <<PDD_[2][K] << " " <<PDD_[3][K] << " " <<PDD_[4][K] << endl;

      return 0;
}

// SDBI3P routine
//*********************************************************************
//* Scattered-data bivariate interpolation
//* (a master subroutine of the SDBI3P/SDSF3P subroutine package)
//*
//* Hiroshi Akima
//* U.S. Department of Commerce, NTIA/ITS
//* Version of 1995/05
//*
//* This subroutine performs bivariate interpolation when the data
//* points are scattered in the x-y plane.  It is based on the
//* revised Akima method that has the accuracy of a cubic (third-
//* degree) polynomial.
//*
//* The input arguments are
//*   MD  = mode of computation
//*       = 1 for new XD-YD (default)
//*       = 2 for old XD-YD, new ZD
//*       = 3 for old XD-YD, old ZD,
//*   NDP = number of data points (must be 10 or greater),
//*   XD  = array of dimension NDP containing the x coordinates
//*         of the data points,
//*   YD  = array of dimension NDP containing the y coordinates
//*         of the data points,
//*   ZD  = array of dimension NDP containing the z values at
//*         the data points,
//*   NIP = number of output points at which interpolation is
//*         to be performed (must be 1 or greater),
//*   XI  = array of dimension NIP containing the x coordinates
//*         of the output points,
//*   YI  = array of dimension NIP containing the y coordinates
//*         of the output points.
//*
//* The output arguments are
//*   ZI  = array of dimension NIP, where interpolated z values
//*         are to be stored,
//*   IER = error flag
//*       = 0 for no errors
//*       = 1 for NDP = 9 or less
//*       = 2 for NDP not equal to NDPPV
//*       = 3 for NIP = 0 or less
//*       = 9 for errors in SDTRAN called by this subroutine.
//*
//* The other arguments are
//*   WK  = two-dimensional array of dimension NDP*17 used
//*         internally as a work area,
//*   IWK = two-dimensional integer array of dimension NDP*39
//*         used internally as a work area.
//*
//* The very first call to this subroutine and the call with a new
//* NDP value or new XD and YD arrays must be made with MD=1.  The
//* call with MD=2 must be preceded by another call with the same
//* NDP value and same XD and YD arrays.  The call with MD=3 must
//* be preceded by another call with the same NDP value and same
//* XD, YD, and ZD arrays.  Between the call with MD=2 and its
//* preceding call, the IWK array must not be disturbed.  Between
//* the call with MD=3 and its preceding call, the WK and IWK
//* arrays must not be disturbed.
//*
//* The constant in the PARAMETER statement below is
//*   NIPIMX = maximum number of output points to be processed
//*            at a time.
//* The constant value has been selected empirically.
//*
//* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL
//* subroutines.
//*
//* Comments added to Remark:
//*
//* It also calls TRMESH from the TRIPACK package of ACM Algorithm
//* 751 by R. J. Renka.  The TRMESH subroutine in turn calls either
//* directly or indirectly 12 other subprograms included in the
//* package.  In addition, a newly added routine, GRADC, is called
//* to compute partial derivatives at those nodes for which the
//* cubic fit failed due to ill-conditioning.
//*
//*******************************************************************
//SDBI3P(MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IER,WK,IWK)
template <class P>
int Akima761<P>::SDBI3P(double XI, double YI, double& ZI) const
{
      int ITLI,KTLI;

      // Data structure should be initialized previously
      if (MD_ == 1) return -1;

      //* Locates point at which interpolation is to be performed
      //* and interpolates the ZI value
      SDLCTN(1,&XI,&YI,&KTLI,&ITLI);

      SDPLNL(1,&XI,&YI,&KTLI,&ITLI,&ZI);

      return 0;
}

// SDTRAN routine
/*******************************************************************
* Triangulation of the data area in a plane with a scattered data
* point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area in the x-y plane with
* a scattered data point set.  It divides the data area into a
* number of triangles and determines line segments that form the
* border of the data area.
*
* This subroutine consists of the following two steps, i.e.,
* (1) basic triangulation in the convex hull of the data points,
* and (2) removal of thin triangles along the border line of the
* data area.  It calls the SDTRCH and SDTRTT subroutines, that
* correspond to Steps (1) and (2), respectively.
*
* The SDTRCH subroutine depends on the TRIPACK package of ACM
* Algorithm XXX by R. J. Renka.  It calls the TRLIST subroutine
* included in the package.
*
* The input arguments are
*   NDP  = number of data points (must be greater than 3),
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*   LIST = integer array of dimension 6*NDP returned by TRMESH.
*   LPTR = integer array of dimension 6*NDP returned by TRMESH.
*   LEND = integer array of dimension NDP returned by TRMESH.
*
* The output arguments are
*   NT   = number of triangles (its maximum is 2*NDP-5),
*   IPT  = two-dimensional integer array of dimension
*          (3,NT), where the point numbers of the vertexes
*          of the ITth triangle are to be stored counter-
*          clockwise in the ITth column, where IT = 1, 2,
*          ..., NT,
*   NL   = number of border line segments (its maximum is
*          NDP),
*   IPL  = two-dimensional integer array of dimension
*          (2,NL), where the point numbers of the end
*          points of the (IL)th border line segment are to
*          be stored counterclockwise in the ILth column,
*          where IL = 1, 2, ..., NL, with the line segments
*          stored counterclockwise,
*   IERT = error flag
*        = 0 for no errors
*        = 1 for NDP = 3 or less
*        = 2 for identical data points
*        = 3 for all collinear data points.
*
* The other arguments are
*   LTRI = two-dimensional integer array of dimension 12*NDP
*          used internally as a work area.
*   ITL  = integer array of dimension NDP used internally as
*          a work area.
*******************************************************************/
//SUBROUTINE SDTRAN(NDP,XD,YD,NT,IPT,NL,IPL,IERT,LIST,LPTR,LEND,LTRI,ITL)

template <class P>
int Akima761<P>::SDTRAN(int& NT,int& NL)
{
      int IERTL;

      // Basic triangulation
      IERTL = SDTRCH(NT,NL);
      if (IERTL == 0)
      {
	    // Removal of thin triangles that share border line segments
	      SDTRTT(NT,NL);
      }
      else  //Error exit
      {
	    if (IERTL == 1)
	    {
		 IERTL = 4;
		 printf("\n*** SDTRAN Error 4: NDP outside its valid range: %d\n",NDP_);
	    }
	    else if (IERTL == 2)
	    {
		 IERTL = 5;
		 printf("\n*** SDTRAN Error 5:Invalid data structure (LIST,LPTR,LEND)\n");
	    }
      }

#if 0
MagLog::dev()<< "NT=" << NT << endl;
int I;
for (I=0;I<2*NDP_;I++)
	MagLog::dev()<< I << ' ' << IPT_[0][I] << " " << IPT_[1][I] << " " << IPT_[2][I] << endl;

MagLog::dev()<< "NL=" << NL << endl;
for (I=0;I<NL;I++)
	MagLog::dev()<< I << ' ' << IPL_[0][I] << " " << IPL_[1][I] << endl;
#endif

      return IERTL;
}

// SDTRCH routine
/*******************************************************************
* Basic triangulation in the convex hull of a scattered data point
* set in a plane
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area that is a convex hull
* of the scattered data points in the x-y plane.  It divides the
* data area into a number of triangles and determines line segments
* that form the border of the data area.
*
* This subroutine depends on the TRIPACK package of ACM Algorithm
* 751 by R. J. Renka.  It calls the TRLIST subroutine included in
* the package.
*
* The input arguments are
*   NDP   = number of data points (must be greater than 3),
*   LIST = integer array of dimension 6*NDP returned by TRMESH.
*   LPTR = integer array of dimension 6*NDP returned by TRMESH.
*   LEND = integer array of dimension NDP returned by TRMESH.
*
* The output arguments are
*   NT    = number of triangles (its maximum is 2*NDP-5),
*   IPT   = two-dimensional integer array of dimension
*           (3,NT), where the point numbers of the vertexes
*           of the ITth triangle are to be stored counter-
*           clockwise in the ITth column, where IT = 1, 2,
*           ..., NT,
*   NL    = number of border line segments (its maximum is
*           NDP),
*   IPL   = two-dimensional integer array of dimension
*           (2,NL), where the point numbers of the end
*           points of the (IL)th border line segment are to
*           be stored counterclockwise in the ILth column,
*           where IL = 1, 2, ..., NL, with the line segments
*           stored counterclockwise,
*   IERTL = error flag from the TRLIST subroutine,
*         = 0 for no errors
*         = 1 for invalid NCC, NDP, or NROW value.
*         = 2 for invalid data structure (LIST,LPTR,LEND).
*
* The other arguments are
*   LTRI  = two-dimensional integer array of dimension 12*NDP
*           used internally as a work area.
*******************************************************************/
// SUBROUTINE SDTRCH(NDP,NT,IPT,NL,IPL,IERTL,LIST,LPTR,LEND,LTRI)
template <class P>
int Akima761<P>::SDTRCH(int& NT,int& NL)
{
#define NCCF 0

      int I,I1,I2,IL,IL1,IL2,IPL11,IPL21,J,IERTL;
      int LCC[1],LCT[1];

      // Performs basic triangulation
      // CALL TRLIST(NCC,LCC,NDP,LIST,LPTR,LEND,NROW,NT,LTRI,LCT,IERTL)
      IERTL = TRLIST(NCCF,LCC,NDP_,LIST_,LPTR_,LEND_,NROW_,NT,LCT);
      if (IERTL != 0) return IERTL;

//for (J=0;J<2*(NDP_+1);J++)
//{
//  MagLog::dev()<< J << " ";
//for (I=0;I<NROW_+1;I++)
//	  MagLog::dev()<< LTRI_[I][J] << " ";
//  MagLog::dev()<< endl;
//}

      // Extracts the triangle data from the LTRI array and set the IPT array
      for (J = 0; J < NT; J++)
      {
	   for (I = 0; I < 3; I++)
		   IPT_[I][J] = LTRI_[I+1][J+1];  //????
      }

      // Extracts the border-line-segment data from the LTRI array and
      // set the IPL array
      IL = -1; //c++
      for (J = 1; J <= NT; J++) //???
      {
	   for (I = 1; I <= 3; I++) //???
	   {
		  if (LTRI_[I+3][J] <= 0)
		  {
			  IL = IL + 1;
			  //I1 = MOD(I,3) + 1;
			  //I2 = MOD(I+1,3) + 1
			  I1 = I%3 + 1;
			  I2 = (I+1)%3 + 1;
			  IPL_[0][IL] = LTRI_[I1][J];
			  IPL_[1][IL] = LTRI_[I2][J];
		  }
	   }
      }
      NL = IL+1; //???

      // Sorts the IPL array
      for (IL1 = 0; IL1 < NL - 1; IL1++)
      {
	  for (IL2 = IL1 + 1; IL2 < NL; IL2++)
		  if (IPL_[0][IL2] == IPL_[1][IL1]) break;

          IPL11 = IPL_[0][IL1+1];
          IPL21 = IPL_[1][IL1+1];
          IPL_[0][IL1+1] = IPL_[0][IL2];
          IPL_[1][IL1+1] = IPL_[1][IL2];
          IPL_[0][IL2] = IPL11;
          IPL_[1][IL2] = IPL21;
      }
//MagLog::dev()<< "IL=" << IL << endl;
//for (I=0;I<IL+1;I++)
//	MagLog::dev()<< I << ' ' << IPL_[0][I] << " " << IPL_[1][I] << endl;


      return 0;
}

// SDTRTT routine
/*********************************************************************
* Removal of thin triangles along the border line of triangulation
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine removes thin triangles along the border line of
* triangulation.
*
* The input arguments are
*   NDP = number of data points (must be greater than 3),
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points.
*
* The input and output arguments are
*   NT  = number of triangles (its maximum is 2*NDP-5),
*   IPT = two-dimensional integer array of dimension
*         (3,NT), where the point numbers of the vertexes
*         of the ITth triangle are to be stored counter-
*         clockwise in the ITth column, where IT = 1, 2,
*         ..., NT,
*   NL  = number of border line segments (its maximum is
*         NDP),
*   IPL = two-dimensional integer array of dimension
*         (2,NL), where the point numbers of the end
*         points of the (IL)th border line segment are to
*         be stored counterclockwise in the ILth column,
*         where IL = 1, 2, ..., NL, with the line segments
*         stored counterclockwise.
*
* The other argument is
*   ITL = integer array of dimension NDP used internally as
*         a work area.
*
* The constants in the PARAMETER statement below are
*   HBRMN = minimum value of the height-to-bottom ratio of a
*           triangle along the borde line of the data area,
*   NRRTT = number of repetitions in thin triangle removal.
* The constant values have been selected empirically.
***************************************************************/
//SUBROUTINE SDTRTT(NDP,XD,YD,NT,IPT,NL,IPL,ITL)
template <class P>
void Akima761<P>::SDTRTT(int& NT,int& NL)
{
#define HBRMN 0.10
#define NRRTT 5
#define DSQF(U1,V1,U2,V2,U3,V3) (pow(((U2-U1)/U3),2.) + pow(((V2-V1)/V3),2.))
#define VPDT1(U1,V1,U2,V2,U3,V3,U4,V4) (((V3-V1)/V4)* ((U2-U1)/U4) - ((U3-U1)/U4)* ((V2-V1)/V4))

      double DXA,DYA,HBR;
      int    IL,IL0,IL00,IL1,ILP1,ILR1,IP1,IP2,IP3,IPL1,IPL2,
	     IREP,IT,IT0,IV,IVP1,MODIF,NL0;

      // Triangle numbers of triangles that share line segments with the
      // border line
      for (IL = 0; IL < NL; IL++)
      {
	  IPL1 = IPL_[0][IL];
          IPL2 = IPL_[1][IL];
          for (IT = 0; IT < NT; IT++)
	  {
              if (IPL1 == IPT_[0][IT] || IPL1 == IPT_[1][IT] || IPL1 == IPT_[2][IT])
	      {
                  if (IPL2 == IPT_[0][IT] || IPL2 == IPT_[1][IT] || IPL2 == IPT_[2][IT])
		  {
			  ITL_[IL] = IT;
			  break;
		  }
	      }
	  }
      }

#if 0
int I;
MagLog::dev()<< "NT=" << NT << endl;
for (I=0;I<2*NDP_;I++)
	MagLog::dev()<< I << ' ' << IPT_[0][I] << " " << IPT_[1][I] << " " << IPT_[2][I] << endl;
MagLog::dev()<< "NL=" << NL << endl;
for (I=0;I<NDP_;I++)
	MagLog::dev()<< I << ' ' << IPL_[0][I] << " " << IPL_[1][I] << endl;
MagLog::dev()<< "ITL" << endl;
for (I=0;I<NDP_;I++)
	MagLog::dev()<< I << ' ' << ITL_[I] << endl;
#endif

      // Average delta x and y for boundary line segments
      DXA = 0.0;
      DYA = 0.0;
      for (IL = 0; IL < NL; IL++)
      {
	      IP1 = IPL_[0][IL];
	      IP2 = IPL_[1][IL];
	      DXA = DXA + fabs(XD_[IP1]-XD_[IP2]);
	      DYA = DYA + fabs(YD_[IP1]-YD_[IP2]);
      }
      DXA = DXA/double(NL);
      DYA = DYA/double(NL);

      // Removes thin triangles that share line segments with the border line
      for (IREP = 1; IREP <= NRRTT; IREP++)
      {
	  MODIF = 0;
          NL0 = NL;
          IL = -1; //c++
          for (IL0 = 1; IL0 <= NL0; IL0++)
	  {
	      IL = IL + 1;
              IP1 = IPL_[0][IL];
              IP2 = IPL_[1][IL];
              IT = ITL_[IL];

	      // Calculates the height-to-bottom ratio of the triangle
              if (IPT_[0][IT] != IP1 && IPT_[0][IT] != IP2)
		  IP3 = IPT_[0][IT];
              else if (IPT_[1][IT] != IP1 && IPT_[1][IT] != IP2)
		  IP3 = IPT_[1][IT];
              else
		  IP3 = IPT_[2][IT];

              HBR = VPDT1(XD_[IP1],YD_[IP1],XD_[IP2],YD_[IP2],XD_[IP3],YD_[IP3],DXA,DYA) / DSQF(XD_[IP1],YD_[IP1],XD_[IP2],YD_[IP2],DXA,DYA);

              if (HBR >= HBRMN) continue; //for(IL0)

              MODIF = 1;

	      // Removes this triangle when applicable
              for (IT0 = IT+1; IT0 < NT; IT0++)
	      {
		    IPT_[0][IT0-1] = IPT_[0][IT0];
		    IPT_[1][IT0-1] = IPT_[1][IT0];
		    IPT_[2][IT0-1] = IPT_[2][IT0];
	      }

              NT = NT - 1;
              for (IL00 = 0; IL00 < NL; IL00++)
		    if (ITL_[IL00] > IT) ITL_[IL00]--;

	      // Replaces the border line segment with two new line segments
              if (IL < NL-1) //c++
	      {
		    ILP1 = IL + 1;
                    for (ILR1 = ILP1; ILR1 < NL; ILR1++)
		    {
			    IL1 = NL + ILP1 - ILR1 -1; //c++
                          IPL_[0][IL1+1] = IPL_[0][IL1];
                          IPL_[1][IL1+1] = IPL_[1][IL1];
                          ITL_[IL1+1] = ITL_[IL1];
		    }
	      }

	      // Adds the first new line segment
              IPL_[0][IL] = IP1;
	      IPL_[1][IL] = IP3;
	      bool flag = false;
              for (IT0 = 0; IT0 < NT; IT0++)
	      {
		    for (IV = 0; IV < 3; IV++)
		    {    
                         if (IPT_[IV][IT0] == IP1 || IPT_[IV][IT0] == IP3)
			 {
			      IVP1 = (IV+1)%3; //???
                              if (IPT_[IVP1][IT0] == IP1 || IPT_[IVP1][IT0] == IP3)
			      {
				     flag = true;
				     break;
			      }
			  }
		    }
		    if (flag) break;
	      }

	      ITL_[IL] = IT0;

	      // Adds the second new line segment
              IL = IL + 1;
              IPL_[0][IL] = IP3;
	      IPL_[1][IL] = IP2;
	      flag = false;
              for (IT0 = 0; IT0 < NT; IT0++)
	      { 
		   for (IV = 0; IV < 3; IV++)
		   {
                        if (IPT_[IV][IT0] == IP3 || IPT_[IV][IT0] == IP2)
			{
			      IVP1 = (IV+1)%3;
                              if (IPT_[IVP1][IT0] == IP3 || IPT_[IVP1][IT0] == IP2)
			      {
				    flag = true;
				    break;
			      }
			 }
		    }
		    if (flag) break;
	      }

              ITL_[IL] = IT0;
              NL++;

#if 0
MagLog::dev()<< "NT=" << NT << endl;
for (I=0;I<2*NDP_;I++)
	MagLog::dev()<< I << ' ' << IPT_[0][I] << " " << IPT_[1][I] << " " << IPT_[2][I] << endl;
MagLog::dev()<< "NL=" << NL << endl;
for (I=0;I<NDP_;I++)
	MagLog::dev()<< I << ' ' << IPL_[0][I] << " " << IPL_[1][I] << endl;
MagLog::dev()<< "ITL" << endl;
for (I=0;I<NDP_;I++)
	MagLog::dev()<< I << ' ' << ITL_[I] << endl;
#endif

          } //for(IL0)

          if (MODIF == 0) return;

      } //for(IREP)

      return;
}

// SDPD3P routine
/*****************************************************************
* Partial derivatives for bivariate interpolation and surface
* fitting for scattered data
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine estimates partial derivatives of the first and
* second orders at the data points for bivariate interpolation
* and surface fitting for scattered data.  In most cases, this
* subroutine has the accuracy of a cubic (third-degree)
* polynomial.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z values
*          at the data points.
*
* The output arguments are
*   PDD  = two-dimensional array of dimension 5*NDP, where
*          the estimated zx, zy, zxx, zxy, and zyy values
*          at the IDPth data point are to be stored in the
*          IDPth row, where IDP = 1, 2, ..., NDP.
*   IORD = integer array of dimension NDP containing the
*          degree of the polynomial used to compute PDD.
*
* The other arguments are
*   CF3  = two-dimensional array of dimension 9*NDP used
*          internally as a work area,
*   CFL1 = two-dimensional array of dimension 2*NDP used
*          internally as a work area,
*   DSQ  = array of dimension NDP used internally as a work
*          area,
*   IDSQ = integer array of dimension NDP used internally
*          as a work area,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          used internally as a work area,
*   NCP  = integer array of dimension NDP used internally
*          as a work area.
*
* The constant in the first PARAMETER statement below is
*   NPEMX = maximum number of primary estimates.
* The constant value has been selected empirically.
*
* The constants in the second PARAMETER statement below are
*   NPEAMN = minimum number of primary estimates,
*   NPEAMX = maximum number of primary estimates when
*            additional primary estimates are added.
* The constant values have been selected empirically.
*
* This subroutine calls the SDCLDP, SDCF3P, and SDLS1P
* subroutines.
*
*****************************************************************/
//SUBROUTINE SDPD3P(NDP,XD,YD,ZD,PDD,CF3,CFL1,DSQ,IDSQ,IPC,NCP,IORD)

template <class P>
void Akima761<P>::SDPD3P()
{
#define NPEMX 25
#define NPEAMN 3
#define NPEAMX 6

      bool   flag1,flag2;
      double A01,A02,A03,A10,A11,A12,A20,A21,A30,ALPWT,ANPE,
	     ANPEM1,SMWTF,SMWTI,WTF,WTI,X,Y,ZX,ZY;
      int    IDP1,IDP2,IDPI,IDPPE1,IMN,IPE,IPE1,J,J1,J2,JJ,
	     JMN,K,NCP2,NCP2P1,NPE;

      double AMPDPE[5],PDDIF[5],PDDII[5],PDPE[5][NPEMX],
	     PWT[NPEMX],RVWT[NPEMX],SSPDPE[5];
      int    IDPPE[NPEMX],IPCPE[10][NPEMX];

      // Selects, at each of the data points, nine data points closest
      // to the data point in question
      SDCLDP();

#if 0
MagLog::dev()<< "IPC" << endl;
int i,j;
for (i=0;i<NDP_;i++)
{
	MagLog::dev()<< i << " ";
	for(j=0;j<9;j++)
		    MagLog::dev()<< IPC_[j][i] << " ";
     MagLog::dev()<< endl;
 }
MagLog::dev()<< "IDSQ" << endl;
for (i=0;i<NDP_;i++)
	MagLog::dev()<< i << " " << IDSQ_[i] << endl;
MagLog::dev()<< "DSQ" << endl;
for (i=0;i<NDP_;i++)
	MagLog::dev()<< i << " " << DSQ_[i] << endl;
#endif

      // Fits, at each of the data points, a cubic (third-degree)
      // polynomial to z values at the 10 data points that consist of
      // the data point in question and 9 data points closest to it
      SDCF3P();

#if 0
MagLog::dev()<< "CF3" << endl;
for (i=0;i<NDP_;i++)
{	
	MagLog::dev()<< i << " ";
	for(j=0;j<9;j++)
		    MagLog::dev()<< CF3_[j][i] << " ";
     MagLog::dev()<< endl;
 }
 MagLog::dev()<< "NCP" << endl;
for (i=0;i<NDP_;i++)
	MagLog::dev()<< i << " " << NCP_[i] << endl;
 MagLog::dev()<< "IORD" << endl;
for (i=0;i<NDP_;i++)
	MagLog::dev()<< i << " " << IORD_[i] << endl;
#endif

     // Performs, at each of the data points, the least-squares fit of
      // a plane to z values at the 10 data points.
      SDLS1P();

#if 0
 MagLog::dev()<< "CFL1" << endl;
for (i=0;i<NDP_;i++)
	MagLog::dev()<< i << " " << CFL1_[0][i] << " " << CFL1_[1][i] <<endl;
#endif

      // Outermost DO-loop with respect to the data point
      for (IDP1 = 0; IDP1 < NDP_; IDP1++)
      {
	  // Selects data point sets for sets of primary estimates
	  // of partial derivatives
	  // Selects a candidate
	  NPE = -1; //c++
          for (IDP2 = 0; IDP2 < NDP_; IDP2++)
	  {
	      NCP2 = NCP_[IDP2];
              NCP2P1 = NCP2 + 1;
              if (IDP2 != IDP1)
	      {
		  bool found=false;
		  for (J = 0; J < NCP2; J++)
		  {
		       if(IPC_[J][IDP2] == IDP1)
		       {
			    found = true;
			    break;
		       }
		  }
		  if ( !found ) continue; //for(IDP2)
	      }

              IPCPE[0][NPE+1] = IDP2;
              for (J = 0; J < NCP2; J++)
		   IPCPE[J+1][NPE+1] = IPC_[J][IDP2];

              for (J1 = 0; J1 < NCP2; J1++)
	      {
		  JMN = J1;
                  IMN = IPCPE[JMN][NPE+1];
                  for (J2 = J1; J2 < NCP2P1; J2++)
		  {
                      if (IPCPE[J2][NPE+1] < IMN)
		      {
			  JMN = J2;
                          IMN = IPCPE[JMN][NPE+1];
		      }
		  }
                  IPCPE[JMN][NPE+1] = IPCPE[J1][NPE+1];
                  IPCPE[J1][NPE+1] = IMN;
	      }

	      // Checks whether or not the candidate has already been included
	      flag2 = false;
	      if (NPE >= 0) //c++
	      {
		   for (IPE1 = 0; IPE1 <= NPE; IPE1++)
		   {
		      IDPPE1 = IDPPE[IPE1];
                      if (NCP2 != NCP_[IDPPE1]) continue;

		      flag1 = false;
		      for (J = 0; J < NCP2P1; J++)
		      {
			   if (IPCPE[J][NPE+1] != IPCPE[J][IPE1])
			   {
				flag1 = true;
				break; //for(J)
			   }
		      }
		      if (flag1) continue; //for(IPE1)

		      flag2 = true;
		      break; //for(IPE1)
		   } //for(IPE1)
              }

	      if (flag2) continue; //for(IDP2)

              NPE = NPE + 1;
              IDPPE[NPE] = IDP2;
	      if (NPE >= NPEMX-1) break;  //c++
	  } //for(IDP2) 80

#if 0
 MagLog::dev()<< "IPCPE" << endl;
for (i=0;i<25;i++)
{
	MagLog::dev()<< i << " ";
	for (j=0;j<10;j++)
		     MagLog::dev()<< IPCPE[j][i] << " ";
	MagLog::dev()<< endl;
}
 MagLog::dev()<< "IDPPE" << endl;
for (i=0;i<NPEMX;i++)
	MagLog::dev()<< i << " " << IDPPE[i] << endl;
#endif

	  // Adds additional closest data points when necessary
	  if (NPE < NPEAMN-1) //c++
	  {
	       for (JJ = 0; JJ < 9; JJ++)
	       {
		  IDP2 = IPC_[JJ][IDP1];
                  NCP2 = NCP_[IDP2];
                  NCP2P1 = NCP2 + 1;
		  IPCPE[0][NPE+1] = IDP2;
                  for (J = 0; J < NCP2; J++)
		       IPCPE[J+1][NPE+1] = IPC_[J][IDP2];

                  for (J1 = 0; J1 < NCP2; J1++)
		  {
		      JMN = J1;
                      IMN = IPCPE[JMN][NPE+1];
                      for (J2 = J1; J2 < NCP2P1; J2++)
		      {
                          if (IPCPE[J2][NPE+1] < IMN)
			  {
			      JMN = J2;
                              IMN = IPCPE[JMN][NPE+1];
			  }
		      }
                      IPCPE[JMN][NPE+1] = IPCPE[J1][NPE+1];
                      IPCPE[J1][NPE+1] = IMN;
		  } //for(J1) 120

		  flag2 = false;
		  if (NPE >= 0) //c++
		  {
		      for (IPE1 = 0; IPE1 <= NPE; IPE1++)
		      {
			  IDPPE1 = IDPPE[IPE1];
                          if (NCP2 != NCP_[IDPPE1]) continue;

			  flag1 = false;
                          for (J = 0; J < NCP2P1; J++)
			  {
                              if (IPCPE[J][NPE+1] != IPCPE[J][IPE1])
			      {
				      flag1 = true;
				      break;
			      }
			  }
			  if (flag1) continue; //for(IPE1)

			  flag2 = true;
			  break; //for(IPE1)
		      } //for(IPE1) 140
                  }
		  if (flag2) continue; //for(JJ)

                  NPE = NPE + 1;
                  IDPPE[NPE] = IDP2;
		  if (NPE >= NPEAMX-1) break; //c++
	       } //for(JJ) 150
	  }

	  //Calculates the primary estimates of partial derivatives
	  X = XD_[IDP1+1]; //c++
          Y = YD_[IDP1+1];
          for (IPE = 0; IPE <= NPE; IPE++)
	  {
	      IDPI = IDPPE[IPE];
              A10 = CF3_[0][IDPI];
              A20 = CF3_[1][IDPI];
              A30 = CF3_[2][IDPI];
              A01 = CF3_[3][IDPI];
              A11 = CF3_[4][IDPI];
              A21 = CF3_[5][IDPI];
              A02 = CF3_[6][IDPI];
              A12 = CF3_[7][IDPI];
              A03 = CF3_[8][IDPI];
              PDPE[0][IPE] = A10 + X* (2.0*A20+X*3.0*A30) + Y* (A11+2.0*A21*X+A12*Y);
              PDPE[1][IPE] = A01 + Y* (2.0*A02+Y*3.0*A03) + X* (A11+2.0*A12*Y+A21*X);
              PDPE[2][IPE] = 2.0*A20 + 6.0*A30*X + 2.0*A21*Y;
              PDPE[3][IPE] = A11 + 2.0*A21*X + 2.0*A12*Y;
              PDPE[4][IPE] = 2.0*A02 + 6.0*A03*Y + 2.0*A12*X;
	  }

#if 0
MagLog::dev()<< "PDPE" << endl;
for (i=0;i<=NPE;i++)
	MagLog::dev()<< i << " " << PDPE[0][i] << " " << PDPE[1][i] << " "<< PDPE[2][i] << " "<< PDPE[3][i] << " "<< PDPE[4][i] << endl;
#endif

          if (NPE == 0) //c++
	  {
	      // Only one qualified point set
	      for (K = 0; K < 5; K++)
		      PDD_[K][IDP1] = PDPE[K][0];

	      continue; //for(IDP1)
	  }

	  //Weighted values of partial derivatives
	  // Calculates the probability weight
	  ANPE = double(NPE) + 1.; //c++
          ANPEM1 = double(NPE);    //c++
          for (K = 0; K < 5; K++)
	  {
	      AMPDPE[K] = 0.0;
	      //*DELETED from Valtulina  SSPDPE(K) = 0.0
	      for (IPE = 0; IPE <= NPE; IPE++)
		      AMPDPE[K] = AMPDPE[K] + PDPE[K][IPE];

	      //*DELETED from Valtulina  SSPDPE(K) = SSPDPE(K) + PDPE(K,IPE)**2
	      AMPDPE[K] = AMPDPE[K]/ANPE;
	      //*DELETED from Valtulina  SSPDPE(K) = (SSPDPE(K)-ANPE*AMPDPE(K)**2)/ANPEM1
	  }

#if 0
MagLog::dev()<< "AMPDPE" << endl;
for (i=0;i<5;i++)
	MagLog::dev()<< i << " " << AMPDPE[i] << endl;
#endif

	  //* ADDED from Valtulina
	  //* Calculates the unbiased estimate of variance
	  for (K = 0; K < 5; K++)
	  {
	      SSPDPE[K] = 0.0;
	      for (IPE = 0; IPE <= NPE; IPE++)
		      SSPDPE[K] = SSPDPE[K] + pow(PDPE[K][IPE]-AMPDPE[K],2.);

              SSPDPE[K] = SSPDPE[K]/ANPEM1;
	  }

#if 0
MagLog::dev()<< "SSPDPE" << endl;
for (i=0;i<5;i++)
	MagLog::dev()<< i << " " << SSPDPE[i] << endl;
#endif

	  for (IPE = 0; IPE <= NPE; IPE++) //c++
	  {
	      ALPWT = 0.0;
              for (K = 0; K < 5; K++)
	      {
                  if (SSPDPE[K] != 0.0)
			  ALPWT = ALPWT + pow(PDPE[K][IPE]-AMPDPE[K],2.)/SSPDPE[K];
	      }
              PWT[IPE] = exp(-ALPWT/2.0);
	  }

#if 0
MagLog::dev()<< "PWT" << endl;
for (i=0;i<=NPE;i++)
	MagLog::dev()<< i << " " << PWT[i] << endl;
#endif

	  // Calculates the reciprocal of the volatility weight
	  for (IPE = 0; IPE <= NPE; IPE++)
	  {
	      IDPI = IDPPE[IPE];
              ZX = CFL1_[0][IDPI];
              ZY = CFL1_[1][IDPI];
              RVWT[IPE] = (pow(PDPE[0][IPE]-ZX,2.) + pow(PDPE[1][IPE]-ZY,2.)) * (pow(PDPE[2][IPE],2.) + 2.0*pow(PDPE[3][IPE],2.) + pow(PDPE[4][IPE],2.));

	      //*ZXX=0.0
              //*ZXY=0.0
	      //*ZYY=0.0
	      //*RVWT(IPE)=((PDPE(1,IPE)-ZX)**2+(PDPE(2,IPE)-ZY)**2)
	      //*    1             *((PDPE(3,IPE)-ZXX)**2+2.0*(PDPE(4,IPE)-ZXY)**2
             // *    2              +(PDPE(5,IPE)-ZYY)**2)
	  }

	  // Calculates the weighted values of partial derivatives
	  for (K = 0; K < 5; K++)
	  {
	      PDDIF[K] = 0.0;
              PDDII[K] = 0.0;
	  }

          SMWTF = 0.0;
          SMWTI = 0.0;
	  for (IPE = 0; IPE <= NPE; IPE++)  //c++
	  {
	      //*CHANGED from Valtulina : IF (RVWT(IPE).GT.0.0) THEN
              if (RVWT[IPE] > 1.0E-38)
	      {
		  WTF = PWT[IPE]/RVWT[IPE];
                  for (K = 0; K < 5; K++)
			  PDDIF[K] = PDDIF[K] + PDPE[K][IPE]*WTF;

                  SMWTF = SMWTF + WTF;
	      }
              else
	      {
		  WTI = PWT[IPE];
                  for (K = 0; K < 5; K++)
			  PDDII[K] = PDDII[K] + PDPE[K][IPE]*WTI;

                  SMWTI = SMWTI + WTI;
	      }
	  }

          if (SMWTI <= 0.0)
	  {
              for (K = 0; K < 5; K++)
		      PDD_[K][IDP1] = PDDIF[K]/SMWTF;
	  }
          else
	  {
              for (K = 0; K < 5; K++)
		      PDD_[K][IDP1] = PDDII[K]/SMWTI;
	  }

      } //for(IDP1)

      return;
}

// SDCLDP routine
/***********************************************************************
* Closest data points
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine selects, at each of the data points, nine data
* points closest to it.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*
* The output argument is
*   IPC  = two-dimensional integer array of dimension 9*NDP,
*          where the point numbers of nine data points closest
*          to the IDPth data point, in an ascending order of
*          the distance from the IDPth point, are to be
*          stored in the IDPth column, where IDP = 1, 2,
*          ..., NDP.
*
* The other arguments are
*   DSQ  = array of dimension NDP used as a work area,
*   IDSQ = integer array of dimension NDP used as a work
*          area.
********************************************************************/
//SUBROUTINE SDCLDP(NDP,XD,YD,IPC,DSQ,IDSQ)
template <class P>
void Akima761<P>::SDCLDP()
{
      double DSQMN;
      int    IDP,IDP1,IDSQMN,JDP,JDP1,JDPMN,JDSQMN,JIPC,JIPCMX;

      // DO-loop with respect to the data point number
      for (IDP = 0; IDP < NDP_; IDP++)
      {
	  IDP1 = IDP+1;
	  // Calculates the distance squared for all data points from the
	  // IDPth data point and stores the data point number and the
	  // calculated results in the IDSQ and DSQ arrays, respectively
	  for (JDP = 0; JDP < NDP_; JDP++)
	  {
	       JDP1 = JDP+1;
	       IDSQ_[JDP] = JDP;
	       DSQ_[JDP] = pow(XD_[JDP1]-XD_[IDP1],2.) + pow(YD_[JDP1]-YD_[IDP1],2.);
	  }

	  // Sorts the IDSQ and DSQ arrays in such a way that the IDPth
	  // point is in the first element in each array
          IDSQ_[IDP] = 0; //??? c++
          DSQ_[IDP] = DSQ_[0]; //c++
          IDSQ_[0] = IDP;     //c++
          DSQ_[0] = 0.0;      //c++

	  // Selects nine data points closest to the IDPth data point and
	  // stores the data point numbers in the IPC array
          JIPCMX = MIN(NDP_-1,10);
          for (JIPC = 1; JIPC < JIPCMX; JIPC++) //c++
	  {
	      JDSQMN = JIPC;
              DSQMN = DSQ_[JIPC];
	      JDPMN = JIPC + 1;
              for (JDP = JDPMN; JDP < NDP_; JDP++)
	      {
                  if (DSQ_[JDP] < DSQMN)
		  {
		      JDSQMN = JDP;
                      DSQMN = DSQ_[JDP];
		  }
	      }
              IDSQMN = IDSQ_[JDSQMN];
              IDSQ_[JDSQMN] = IDSQ_[JIPC];
              DSQ_[JDSQMN] = DSQ_[JIPC];
              IDSQ_[JIPC] = IDSQMN;
	  }

          for (JIPC = 0; JIPC < 9; JIPC++)
		  IPC_[JIPC][IDP] = IDSQ_[JIPC+1];

      } //for(IDP)

      return;
}

// SDCF3P routine
/**********************************************************************
* Coefficients of the third-degree polynomial for z(x,y)
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine calculates, for each data point, coefficients
* of the third-degree polynomial for z(x,y) fitted to the set of
* 10 data points consisting of the data point in question and
* nine data points closest to it.  When the condition number of
* the matrix associated with the 10 data point set is too large,
* this subroutine calculates coefficients of the second-degree
* polynomial fitted to the set of six data points consisting of
* the data point in question and five data points closest to it.
* When the condition number of the matrix associated with the six
* data point set is too large, this subroutine calculates
* coefficients of the first-degree polynomial fitted to the set of
* three data points closest to the data point in question.  When
* the condition number of the matrix associated with the three data
* point set is too large, this subroutine calculates coefficients
* of the first-degree polynomial fitted to the set of two data
* points consisting of the data point in question and one data
* point closest to it, assuming that the plane represented by the
* polynomial is horizontal in the direction which is at right
* angles to the line connecting the two data points.
*
* The input arguments are
*   NDP = number of data points,
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points,
*   ZD  = array of dimension NDP containing the z values
*         at the data points,
*   IPC = two-dimensional integer array of dimension
*         9*NDP containing the point numbers of 9 data
*         points closest to the IDPth data point in the
*         IDPth column, where IDP = 1, 2, ..., NDP.
*
* The output arguments are
*   CF  = two-dimensional array of dimension 9*NDP,
*         where the coefficients of the polynomial
*         (a10, a20, a30, a01, a11, a21, a02, a12, a03)
*         calculated at the IDPth data point are to be
*         stored in the IDPth column, where IDP = 1, 2,
*         ..., NDP,
*   NCP = integer array of dimension NDP, where the numbers
*         of the closest points used are to be stored.
*   IORD = integer array of dimension NDP containing the
*          degree of the polynomial used to compute PDD.
*
* The constant in the first PARAMETER statement below is
*   CNRMX = maximum value of the ratio of the condition
*           number of the matrix associated with the point
*           set to the number of points.
* The constant value has been selected empirically.
*
* The N1, N2, and N3 constants in the second PARAMETER statement
* are the numbers of the data points used to determine the first-,
* second-, and third-degree polynomials, respectively.
*
* This subroutine calls the SDLEQN subroutine.
******************************************************************/
//SUBROUTINE SDCF3P(NDP,XD,YD,ZD,IPC,CF,NCP,IORD)
template <class P>
void Akima761<P>::SDCF3P()
{
//*CHANGED from Valtulina : PARAMETER        (CNRMX=1.5E+04)
#define CNRMX 3.5E+07

      double CN,DET,X,X1,X2,Y,Y1,Y2,Z1,Z2;
      int I,IDP,IDPI,J;

//      double AA1[N1_][N1_],AA2[N2_][N2_];
      double AA3[N3_][N3_],B[N3_],CFI[N3_];

      // Main DO-loop with respect to the data point
      for (IDP = 0; IDP < NDP_; IDP++)
      {
	  for (J = 0; J < 9; J++)
	       CF3_[J][IDP] = 0.0;

#if 0
MagLog::dev()<< "CF3_" << endl;
for (I = 0; I < NDP_; I++)
{
	MagLog::dev()<< I << " " ;
	for(J=0;J<9;J++)
		MagLog::dev()<< CF3_[J][I] << " " ;
	MagLog::dev()<< endl;
}
MagLog::dev()<< "dummy" << endl;
#endif

	  // Calculates the coefficients of the set of linear equations
	  // with the 10-point data point set
          for (I = 0; I < N3_; I++)
	  {
              if (I == 0)
		  IDPI = IDP + 1; //c++
              else
		      IDPI = IPC_[I-1][IDP] + 1; //c++

              X = XD_[IDPI];
              Y = YD_[IDPI];
              AA3[I][0] = 1.0;
              AA3[I][1] = X;
              AA3[I][2] = X*X;
              AA3[I][3] = X*X*X;
              AA3[I][4] = Y;
              AA3[I][5] = X*Y;
              AA3[I][6] = X*X*Y;
              AA3[I][7] = Y*Y;
              AA3[I][8] = X*Y*Y;
              AA3[I][9] = Y*Y*Y;
	      B[I] = ZD_[IDPI];
	  }

#if 0
MagLog::dev()<< "AA3" << endl;
for (I = 0; I < N3_; I++)
{
	MagLog::dev()<< I << " " ;
	for(J=0;J<N3_;J++)
		MagLog::dev()<< AA3[J][I] << " " ;
	MagLog::dev()<< endl;
}
MagLog::dev()<< "B" << endl;
for (I = 0; I < N3_; I++)
	MagLog::dev()<< I << " " << B[I] << endl;
#endif

	  // Solves the set of linear equations
          SDLEQN(N3_,AA3,B,CFI,DET,CN);

#if 0
MagLog::dev()<< "CFI" << endl;
MagLog::dev()<< DET << " " << CN << endl;
for (I = 0; I < N3_; I++)
	MagLog::dev()<< I << " " << CFI[I] << endl;
#endif

	  // Stores the calculated results as the coefficients of the
	  // third-degree polynomial when applicable
          if (DET != 0.0)
	  {
              if (CN <= CNRMX*double(N3_))
	      {
		  for (J = 1; J < N3_; J++)
		       CF3_[J-1][IDP] = CFI[J];

                  NCP_[IDP] = N3_ - 1;
                  IORD_[IDP] = 3;
                  continue; //for(IDP)
	      }
	  }

	  // Calculates the coefficients of the set of linear equations
	  // with the 6-point data point set.
          for (I = 0; I < N2_; I++)
	  {
              if (I == 0)
		      IDPI = IDP + 1; //c++
              else
		      IDPI = IPC_[I-1][IDP] + 1; //c++

              X = XD_[IDPI];
              Y = YD_[IDPI];
              AA3[I][0] = 1.0;
              AA3[I][1] = X;
              AA3[I][2] = X*X;
              AA3[I][3] = Y;
              AA3[I][4] = X*Y;
              AA3[I][5] = Y*Y;
              B[I] = ZD_[IDPI];
	  }

	  // Solves the set of linear equations
          SDLEQN(N2_,AA3,B,CFI,DET,CN);

	  // Stores the calculated results as the coefficients of the
	  // second-degree polynomial when applicable
          if (DET != 0.0)
	  {
              if (CN <= CNRMX*double(N2_))
	      {
		  CF3_[0][IDP] = CFI[1];
		  CF3_[1][IDP] = CFI[2];
                  CF3_[3][IDP] = CFI[3];
                  CF3_[4][IDP] = CFI[4];
                  CF3_[6][IDP] = CFI[5];
                  NCP_[IDP] = N2_ - 1;
                  IORD_[IDP] = 2;
                  continue; //for(IDP)
	      }
	  }

	  // Calculates the coefficients of the set of linear equations
	  // with the 3-point data point set
          for (I = 0; I < N1_; I++)
	  {
	      IDPI = IPC_[I][IDP] + 1; //c++
              X = XD_[IDPI];
              Y = YD_[IDPI];
              AA3[I][0] = 1.0;
              AA3[I][1] = X;
              AA3[I][2] = Y;
              B[I] = ZD_[IDPI];
	  }

	  // Solves the set of linear equations
          SDLEQN(N1_,AA3,B,CFI,DET,CN);

	  // Stores the calculated results as the coefficients of the
	  // first-degree polynomial when applicable
          if (DET != 0.0)
	  {
              if (CN <= CNRMX*double(N1_))
	      {
		  CF3_[0][IDP] = CFI[1]; //c++
                  CF3_[3][IDP] = CFI[2]; //c++
                  NCP_[IDP] = N1_;
                  IORD_[IDP] = 1;
                  continue; //for(IDP)
	      }
	  }

	  // Calculates the coefficients of the set of linear equations
	  // with the 2-point data point set when applicable
          IDPI = IDP+1; //c++
          X1 = XD_[IDPI];
          Y1 = YD_[IDPI];
          Z1 = ZD_[IDPI];
          IDPI = IPC_[0][IDP];
          X2 = XD_[IDPI];
          Y2 = YD_[IDPI];
          Z2 = ZD_[IDPI];
          CF3_[0][IDP] = (X2-X1)* (Z2-Z1)/ (pow(X2-X1,2.)+ pow(Y2-Y1,2.));
          CF3_[3][IDP] = (Y2-Y1)* (Z2-Z1)/ (pow(X2-X1,2.)+ pow(Y2-Y1,2.));
          NCP_[IDP]   = 1;
          IORD_[NDP_]  = 0;

      } //for(IDP)

#if 0
MagLog::dev()<< "CF3_ END" << endl;
for (I = 0; I < NDP_; I++)
{
	MagLog::dev()<< I << " " ;
	for(J=0;J<9;J++)
		MagLog::dev()<< CF3_[J][I] << " " ;
	MagLog::dev()<< endl;
}
MagLog::dev()<< "dummy" << endl;
#endif

      return;
}

// SDLEQN routine
/*********************************************************************
* Solution of a set of linear equations
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine solves a set of linear equations.
*
* The input arguments are
*   N   = number of linear equations,
*   AA  = two-dimensional array of dimension N*N
*         containing the coefficients of the equations,
*   B   = array of dimension N containing the constant
*         values in the right-hand side of the equations.
*
* The output arguments are
*   X   = array of dimension N, where the solution is
*         to be stored,
*   DET = determinant of the AA array,
*   CN  = condition number of the AA matrix.
*
* The other arguments are
*   K   = integer array of dimension N used internally
*         as the work area,
*   EE  = two-dimensional array of dimension N*N used
*         internally as the work area,
*   ZZ  = two-dimensional array of dimension N*N used
*         internally as the work area.
*******************************************************************/
// SUBROUTINE SDLEQN(N,AA,B,X,DET,CN,K,EE,ZZ)

template <class P>
void Akima761<P>::SDLEQN(int N,double AA[N3_][N3_],double B[N3_],double X[N3_],double& DET,double& CN)
{
      float EE[N3_][N3_],ZZ[N3_][N3_];
      int K[N3_];

      float AANORM, ASOM, ZSOM, ZZNORM;
      float AAIIJ,AAIJIJ,AAIJMX,AAMX;
      int    I,IJ,IJP1,IJR,J,JJ,JMX,KJMX;


#if 0
MagLog::dev()<< "AA IN " << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< AA[J][I] << " ";
MagLog::dev()<< endl;
}
MagLog::dev()<< "dummy" << endl;
#endif

      // Calculation
      // Initial setting
      for (J = 0; J < N; J++)
	   K[J] = J;

      // ADDED from Valtulina : calculation of AANORM=NORMinf(AA)
      AANORM=0.0;
      for (I = 0; I < N; I++)
      {
	  ASOM=0.0;
          for (J = 0; J < N; J++)
	  {
	      EE[I][J] = 0.0;
              ASOM = ASOM + fabs(AA[I][J]);
	  }
          EE[I][I] = 1.0;
          if (ASOM > AANORM) AANORM=ASOM;
      }

      // Calculation of inverse matrix of AA
      for (IJ = 0; IJ < N; IJ++)
      {
	  // Finds out the element having the maximum absolute value
	  // in the IJ th row
	  AAMX = fabs(AA[IJ][IJ]);
          JMX = IJ;
          for (J = IJ; J < N; J++)
	  {
              if (fabs(AA[IJ][J]) > AAMX)
	      {
		  AAMX = fabs(AA[IJ][J]);
                  JMX = J;
	      }
	  }

	  // Switches two columns in such a way that the element with the
	  // maximum value is on the diagonal
          for (I = 0; I < N; I++)
	  {
	      AAIJMX = AA[I][IJ];
              AA[I][IJ] = AA[I][JMX];
              AA[I][JMX] = AAIJMX;
	  }
          KJMX = K[IJ];
          K[IJ] = K[JMX];
          K[JMX] = KJMX;

#if 0
MagLog::dev()<< "AA " << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< AA[J][I] << " ";
MagLog::dev()<< endl;
}
MagLog::dev()<< "K " << endl;
for (I = 0; I < N; I++)
MagLog::dev()<< I << " " << K[I] << endl;
#endif

	  // Makes the diagonal element to be unity
          AAIJIJ = AA[IJ][IJ];

	  //*CHANGED from Valtulina : IF (AAIJIJ.EQ.0.0) GO TO 210
          if (fabs(AAIJIJ) < 1.0E-8)
	  {
	       // Special case where the determinant is zero
	       for (I = 0; I < N; I++)
		       X[I] = 0.0;

	       DET = 0.0;
	       return;
	  }

	  for (J = IJ; J < N; J++)
	       AA[IJ][J] = AA[IJ][J]/AAIJIJ;

          for (JJ = 0; JJ < N; JJ++)
	       EE[IJ][JJ] = EE[IJ][JJ]/AAIJIJ;

#if 0
MagLog::dev()<< "AA " << endl;
for (I = 0; I < N; I++)
	MagLog::dev()<< IJ << " " << I << " " << AA[IJ][I] << endl;

MagLog::dev()<< "EE " << AAIJIJ << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< EE[J][I] << " ";
MagLog::dev()<< endl;
}
#endif

	  // Eliminates the lower left elements
          if (IJ < N-1) //c++
	  {
	      IJP1 = IJ + 1;
              for (I = IJP1; I < N; I++)
	      {
		  AAIIJ = AA[I][IJ];
                  for (J = IJP1; J < N; J++)
		      AA[I][J] = AA[I][J] - AA[IJ][J]*AAIIJ;

                  for (JJ = 0; JJ < N; JJ++)
		      EE[I][JJ] = EE[I][JJ] - EE[IJ][JJ]*AAIIJ;
	      }

#if 0
MagLog::dev()<< "AA IN " << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< AA[J][I] << " ";
MagLog::dev()<< endl;
}
#endif
	  }

#if 0
MagLog::dev()<< "EE" << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< EE[J][I] << " ";
MagLog::dev()<< endl;
}
MagLog::dev()<< "dummy" << endl;
#endif

	  //* Calculates the determinant
	  /**DELETED from Valtulina
	   *DELETED          IF (IJ.EQ.1) THEN
	   *DELETED              DET = 0.0
	   *DELETED              SGN = 1.0
	   *DELETED          END IF
	   *DELETED          SGN = SGN* ((-1)** (IJ+JMX))
	   *DELETED          DET = DET + LOG(ABS(AAIJIJ))
	   */
      } //for(IJ)

#if 0
MagLog::dev()<< "AA" << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< AA[J][I] << " ";
MagLog::dev()<< endl;
}
MagLog::dev()<< "EE" << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< EE[J][I] << " ";
MagLog::dev()<< endl;
}
MagLog::dev()<< "K" << endl;
for (I = 0; I < N; I++)
	MagLog::dev()<< I << " " << K[I] << endl;
#endif


      //*DELETED      IF (DET.LT.85.0) THEN
      //*DELETED          DET = SGN*EXP(DET)
      //*DELETED      ELSE
      //*DELETED          DET = SGN*1.0E38
      //*DELETED      END IF
      //*ADDED from Valtulina : at this point DET must be not equal 0
      DET=1.0;

      // Calculates the elements of the inverse matrix
      for (IJR = 0; IJR < N; IJR++)
      {
	  //F IJ = N + 1 - IJR 
	  IJ = N - 1 - IJR; //c++
          if (IJ < N-1)     //c++
	  {
	      IJP1 = IJ + 1;
              for (J = IJP1; J < N; J++)
	      {
		  for (JJ = 0; JJ < N; JJ++)
		       EE[IJ][JJ] = EE[IJ][JJ] - AA[IJ][J]*EE[J][JJ];
	      }
	  }
      }

#if 0
MagLog::dev()<< "EE" << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< EE[J][I] << " ";
MagLog::dev()<< endl;
}
#endif

      for (J = 0; J < N; J++)
      {
	  I = K[J];
          for (JJ = 0; JJ < N; JJ++)
	       ZZ[I][JJ] = EE[J][JJ];
      }

#if 0
MagLog::dev()<< "ZZ" << endl;
for (I = 0; I < N; I++)
{
	MagLog::dev()<< I << " ";
for (J = 0; J < N; J++)
	MagLog::dev()<< ZZ[J][I] << " ";
MagLog::dev()<< endl;
}
#endif

      // Calculation of the condition number of AA
      // *ADDED from Valtulina : calculation of ZZNORM=NORMinf(ZZ)
      // *DELETED      SA = 0.0
      // *DELETED      SZ = 0.0
      ZZNORM=0.0;
      for (I = 0; I < N; I++)
      {
	  ZSOM=0.0;
          for (J = 0; J < N; J++)
	  {
	       //*DELETED     SA = SA + AA(I,J)*AA(J,I)
	       //*DELETED     SZ = SZ + ZZ(I,J)*ZZ(J,I)
	       ZSOM=ZSOM+fabs(ZZ[I][J]);
	  }
          if (ZSOM > ZZNORM) ZZNORM=ZSOM;
      }

      // *DELETED      CN = SQRT(ABS(SA*SZ))
      CN=AANORM*ZZNORM;

      // Calculation of X vector
      for (I = 0; I < N; I++)
      {
	  X[I] = 0.0;
          for (J = 0; J < N; J++)
	       X[I] = X[I] + ZZ[I][J]*B[J];
      }

#if 0
MagLog::dev()<< "X" << endl;
for (I = 0; I < N; I++)
	MagLog::dev()<< I << " " << X[I] << endl;
#endif

      return;
}

// SDLS1P routine
/*******************************************************************
* Least squares fit of a linear surface (plane) to z(x,y) values
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs the least squares fit of a linear
* surface (plane) to a data point set consisting of the data
* point in question and several data points closest to it used
* in the SDCF3P subroutine.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x coordinates
*          of the data points,
*   YD   = array of dimension NDP containing the y coordinates
*          of the data points,
*   ZD   = array of dimension NDP containing the z values at
*          the data points,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          containing, in the IDPth column, point numbers of
*          nine data points closest to the IDPth data point,
*          where IDP = 1, 2, ..., NDP,
*   NCP  = integer array of dimension NDP containing the
*          numbers of the closest points used in the SDCF3P
*          subroutine.
*
* The output argument is
*   CFL1 = two-dimensional array of dimension 2*NDP, where
*          the coefficients (a10, a01) of the least squares
*          fit, first-degree polynomial calculated at the
*          IDPth data point are to be stored in the IDPth
*          column, where IDP = 1, 2, ..., NDP.
*
* Before this subroutine is called, the SDCF3P subroutine must
* have been called.
*************************************************************/
// SUBROUTINE SDLS1P(NDP,XD,YD,ZD,IPC,NCP,CFL1)
template <class P>
void Akima761<P>::SDLS1P()
{
      double A11,A12,A22,AN,B1,B2,DLT,SX,SXX,SXY,SXZ,SY,SYY,
	     SYZ,SZ,X,X1,X2,Y,Y1,Y2,Z,Z1,Z2;
      int    I,IDP,IDPI,NPLS;

      // DO-loop with respect to the data point
      for (IDP = 0; IDP < NDP_; IDP++)
      {
	  NPLS = NCP_[IDP] + 1;
          if (NPLS != 2)
	  {
	       // Performs the least squares fit of a plane.
	       SX  = SY  = 0.0;
	       SXX = SXY = SYY = 0.0;
	       SZ  = SXZ = SYZ = 0.0;
	       for (I = 0; I < NPLS; I++)
	       {
		     if (I == 0)
			  IDPI = IDP+1; //c++
		     else
			  IDPI = IPC_[I-1][IDP] + 1; //c++

		     X = XD_[IDPI];
		     Y = YD_[IDPI];
		     Z = ZD_[IDPI];
		     SX = SX + X;
		     SY = SY + Y;
		     SXX = SXX + X*X;
		     SXY = SXY + X*Y;
		     SYY = SYY + Y*Y;
		     SZ = SZ + Z;
		     SXZ = SXZ + X*Z;
		     SYZ = SYZ + Y*Z;
	       }

	       AN = NPLS;
	       A11 = AN*SXX - SX*SX;
	       A12 = AN*SXY - SX*SY;
	       A22 = AN*SYY - SY*SY;
	       B1 = AN*SXZ - SX*SZ;
	       B2 = AN*SYZ - SY*SZ;
	       DLT = A11*A22 - A12*A12;
	       CFL1_[0][IDP] = (B1*A22-B2*A12)/DLT;
	       CFL1_[1][IDP] = (B2*A11-B1*A12)/DLT;
          }
	  else
	  {
	       IDPI = IDP+1;  //c++
	       X1 = XD_[IDPI];
	       Y1 = YD_[IDPI];
	       Z1 = ZD_[IDPI];
	       IDPI = IPC_[0][IDP] + 1; //c++
	       X2 = XD_[IDPI];
	       Y2 = YD_[IDPI];
	       Z2 = ZD_[IDPI];
	       CFL1_[0][IDP] = (X2-X1)* (Z2-Z1)/ (pow(X2-X1,2.)+ pow(Y2-Y1,2.));
	       CFL1_[1][IDP] = (Y2-Y1)* (Z2-Z1)/ (pow(X2-X1,2.)+ pow(Y2-Y1,2.));
	   }
      }

      return;
}

// GRADC routine
/************************************************************
*
*                                               From SRFPACK
*                                            Robert J. Renka
*                                  Dept. of Computer Science
*                                       Univ. of North Texas
*                                             (817) 565-2816
*                                                   01/25/97
*
*   Given a Delaunay triangulation of N points in the plane
* with associated data values Z, this subroutine estimates
* first and second partial derivatives at node K.  The der-
* ivatives are taken to be the partials at K of a cubic
* function which interpolates Z(K) and fits the data values
* at a set of nearby nodes in a weighted least squares
* sense.  A Marquardt stabilization factor is used if neces-
* sary to ensure a well-conditioned system.  Thus, a unique
* solution exists if there are at least 10 noncollinear
* nodes.
*
*   The triangulation may include constraints introduced by
* subroutine ADDCST, in which case the derivative estimates
* are influenced by the nonconvex geometry of the domain.
* Refer to subroutine GETNP.  If data values at the con-
* straint nodes are not known, subroutine ZGRADL, which
* computes approximate data values at constraint nodes along
* with gradients, should be called in place of this routine.
*
*   An alternative routine, GRADG, employs a global method
* to compute the first partial derivatives at all of the
* nodes at once.  That method is usually more efficient
* (when all first partials are needed) and may be more ac-
* curate, depending on the data.
*
* On input:
*
*       K = Index of the node at which derivatives are to be
*           estimated.  1 .LE. K .LE. N.
*
*       NCC = Number of constraint curves (refer to TRIPACK
*             subroutine ADDCST).  NCC .GE. 0.
*
*       LCC = Array of length NCC (or dummy array of length
*             1 if NCC = 0) containing the index of the
*             first node of constraint I in LCC(I).  For I =
*             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
*             LCC(NCC+1) = N+1.
*
*       N = Number of nodes in the triangulation.
*           N .GE. 10.
*
*       X,Y = Arrays of length N containing the coordinates
*             of the nodes with non-constraint nodes in the
*             first LCC(1)-1 locations, followed by NCC se-
*             quences of constraint nodes.
*
*       Z = Array of length N containing data values associ-
*           ated with the nodes.
*
*       LIST,LPTR,LEND = Data structure defining the trian-
*                        gulation.  Refer to TRIPACK
*                        Subroutine TRMESH.
*
* Input parameters are not altered by this routine.
*
* On output:
*
*       DX,DY = Estimated first partial derivatives at node
*               K unless IER < 0.
*
*       DXX,DXY,DYY = Estimated second partial derivatives
*                     at node K unless IER < 0.
*
*       IER = Error indicator:
*             IER = L > 0 if no errors were encountered and
*                         L nodes (including node K) were
*                         employed in the least squares fit.
*             IER = -1 if K, NCC, an LCC entry, or N is
*                      outside its valid range on input.
*             IER = -2 if all nodes are collinear.
*
* TRIPACK modules required by GRADC:  GETNP, INTSEC
*
* SRFPACK modules required by GRADC:  GIVENS, ROTATE, SETRO3
*
* Intrinsic functions called by GRADC:  ABS, MIN, REAL, SQRT
*
**********************************************************************
* Local parameters:
*
* A =         Transpose of the augmented regression matrix
* C =         First component of the plane rotation deter-
*               mined by subroutine GIVENS
* DIST =      Array containing the distances between K and
*               the elements of NPTS (refer to GETNP)
* DMIN =      Minimum of the magnitudes of the diagonal
*               elements of the regression matrix after
*               zeros are introduced below the diagonal
* DS =        Squared distance between nodes K and NPTS(LNP)
* DTOL =      Tolerance for detecting an ill-conditioned
*               system.  The system is accepted when DMIN/W
*               .GE. DTOL.
* I =         DO-loop index
* IERR =      Error flag for calls to GETNP
* J =         DO-loop index
* JP1 =       J+1
* KK =        Local copy of K
* L =         Number of columns of A**T to which a rotation
*               is applied
* LMAX,LMIN = Min(LMX,N), Min(LMN,N)
* LMN,LMX =   Minimum and maximum values of LNP for N
*               sufficiently large.  In most cases LMN-1
*               nodes are used in the fit.  4 .LE. LMN .LE.
*               LMX.
* LM1 =       LMIN-1 or LNP-1
* LNP =       Length of NPTS
* NP =        Element of NPTS to be added to the system
* NPTS =      Array containing the indexes of a sequence of
*               nodes ordered by distance from K.  NPTS(1)=K
*               and the first LNP-1 elements of NPTS are
*               used in the least squares fit.  Unless LNP
*               exceeds LMAX, NPTS(LNP) determines R.
* RIN =       Inverse of the distance R between node K and
*               NPTS(LNP) or some point further from K than
*               NPTS(LMAX) if NPTS(LMAX) is used in the fit.
*               R is a radius of influence which enters into
*               the weight W.
* RS =        R*R
* RTOL =      Tolerance for determining R.  If the relative
*               change in DS between two elements of NPTS is
*               not greater than RTOL, they are treated as
*               being the same distance from node K.
* S =         Second component of the plane rotation deter-
*               mined by subroutine GIVENS
* SF =        Scale factor for the linear terms (columns 8
*               and 9) in the least squares fit -- inverse
*               of the root-mean-square distance between K
*               and the nodes (other than K) in the least
*               squares fit
* SFS =       Scale factor for the quadratic terms (columns
*               5, 6, and 7) in the least squares fit --
*               SF*SF
* SFC =       Scale factor for the cubic terms (first 4
*               columns) in the least squares fit -- SF**3
* STF =       Marquardt stabilization factor used to damp
*               out the first 4 solution components (third
*               partials of the cubic) when the system is
*               ill-conditioned.  As STF increases, the
*               fitting function approaches a quadratic
*               polynomial.
* SUM =       Sum of squared distances between node K and
*               the nodes used in the least squares fit
* W =         Weight associated with a row of the augmented
*               regression matrix -- 1/D - 1/R, where D < R
*               and D is the distance between K and a node
*               entering into the least squares fit
* XK,YK,ZK =  Coordinates and data value associated with K
*
************************************************************/
// SUBROUTINE GRADC(K,NCC,LCC,N,X,Y,Z,LIST,LPTR,LEND,DX,DY,DXX,DXY,DYY,IER)

template <class P>
int Akima761<P>::GRADC(int K,int NCC,int* LCC,int N,double& DX,double& DY,double& DXX,double& DXY,double& DYY)
{
#define LMN 14
#define LMX 30
#define RTOL 1.E-5
#define DTOL 0.01

      bool   flag;
      double C1,DMIN,DS,RIN,RS;
      double S1;
      double SF,SFC;
      double SFS,STF,SUM,W,XK,YK,ZK;
      int    I,IERR,J,JP1,KK,L,LM1,LMAX,LMIN,LNP,NP;

      double A[10][10];
      double DIST[LMX+1]; //c++
      int    NPTS[LMX+1]; //c++

      KK = K;

      // Test for errors and initialize LMIN and LMAX
      if (KK < 1 || KK > N || NCC < 0 || N < 10) return -1;

      LMIN = MIN(LMN,N);
      LMAX = MIN(LMX,N);

      // Compute NPTS, DIST, LNP, SF, SFS, SFC, and RIN --
      // Set NPTS to the closest LMIN-1 nodes to K
      SUM = 0.;
      NPTS[1] = KK;
      DIST[1] = 0.;
      LM1 = LMIN - 1;
      for (LNP = 2; LNP <= LM1; LNP++)
      {
	  IERR = GETNP(NCC,LCC,N,XD_,YD_,LIST1_,LPTR1_,LEND1_,LNP,NPTS,DIST);
          if (IERR != 0) return -1;

	  DS = pow(DIST[LNP],2.);
          SUM = SUM + DS;
      }

      // Add additional nodes to NPTS until the relative increase
      // in DS is at least RTOL
      flag = false;
      for (LNP = LMIN; LNP <= LMAX; LNP++)
      {
	  GETNP(NCC,LCC,N,XD_,YD_,LIST1_,LPTR1_,LEND1_,LNP,NPTS,DIST);
          RS = pow(DIST[LNP],2.);
          if ( ((RS-DS)/DS) > RTOL)
	  {
	       if (LNP > 10)
	       {
		       flag = true;
		       break;
	       }
	  }
          SUM = SUM + RS;
      }

      if (!flag)
      {
	   // Use all LMAX nodes in the least squares fit.  RS is
	   // arbitrarily increased by 10 per cent
	      RS = 1.1*RS;
	      LNP = LMAX + 1;
      }

      // There are LNP-2 equations corresponding to nodes NPTS(2),
      // ...,NPTS(LNP-1)
      SFS = double(LNP-2)/SUM;
      SF = sqrt(SFS);
      SFC = SF*SFS;
      RIN = 1./sqrt(RS);
      XK = XD_[KK];
      YK = YD_[KK];
      ZK = ZD_[KK];

      // A Q-R decomposition is used to solve the least squares
      // system.  The transpose of the augmented regression
      // matrix is stored in A with columns (rows of A) defined
      // as follows:  1-4 are the cubic terms, 5-7 are the quadratic
      // terms with coefficients DXX/2, DXY, and DYY/2,
      // 8 and 9 are the linear terms with coefficients DX and
      // DY, and the last column is the right hand side.

      // Set up the first 9 equations and zero out the lower triangle
      // with Givens rotations
      for (I = 1; I <= 9; I++)
      {
	  NP = NPTS[I+1];
          W = 1./DIST[I+1] - RIN;
          SETRO3(XK,YK,ZK,XD_[NP],YD_[NP],ZD_[NP],SF,SFS,SFC,W,&A[I-1][0]); //c++

	  if (I == 1) continue;
          for (J = 1; J <= I-1; J++)
	  {
	      JP1 = J + 1;
              L = 10 - J;
	      GIVENS(A[J-1][J-1],A[I-1][J-1],C1,S1);         //c++
              ROTATE(L,C1,S1,&A[J-1][JP1-1],&A[I-1][JP1-1]); //c++
	  }
      }

#if 0
MagLog::dev()<< SFS << " " << SF << " " << RIN << " " << XK << " " << YK << " " << ZK << endl;
MagLog::dev()<< " AA " << endl;
for (I = 0; I < 10; I++)
{
     MagLog::dev()<< I << " ";
     for (J = 0; J < 10; J++)
	     MagLog::dev()<< A[I][J] << " ";
     MagLog::dev()<< endl;
}
#endif

      // Add the additional equations to the system using
      // the last column of A.  I .LE. LNP
      flag = false;
      I = 11;
      for (;;) //loop 70
      {
	  while (I < LNP)
	  {
	       NP = NPTS[I];
	       W = 1./DIST[I] - RIN;
	       SETRO3(XK,YK,ZK,XD_[NP],YD_[NP],ZD_[NP],SF,SFS,SFC,W,&A[9][0]);

#if 0
for (int Ii = 0; Ii < 10; Ii++)
{
     MagLog::dev()<< Ii << " ";
     for (int Ji = 0; Ji < 10; Ji++)
	     MagLog::dev()<< A[Ii][Ji] << " ";
     MagLog::dev()<< endl;
}
MagLog::dev()<< "dummy" << endl;
#endif

	       for (J = 1; J <= 9; J++)
	       {
		    JP1 = J + 1;
		    L = 10 - J;
		    GIVENS(A[J-1][J-1],A[9][J-1],C1,S1);      //c++
		    ROTATE(L,C1,S1,&A[J-1][JP1-1],&A[9][JP1-1]);//c++

#if 0
for (int Ii = 0; Ii < 10; Ii++)
{
     MagLog::dev()<< Ii << " ";
     for (int Ji = 0; Ji < 10; Ji++)
	     MagLog::dev()<< A[Ii][Ji] << " ";
     MagLog::dev()<< endl;
}
#endif
	       }
	       I = I + 1;

#if 0
for (int Ii = 0; Ii < 10; Ii++)
{
     MagLog::dev()<< Ii << " ";
     for (int Ji = 0; Ji < 10; Ji++)
	     MagLog::dev()<< A[Ii][Ji] << " ";
     MagLog::dev()<< endl;
}
#endif

	  }

#if 0
MagLog::dev()<< " AA " << endl;
for (I = 0; I < 10; I++)
{
     MagLog::dev()<< I << " ";
     for (J = 0; J < 10; J++)
	     MagLog::dev()<< A[I][J] << " ";
     MagLog::dev()<< endl;
}
#endif

	  // Test the system for ill-conditioning
	  DMIN = MIN( fabs(A[0][0]), fabs(A[1][1]) );
	  DMIN = MIN( DMIN, fabs(A[2][2]) );
	  DMIN = MIN( DMIN, fabs(A[3][3]) );
	  DMIN = MIN( DMIN, fabs(A[4][4]) );
	  DMIN = MIN( DMIN, fabs(A[5][5]) );
	  DMIN = MIN( DMIN, fabs(A[6][6]) );
	  DMIN = MIN( DMIN, fabs(A[7][7]) );
	  DMIN = MIN( DMIN, fabs(A[8][8]) );
	  if ((DMIN/W) >= DTOL)
	  {
	       flag = true;
	       break; //for(;;) loop 70
	  }

	  if (LNP > LMAX) break; //for(;;) loop 70

	  // Add another node to the system and increase R.
	  // Note that I = LNP
          LNP = LNP + 1;
          if (LNP <= LMAX)
	  {
	      GETNP(NCC,LCC,N,XD_,YD_,LIST1_,LPTR1_,LEND1_,LNP,NPTS,DIST);
              RS = pow(DIST[LNP],2.);
	  }
          RIN = 1./sqrt(1.1*RS);
      } //for(;;) loop 70

      if (!flag)
      {
	   // Stabilize the system by damping third partials -- add
	   // multiples of the first four unit vectors to the first
	   // four equations
	   STF = W;
	   for (I = 1; I <= 4; I++)
	   {
		A[9][I-1] = STF;
		for (J = I + 1; J <= 10; J++)
			A[9][J-1] = 0.;

	        for (J = I; J <= 9; J++)
	        {
		     JP1 = J + 1;
		     L = 10 - J;
		     GIVENS(A[J-1][J-1],A[9][J-1],C1,S1);         //c++
		     ROTATE(L,C1,S1,&A[J-1][JP1-1],&A[9][JP1-1]); //c++
	        }
	   }

           // Test the damped system for ill-conditioning
           DMIN = MIN( fabs(A[4][4]), fabs(A[5][5]) );
           DMIN = MIN( DMIN, fabs(A[6][6]) );
           DMIN = MIN( DMIN, fabs(A[7][7]) );
           DMIN = MIN( DMIN, fabs(A[8][8]) );
	   if ((DMIN/W) < DTOL)
		return -2; // No unique solution due to collinear nodes
      }

      // Solve the 9 by 9 triangular system for the last 5
      // components (first and second partial derivatives)
      DY = A[8][9]/A[8][8];
      DX = (A[7][9]-A[7][8]*DY) / A[7][7];
      DYY = (A[6][9]-A[6][7]*DX-A[6][8]*DY) / A[6][6];
      DXY = (A[5][9]-A[5][6]*DYY-A[5][7]*DX-A[5][8]*DY) / A[5][5];
      DXX = (A[4][9]-A[4][5]*DXY-A[4][6]*DYY-A[4][7]*DX-A[4][8]*DY) / A[4][4];

      // Scale the solution components
      DX = SF*DX;
      DY = SF*DY;
      DXX = 2.*SFS*DXX;
      DXY = SFS*DXY;
      DYY = 2.*SFS*DYY;
      
      return (LNP - 1);
}

// SETRO3 routine
/************************************************************
*
*                                               From SRFPACK
*                                            Robert J. Renka
*                                  Dept. of Computer Science
*                                       Univ. of North Texas
*                                             (817) 565-2767
*                                                   01/25/97
*
*   This subroutine sets up the I-th row of an augmented re-
* gression matrix for a weighted least squares fit of a
* cubic function f(x,y) to a set of data values z, where
* f(XK,YK) = ZK.  The first four columns (cubic terms) are
* scaled by S3, the next three columns (quadratic terms)
* are scaled by S2, and the eighth and ninth columns (lin-
* ear terms) are scaled by S1.
*
* On input:
*
*       XK,YK = Coordinates of node K.
*
*       ZK = Data value at node K to be interpolated by f.
*
*       XI,YI,ZI = Coordinates and data value at node I.
*
*       S1,S2,S3 = Scale factors.
*
*       W = Weight associated with node I.
*
* The above parameters are not altered by this routine.
*
*       ROW = Array of length 10.
*
* On output:
*
*       ROW = Array containing a row of the augmented re-
*             gression matrix.
*
* Modules required by SETRO3:  None
*
************************************************************/
//SUBROUTINE SETRO3(XK,YK,ZK,XI,YI,ZI,S1,S2,S3,W,ROW)

template <class P>
void Akima761<P>::SETRO3(double XK,double YK,double ZK,double XI,double YI,double ZI,double S1,double S2,double S3,double W,double* ROW)
{
      double DX,DY,W1,W2,W3;

      DX = XI - XK;
      DY = YI - YK;
      W1 = S1*W;
      W2 = S2*W;
      W3 = S3*W;
      ROW[0] = DX*DX*DX*W3;
      ROW[1] = DX*DX*DY*W3;
      ROW[2] = DX*DY*DY*W3;
      ROW[3] = DY*DY*DY*W3;
      ROW[4] = DX*DX*W2;
      ROW[5] = DX*DY*W2;
      ROW[6] = DY*DY*W2;
      ROW[7] = DX*W1;
      ROW[8] = DY*W1;
      ROW[9] = (ZI-ZK)*W;

      return;
}

// GIVENS routine
/************************************************************
*
*                                               From SRFPACK
*                                            Robert J. Renka
*                                  Dept. of Computer Science
*                                       Univ. of North Texas
*                                             (817) 565-2767
*                                                   09/01/88
*
*   This subroutine constructs the Givens plane rotation,
*
*           ( C  S)
*       G = (     ) , where C*C + S*S = 1,
*           (-S  C)
*
* which zeros the second component of the vector (A,B)**T
* (transposed).  Subroutine ROTATE may be called to apply
* the transformation to a 2 by N matrix.
*
*   This routine is identical to subroutine SROTG from the
* LINPACK BLAS (Basic Linear Algebra Subroutines).
*
* On input:
*
*       A,B = Components of the vector defining the rota-
*             tion.  These are overwritten by values R
*             and Z (described below) which define C and S.
*
* On output:
*
*       A = Signed Euclidean norm R of the input vector:
*           R = +/-SQRT(A*A + B*B)
*
*       B = Value Z such that:
*             C = SQRT(1-Z*Z) and S=Z if ABS(Z) .LE. 1, and
*             C = 1/Z and S = SQRT(1-C*C) if ABS(Z) > 1.
*
*       C = +/-(A/R) or 1 if R = 0.
*
*       S = +/-(B/R) or 0 if R = 0.
*
* Modules required by GIVENS:  None
*
* Intrinsic functions called by GIVENS:  ABS, SQRT
*
************************************************************/
// SUBROUTINE GIVENS(A,B,C,S)

template <class P>
void Akima761<P>::GIVENS(double& A1,double& B1,double& C1,double& S1)
{
      // AA,BB = Local copies of A and B
      // R =     C*A + S*B = +/-SQRT(A*A+B*B)
      // U,V =   Variables used to scale A and B for computing R
      
	double AA,BB,R,U,V;

      AA = A1;
      BB = B1;
      if (fabs(AA) > fabs(BB))
      {
	   U = AA + AA;
	   V = BB/U;
	   R = sqrt(.25+V*V)*U;
	   C1 = AA/R;
	   S1 = V* (C1+C1);

	   // Note that R has the sign of A, C > 0, and S has
	   // SIGN(A)*SIGN(B)
	   B1 = S1;
	   A1 = R;
	   return;
      }
      else
      {
	   if (BB == 0.)
	   {
		   C1 = 1.;
		   S1 = 0.;
		   return;
	   }
	   else
	   {
		   U = BB + BB;
		   V = AA/U;

		   // Store R in A
		   A1 = sqrt(.25+V*V)*U;
		   S1 = BB/A1;
		   C1 = V* (S1+S1);

		   // Note that R has the sign of B, S > 0, and C has
		   // SIGN(A)*SIGN(B)
		   B1 = 1.;
		   if (C1 != 0.) B1 = 1./C1;
		   return;
	   }
      }
}

// ROTATE routine
/************************************************************
*
*                                               From SRFPACK
*                                            Robert J. Renka
*                                  Dept. of Computer Science
*                                       Univ. of North Texas
*                                             (817) 565-2767
*                                                   09/01/88
*
*                                                ( C  S)
*   This subroutine applies the Givens rotation  (     )  to
*                                                (-S  C)
*                    (X(1) ... X(N))
* the 2 by N matrix  (             ) .
*                    (Y(1) ... Y(N))
*
*   This routine is identical to subroutine SROT from the
* LINPACK BLAS (Basic Linear Algebra Subroutines).
*
* On input:
*
*       N = Number of columns to be rotated.
*
*       C,S = Elements of the Givens rotation.  Refer to
*             subroutine GIVENS.
*
* The above parameters are not altered by this routine.
*
*       X,Y = Arrays of length .GE. N containing the compo-
*             nents of the vectors to be rotated.
*
* On output:
*
*       X,Y = Arrays containing the rotated vectors (not
*             altered if N < 1).
*
* Modules required by ROTATE:  None
*
************************************************************/
// SUBROUTINE ROTATE(N,C,S,X,Y)

template <class P>
void Akima761<P>::ROTATE(int N,double C1,double S1,double* X,double* Y)
{
      double XI,YI;
      int    I;

#if 0
MagLog::dev()<< "ROTATE" << endl;
for (I = 0; I < N; I++)
	MagLog::dev()<< I << " " << X[I] << " " << Y[I] << endl;
#endif

      for (I = 0; I < N; I++)
      {
	      XI = X[I];
	      YI = Y[I];
	      X[I] = C1*XI + S1*YI;
	      Y[I] = -S1*XI + C1*YI;
      }
      return;
}

// SDLCTN routine
/*********************************************************************
* Locating points in a scattered data point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine locates points in a scattered data point set in
* the x-y plane, i.e., determines to which triangle each of the
* points to be located belongs.  When a point to be located does
* not lie inside the data area, this subroutine determines the
* border line segment when the point lies in an outside rectangle,
* in an outside triangle, or in the overlap of two outside
* rectangles.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   NIP  = number of points to be located,
*   XI   = array of dimension NIP containing the x
*          coordinates of the points to be located,
*   YI   = array of dimension NIP containing the y
*          coordinates of the points to be located.
*
* The output arguments are
*   KTLI = integer array of dimension NIP, where the code
*          for the type of the piece of plane in which each
*          interpolated point lies is to be stored
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border line
*            segments
*        = 4 for a triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP, where the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the points to
*          be located are to be stored.
******************************************************************/
//SUBROUTINE SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI,KTLI,ITLI)

template <class P>
void Akima761<P>::SDLCTN(int NIP,double* XI,double* YI,int* KTLI,int* ITLI) const
{
#define SPDT(U1,V1,U2,V2,U3,V3) (U1-U3)* (U2-U3) + (V1-V3)* (V2-V3)
#define VPDT(U1,V1,U2,V2,U3,V3) (U1-U3)* (V2-V3) - (V1-V3)* (U2-U3)

      bool goto40;
      double X0,X1,X2,X3,Y0,Y1,Y2,Y3;
      int    IIP,IL1,IL2,ILII,IP1,IP2,IP3,ITII,ITLIPV,KTLIPV;

      // Outermost DO-loop with respect to the points to be located
      for (IIP = 0; IIP < NIP; IIP++)
      {
	  X0 = XI[IIP];
          Y0 = YI[IIP];
          if (IIP == 0)
	  {
	      KTLIPV = 0;
              ITLIPV = 0;
	  }
          else
	  {
	      KTLIPV = KTLI[IIP-1];
              ITLIPV = ITLI[IIP-1];
          }

	  // Checks if in the same inside triangle as previous
          if (KTLIPV == 1)
	  {
	      ITII = ITLIPV;
              IP1 = IPT_[0][ITII-1]; //c++
              IP2 = IPT_[1][ITII-1]; //c++
              IP3 = IPT_[2][ITII-1]; //c++
              X1 = XD_[IP1];
              Y1 = YD_[IP1];
              X2 = XD_[IP2];
              Y2 = YD_[IP2];
              X3 = XD_[IP3];
              Y3 = YD_[IP3];
              if ((VPDT(X1,Y1,X2,Y2,X0,Y0) >= 0.0) &&
                  (VPDT(X2,Y2,X3,Y3,X0,Y0) >= 0.0) &&
                  (VPDT(X3,Y3,X1,Y1,X0,Y0) >= 0.0))
	      {
		  KTLI[IIP] = 1;
                  ITLI[IIP] = ITII;
                  continue; //for(IIP); goto 40
	      }
	  }

	  // Locates inside the data area
	  goto40 = false;
          for (ITII = 0; ITII < NT_; ITII++)
	  {
	      IP1 = IPT_[0][ITII];
              IP2 = IPT_[1][ITII];
              IP3 = IPT_[2][ITII];
              X1 = XD_[IP1];
              Y1 = YD_[IP1];
              X2 = XD_[IP2];
              Y2 = YD_[IP2];
              X3 = XD_[IP3];
              Y3 = YD_[IP3];
              if ((VPDT(X1,Y1,X2,Y2,X0,Y0) >= 0.0) &&
                  (VPDT(X2,Y2,X3,Y3,X0,Y0) >= 0.0) &&
                  (VPDT(X3,Y3,X1,Y1,X0,Y0) >= 0.0))
	      {
		  KTLI[IIP] = 1;
                  ITLI[IIP] = ITII+1; //c++
                  goto40 = true;
		  break;
              }
	  }

	  if (goto40) continue; //for(IIP)

	  // Locates outside the data area
          for (ILII = 0; ILII < NL_; ILII++)
	  {
	      IL1 = ILII;
	      // IL2 = MOD(IL1,NL_) + 1
	      IL2 = (IL1+1)%NL_;
              IP1 = IPL_[0][IL1];
              IP2 = IPL_[0][IL2];
              IP3 = IPL_[1][IL2];
              X1 = XD_[IP1];
              Y1 = YD_[IP1];
              X2 = XD_[IP2];
              Y2 = YD_[IP2];
              X3 = XD_[IP3];
              Y3 = YD_[IP3];
              if (VPDT(X1,Y1,X3,Y3,X0,Y0) <= 0.0)
	      {
                  if (VPDT(X1,Y1,X3,Y3,X2,Y2) <= 0.0)
		  {
                      if ((SPDT(X1,Y1,X0,Y0,X2,Y2) <= 0.0) &&
                          (SPDT(X3,Y3,X0,Y0,X2,Y2) <= 0.0))
		      {
			  KTLI[IIP] = 3;
                          ITLI[IIP] = IL2+1; //c++
			  goto40 = true;
			  break; //for(ILII)
		      }
		  }
                  if (VPDT(X1,Y1,X3,Y3,X2,Y2) >= 0.0)
		  {
                      if ((SPDT(X1,Y1,X0,Y0,X2,Y2) >= 0.0) &&
                          (SPDT(X3,Y3,X0,Y0,X2,Y2) >= 0.0))
		      {
			  KTLI[IIP] = 4;
                          ITLI[IIP] = IL2+1; //c++
                          goto40 = true;
	                  break; //for(ILII)
                      }
		  }
	      }
	  }

	  if (goto40) continue; //for(IIP)

          for (ILII = 0; ILII < NL_; ILII++)
	  {
	      IL2 = ILII;
              IP2 = IPL_[0][IL2];
              IP3 = IPL_[1][IL2];
              X2 = XD_[IP2];
              Y2 = YD_[IP2];
              X3 = XD_[IP3];
              Y3 = YD_[IP3];
              if (VPDT(X2,Y2,X3,Y3,X0,Y0) <= 0.0)
	      {
                  if ((SPDT(X3,Y3,X0,Y0,X2,Y2) >= 0.0) &&
                      (SPDT(X2,Y2,X0,Y0,X3,Y3) >= 0.0))
		  {
		      KTLI[IIP] = 2;
                      ITLI[IIP] = IL2+1; //c++
	              break;
                  }
	      }
	  }

      } //for(IIP) loop 40

      return;
}

// SDPLNL routine
/**************************************************************************
* Polynomials
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine determines a polynomial in x and y for each
* triangle or rectangle in the x-y plane and calculates the z
* value by evaluating the polynomial for the desired points,
* for bivariate interpolation and surface fitting for scattered
* data.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z
*          values at the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   PDD  = two-dimensional array of dimension 5*NDP
*          containing the partial derivatives at the data
*          points,
*   NIP  = number of output points at which interpolation is
*          to be performed,
*   XI   = array of dimension NIP containing the x
*          coordinates of the output points,
*   YI   = array of dimension NIP containing the y
*          coordinates of the output points,
*   KTLI = integer array of dimension NIP, each element
*          containing the code for the type of the piece of
*          the plane in which each output point lies
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border
*            line segments
*        = 4 for the triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP containing the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the output
*          points.
*
* The output argument is
*   ZI   = array of dimension NIP, where the calculated z
*          values are to be stored.
*******************************************************************/
// SUBROUTINE SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,NIP,XI,YI,KTLI,ITLI,ZI)

template <class P>
void Akima761<P>::SDPLNL(int NIP,double* XI,double* YI,int* KTLI,int* ITLI,double* ZI) const
{
      double A,AA,AB,ACT2,AD,ADBC,AP,B,BB,BC,BDT2,BP,C,CC,CD,
             CP,DF,DD,DLT,DP,DX,DY,E1,E2,G1,G2,H1,H2,H3,LUSQ,
             LVSQ,P0,P00,P01,P02,P03,P04,P05,P1,P10,P11,P12,
             P13,P14,P2,P20,P21,P22,P23,P3,P30,P31,P32,P4,P40,
             P41,P5,P50,SPUV,U,V,WT1,WT2,X0,XII,Y0,YII,Z0,ZII,
	     ZII1,ZII2;
      int    I,IDP,IIP,ILI,IR,ITLII,ITLIPV,K,KTLII,KTLIPV;

      double PD[5][3],X[3],Y[3],Z[3],ZU[3],ZUU[3],ZUV[3],ZV[3],ZVV[3];

      // Outermost DO-loop with respect to the output point
      for (IIP = 0; IIP < NIP; IIP++)
      {
	  XII = XI[IIP];
          YII = YI[IIP];
          KTLII = KTLI[IIP];
          ITLII = ITLI[IIP];
          if (IIP == 0)
	  {
	      KTLIPV = 0;
              ITLIPV = 0;
	  }
          else
	  {
	      KTLIPV = KTLI[IIP-1];
              ITLIPV = ITLI[IIP-1];
          }

	  // Part 1.  Calculation of ZII by interpolation
          if (KTLII == 1)
	  {
	      // Calculates the coefficients when necessary
              if (KTLII != KTLIPV || ITLII != ITLIPV)
	      {
		  //Loads coordinate and partial derivative values at the vertexes
		  for (I = 0; I < 3; I++)
		  {
		      IDP = IPT_[I][ITLII-1]; //c++
                      X[I] = XD_[IDP];
                      Y[I] = YD_[IDP];
                      Z[I] = ZD_[IDP];
                      for (K = 0; K < 5; K++)
			      PD[K][I] = PDD_[K][IDP-1];  //c++
		  }

		  // Determines the coefficients for the coordinate system
		  // transformation from the x-y system to the u-v system
		  // and vice versa
                  X0 = X[0];
                  Y0 = Y[0];
                  A = X[1] - X0;
                  B = X[2] - X0;
                  C = Y[1] - Y0;
                  DF = Y[2] - Y0;
                  AD = A*DF;
                  BC = B*C;
                  DLT = AD - BC;
                  AP = DF/DLT;
                  BP = -B/DLT;
                  CP = -C/DLT;
                  DP = A/DLT;

		  // Converts the partial derivatives at the vertexes of the
		  // triangle for the u-v coordinate system
                  AA = A*A;
                  ACT2 = 2.0*A*C;
                  CC = C*C;
                  AB = A*B;
                  ADBC = AD + BC;
                  CD = C*DF;
                  BB = B*B;
                  BDT2 = 2.0*B*DF;
                  DD = DF*DF;
                  for (I = 0; I < 3; I++)
		  {
		      ZU[I]  = A*PD[0][I] + C*PD[1][I];
                      ZV[I]  = B*PD[0][I] + DF*PD[1][I];
                      ZUU[I] = AA*PD[2][I] + ACT2*PD[3][I] + CC*PD[4][I];
                      ZUV[I] = AB*PD[2][I] + ADBC*PD[3][I] + CD*PD[4][I];
                      ZVV[I] = BB*PD[2][I] + BDT2*PD[3][I] + DD*PD[4][I];
		  }

		  // Calculates the coefficients of the polynomial
                  P00 = Z[0];
                  P10 = ZU[0];
                  P01 = ZV[0];
                  P20 = 0.5*ZUU[0];
                  P11 = ZUV[0];
                  P02 = 0.5*ZVV[0];
                  H1 = Z[1] - P00 - P10 - P20;
                  H2 = ZU[1] - P10 - ZUU[0];
                  H3 = ZUU[1] - ZUU[0];
                  P30 = 10.0*H1 - 4.0*H2 + 0.5*H3;
                  P40 = -15.0*H1 + 7.0*H2 - H3;
                  P50 = 6.0*H1 - 3.0*H2 + 0.5*H3;
                  H1 = Z[2] - P00 - P01 - P02;
                  H2 = ZV[2] - P01 - ZVV[0];
                  H3 = ZVV[2] - ZVV[0];
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3;
                  P04 = -15.0*H1 + 7.0*H2 - H3;
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3;
                  LUSQ = AA + CC;
                  LVSQ = BB + DD;
                  SPUV = AB + CD;
                  P41 = 5.0*SPUV/LUSQ*P50;
                  P14 = 5.0*SPUV/LVSQ*P05;
                  H1 = ZV[1] - P01 - P11 - P41;
                  H2 = ZUV[1] - P11 - 4.0*P41;
                  P21 = 3.0*H1 - H2;
                  P31 = -2.0*H1 + H2;
                  H1 = ZU[2] - P10 - P11 - P14;
                  H2 = ZUV[2] - P11 - 4.0*P14;
                  P12 = 3.0*H1 - H2;
                  P13 = -2.0*H1 + H2;
                  E1 = (LVSQ-SPUV)/ ((LVSQ-SPUV)+ (LUSQ-SPUV));
                  E2 = 1.0 - E1;
                  G1 = 5.0*E1 - 2.0;
                  G2 = 1.0 - G1;
                  H1 = 5.0* (E1* (P50-P41)+E2* (P05-P14)) + (P41+P14);
                  H2 = 0.5*ZVV[1] - P02 - P12;
                  H3 = 0.5*ZUU[2] - P20 - P21;
                  P22 = H1 + G1*H2 + G2*H3;
                  P32 = H2 - P22;
                  P23 = H3 - P22;
	      }

	      // Converts XII and YII to u-v system
              DX = XII - X0;
              DY = YII - Y0;
              U = AP*DX + BP*DY;
              V = CP*DX + DP*DY;

	      // Evaluates the polynomial
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))));
              P1 = P10 + V* (P11+V* (P12+V* (P13+V*P14)));
              P2 = P20 + V* (P21+V* (P22+V*P23));
              P3 = P30 + V* (P31+V*P32);
              P4 = P40 + V*P41;
              P5 = P50;
              ZI[IIP] = P0 + U* (P1+U* (P2+U* (P3+U* (P4+U*P5))));
	  }

	  // Part 2.  Calculation of ZII by extrapolation in the rectangle
          if (KTLII == 2)
	  {
	      // Calculates the coefficients when necessary
              if (KTLII != KTLIPV || ITLII != ITLIPV)
	      {
		  // Loads coordinate and partial derivative values at the end
		  // points of the border line segment
		  for (I = 0; I < 2; I++)
		  {
		      IDP = IPL_[I][ITLII-1]; //c++
                      X[I] = XD_[IDP];
                      Y[I] = YD_[IDP];
                      Z[I] = ZD_[IDP];
                      for (K = 0; K < 5; K++)
			      PD[K][I] = PDD_[K][IDP-1];
		  }

		  // Determines the coefficients for the coordinate system
		  // transformation from the x-y system to the u-v system
		  // and vice versa
                  X0 = X[0];
                  Y0 = Y[0];
                  A = Y[1] - Y[0];
                  B = X[1] - X[0];
                  C = -B;
                  DF = A;
                  AD = A*DF;
                  BC = B*C;
                  DLT = AD - BC;
                  AP = DF/DLT;
                  BP = -B/DLT;
                  CP = -BP;
                  DP = AP;

		  // Converts the partial derivatives at the end points of the
		  // border line segment for the u-v coordinate system
                  AA = A*A;
                  ACT2 = 2.0*A*C;
                  CC = C*C;
                  AB = A*B;
                  ADBC = AD + BC;
                  CD = C*DF;
                  BB = B*B;
                  BDT2 = 2.0*B*DF;
                  DD = DF*DF;
                  for (I = 0; I < 2; I++)
		  {
		      ZU[I]  = A*PD[0][I] + C*PD[1][I];
                      ZV[I]  = B*PD[0][I] + DF*PD[1][I];
                      ZUU[I] = AA*PD[2][I] + ACT2*PD[3][I] + CC*PD[4][I];
                      ZUV[I] = AB*PD[2][I] + ADBC*PD[3][I] + CD*PD[4][I];
                      ZVV[I] = BB*PD[2][I] + BDT2*PD[3][I] + DD*PD[4][I];
		  }

		  // Calculates the coefficients of the polynomial
                  P00 = Z[0];
                  P10 = ZU[0];
                  P01 = ZV[0];
                  P20 = 0.5*ZUU[0];
                  P11 = ZUV[0];
                  P02 = 0.5*ZVV[0];
                  H1 = Z[1] - P00 - P01 - P02;
                  H2 = ZV[1] - P01 - ZVV[0];
                  H3 = ZVV[1] - ZVV[0];
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3;
                  P04 = -15.0*H1 + 7.0*H2 - H3;
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3;
                  H1 = ZU[1] - P10 - P11;
                  H2 = ZUV[1] - P11;
                  P12 = 3.0*H1 - H2;
                  P13 = -2.0*H1 + H2;
                  P21 = 0.5* (ZUU[1]-ZUU[0]);
              }

	      // Converts XII and YII to u-v system
              DX = XII - X0;
              DY = YII - Y0;
              U = AP*DX + BP*DY;
              V = CP*DX + DP*DY;

	      // Evaluates the polynomial
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))));
              P1 = P10 + V* (P11+V* (P12+V*P13));
              P2 = P20 + V*P21;
              ZI[IIP] = P0 + U* (P1+U*P2);
          }

	  // Part 3.  Calculation of ZII by extrapolation in the triangle
          if (KTLII == 3)
	  {
	      // Calculates the coefficients when necessary
              if (KTLII != KTLIPV || ITLII != ITLIPV)
	      {
		  // Loads coordinate and partial derivative values at the vertex
		  // of the triangle
		  IDP = IPL_[0][ITLII-1]; //c++
                  X0 = XD_[IDP];
                  Y0 = YD_[IDP];
                  Z0 = ZD_[IDP];
                  for (K = 0; K < 5; K++)
			  PD[K][0] = PDD_[K][IDP-1]; //c++

		  // Calculates the coefficients of the polynomial
                  P00 = Z0;
                  P10 = PD[0][0];
                  P01 = PD[1][0];
                  P20 = 0.5*PD[2][0];
                  P11 = PD[3][0];
                  P02 = 0.5*PD[4][0];
              }

	      // Converts XII and YII to U-V system
              U = XII - X0;
              V = YII - Y0;

	      // Evaluates the polynomial
              P0 = P00 + V* (P01+V*P02);
              P1 = P10 + V*P11;
              ZI[IIP] = P0 + U* (P1+U*P20);
	  }

	  // Part 4.  Calculation of ZII by extrapolation in the triangle
	  // which is an overlap of two rectangles
          if (KTLII == 4)
	  {
	      // Calculates the coefficients
	      for (IR = 1; IR <= 2; IR++)
	      {
                  if (IR == 1)
		      // ILI = MOD(ITLII+NL-2,NL) + 1
		      ILI = ((ITLII+NL_-2)%NL_) + 1;
                  else
		      ILI = ITLII;

		  // Loads coordinate and partial derivative values at the end
		  // points of the border line segment
                  for (I = 0; I < 2; I++)
		  {
		      IDP = IPL_[I][ILI-1]; //c++
                      X[I] = XD_[IDP];
                      Y[I] = YD_[IDP];
                      Z[I] = ZD_[IDP];
                      for (K = 0; K < 5; K++)
			      PD[K][I] = PDD_[K][IDP-1]; //c++
		  }

		  // Determines the coefficients for the coordinate system
		  // transformation from the x-y system to the u-v system
		  // and vice versa
                  X0 = X[0];
                  Y0 = Y[0];
                  A = Y[1] - Y[0];
                  B = X[1] - X[0];
                  C = -B;
                  DF = A;
                  AD = A*DF;
                  BC = B*C;
                  DLT = AD - BC;
                  AP = DF/DLT;
                  BP = -B/DLT;
                  CP = -BP;
                  DP = AP;

		  // Converts the partial derivatives at the end points of the
		  // border line segment for the u-v coordinate system
                  AA = A*A;
                  ACT2 = 2.0*A*C;
                  CC = C*C;
                  AB = A*B;
                  ADBC = AD + BC;
                  CD = C*DF;
                  BB = B*B;
                  BDT2 = 2.0*B*DF;
                  DD = DF*DF;
                  for (I = 0; I < 2; I++)
		  {
		      ZU[I]  = A*PD[0][I] + C*PD[1][I];
                      ZV[I]  = B*PD[0][I] + DF*PD[1][I];
                      ZUU[I] = AA*PD[2][I] + ACT2*PD[3][I] + CC*PD[4][I];
                      ZUV[I] = AB*PD[2][I] + ADBC*PD[3][I] + CD*PD[4][I];
                      ZVV[I] = BB*PD[2][I] + BDT2*PD[3][I] + DD*PD[4][I];
		  }

		  // Calculates the coefficients of the polynomial
                  P00 = Z[0];
                  P10 = ZU[0];
                  P01 = ZV[0];
                  P20 = 0.5*ZUU[0];
                  P11 = ZUV[0];
                  P02 = 0.5*ZVV[0];
                  H1 = Z[1] - P00 - P01 - P02;
                  H2 = ZV[1] - P01 - ZVV[0];
                  H3 = ZVV[1] - ZVV[0];
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3;
                  P04 = -15.0*H1 + 7.0*H2 - H3;
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3;
                  H1 = ZU[1] - P10 - P11;
                  H2 = ZUV[1] - P11;
                  P12 = 3.0*H1 - H2;
                  P13 = -2.0*H1 + H2;
                  P21 = 0.5* (ZUU[1]-ZUU[0]);

		  // Converts XII and YII to u-v system
                  DX = XII - X0;
                  DY = YII - Y0;
                  U = AP*DX + BP*DY;
                  V = CP*DX + DP*DY;

		  // Evaluates the polynomial
                  P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))));
                  P1 = P10 + V* (P11+V* (P12+V*P13));
                  P2 = P20 + V*P21;
                  ZII = P0 + U* (P1+U*P2);
                  if (IR == 1)
		  {
		      ZII1 = ZII;
                      WT2 = pow((X[0]-X[1])* (XII-X[1])+
				 (Y[0]-Y[1])* (YII-Y[1]),2.);
		  }
                  else
		  {
		      ZII2 = ZII;
                      WT1 = pow((X[1]-X[0])* (XII-X[0])+
				(Y[1]-Y[0])* (YII-Y[0]),2.);
		  }

	       } //for(IR); loop 110

              ZI[IIP] = (WT1*ZII1+WT2*ZII2)/ (WT1+WT2);
	  }

      } //for (IIP); loop 120

      return;
}
