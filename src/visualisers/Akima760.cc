/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima760.cc
    Implementation of Akima760 class.
    
    Magics Team - ECMWF 2004
   
    Created: Fri 12-Mar-2004
    
*/

#include "MagLog.h"
#include "Akima760Method.h"
#include "Timer.h"

using namespace magics;


Akima760::Akima760(const  AbstractMatrix& matrix, const Akima760MethodAttributes& attr) :
    MatrixHandler(matrix), attr_(attr)
{
	MagLog::debug() << "Akima760 Constructor" << "\n";

       int j;
//     monoColumns_ = MatrixHandler::columns(); //syntax also valid
       monoColumns_ = this->matrix_.columns();
       monoRows_ = this->matrix_.rows();

      // Compute matrix output sizes
       double aux = (this->matrix_.regular_column( monoColumns_-1) - this->matrix_.regular_column(0)) / attr_.resolutionX_;
       if ( (double)(int(aux)) != aux ) aux += 1.; //next integer number
       ncols_ = int(aux + 1.);                  //must include the first and the last input coordinates

       aux = (this->matrix_.regular_row( monoRows_-1) - this->matrix_.regular_row(0)) / attr_.resolutionY_;
       if ( (double)(int(aux)) != aux ) aux += 1.; //next integer number
       nrows_ = int(aux + 1.);                  //must include the first and the last input coordinates

      // Allocate 3 auxiliary arrays where the estimated zx, zy, and zxy 
      // values at the input-grid data points are to be stored.
      WKZX_  = new double* [ monoRows_];
      WKZY_  = new double* [ monoRows_];
      WKZXY_ = new double* [ monoRows_];
      for (j = 0; j <  monoRows_; j++)
      {
	      WKZX_[j]  = new double [monoColumns_];
	      WKZY_[j]  = new double [monoColumns_];
	      WKZXY_[j] = new double [monoColumns_];
      }

      // Check for missing values
      missingValues_ = this->matrix_.hasMissingValues();

      // Estimates partial derivatives at all input-grid data points
      //{
      Timer timer("Akima", "Time spent in interpolation");
      rgpd3p();
      //}
      
      double from = this->matrix_.regular_row(0);
      for ( int i = 0; i < nrows_; i++) {
    	  double pos = from + i *attr_.resolutionY_;
    	  rowsMap_.insert(make_pair(pos, i));
    	  rows_.push_back(pos);
      }
      from = this->matrix_.regular_column(0);
      for ( int i = 0; i < ncols_; i++) {
    	  double pos = from + i *attr_.resolutionX_;
          	 columnsMap_.insert(make_pair(pos, i));
          	columns_.push_back(pos);
      }
         
          
#if 0
      // Print partial derivatives
      int i;
      MagLog::debug() << "WKZX" << "\n";;
      for (i = 0; i < this->matrix_.rows(); i++)
      {
	      MagLog::debug() << "\n";;
	      for (j = 0; j < this->matrix_.columns(); j++)
		      MagLog::debug() << WKZX_[i][j] << " ";
       }
      MagLog::debug() << "\n" << "WKZY" << "\n";;
      for (i = 0; i < this->matrix_.rows(); i++)
      {
	      MagLog::debug() << "\n";;
	      for (j = 0; j < this->matrix_.columns(); j++)
	      MagLog::debug() << WKZY_[i][j] << " ";
       }
      MagLog::debug() << "\n" << "WKZXY" << "\n";;
      for (i = 0; i < this->matrix_.rows(); i++)
      {
	      MagLog::debug() << "\n";;
	      for (j = 0; j < this->matrix_.columns(); j++)
	      MagLog::debug() << WKZXY_[i][j] << " ";
       }
#endif
}


Akima760::~Akima760()
{
      int j;

      // release memory
      for (j = 0; j < monoRows_; j++)
      {
	      delete [] WKZX_[j];
	      delete [] WKZY_[j];
	      delete [] WKZXY_[j];
	      WKZX_[j]  = NULL;
	      WKZY_[j]  = NULL;
	      WKZXY_[j] = NULL;
      }

      delete [] WKZX_;
      delete [] WKZY_;
      delete [] WKZXY_;
      WKZX_  = NULL;
      WKZY_  = NULL;
      WKZXY_ = NULL; 
}


double Akima760::regular_row(int i) const
{
//Remove later. Why this function is called so many times ???
//static long itest=0;
//MagLog::debug() << "Akima760 row=" << itest++ << "\n";

	// Check if the coordinate does not go beyond the upper limit
	double aux = i*attr_.resolutionY_ + this->matrix_.regular_row(0);
        return (aux > this->matrix_.regular_row(monoRows_-1) ? this->matrix_.regular_row(monoRows_-1) : aux);
}


double Akima760::row(int i, int j) const {
	return regular_row(i);
}


double Akima760::regular_column(int j) const
{ 
//Remove later. Why this function is called so many times ???
//static long jtest=0;
//MagLog::debug() << "Akima760 column=" << jtest++ << "\n";

	// Check if the coordinate does not go beyond the upper limit
	double aux = j*attr_.resolutionX_ + this->matrix_.regular_column(0);
        return (aux > this->matrix_.regular_column(monoColumns_-1) ? this->matrix_.regular_column(monoColumns_-1) : aux);
}

double Akima760::column(int i, int j) const {
	
	return regular_column(j);
	
}

double Akima760::operator()(int  i, int  j) const
{
	double value =  rgbi3p(regular_column(j), regular_row(i));
	
	return value;
}


void Akima760::rgpd3p()
{
//*
//* Partial derivatives of a bivariate function on a rectangular grid
//* (a supporting subroutine of the RGBI3P/RGSF3P subroutine package)
//*
//* Hiroshi Akima
//* U.S. Department of Commerce, NTIA/ITS
//* Version of 1995/08
//*
//* This subroutine estimates three partial derivatives, zx, zy, and
//* zxy, of a bivariate function, z(x,y), on a rectangular grid in
//* the x-y plane.  It is based on the revised Akima method that has
//* the accuracy of a bicubic polynomial.
//*
//* The input arguments are
//*   NXDF = number of the input-grid data points in the x
//*         coordinate (must be 2 or greater),
//*   NYDF = number of the input-grid data points in the y
//*         coordinate (must be 2 or greater),
//*   XD  = array of dimension NXDF containing the x coordinates
//*         of the input-grid data points (must be in a
//*         monotonic increasing order),
//*   YD  = array of dimension NYDF containing the y coordinates
//*         of the input-grid data points (must be in a
//*         monotonic increasing order),
//*   ZD  = two-dimensional array of dimension NXDF*NYDF
//*         containing the z(x,y) values at the input-grid data
//*         points.
//*
//* The output argument is
//*   PDD = three-dimensional array of dimension 3*NXDF*NYDF,
//*         where the estimated zx, zy, and zxy values at the
//*         input-grid data points are to be stored.
//*
//* This routine was adapted to C++ by Fernando Ii, 02/04
//*

      double  B00=0., B00X=0., B00Y=0., B01=0., B10=0., B11=0., CX1=0., CX2=0., CX3=0., CY1=0., CY2=0.,
              CY3=0., DISF =0., DNM =0., DZ00 =0., DZ01 =0., DZ02 = 0.,DZ03 =0., DZ10 =0., DZ11 =0., DZ12 = 0.,
              DZ13 =0., DZ20 =0., DZ21 =0., DZ22 =0., DZ23 =0., DZ30 = 0.,DZ31 =0., DZ32 = 0.,DZ33 = 0.,
              DZX10 =0., DZX20 = 0.,DZX30 =0., DZXY11 =0., DZXY12 =0., DZXY13 =0., DZXY21 = 0.,
              DZXY22 =0., DZXY23 =0., DZXY31 =0., DZXY32 =0., DZXY33 =0., DZY01 =0., DZY02 = 0.,
              DZY03 =0., EPSLN =0., PEZX =0., PEZXY =0., PEZY = 0.,SMPEF =0., SMPEI =0., SMWTF = 0.,
              SMWTI = 0.,SX =0., SXX =0., SXXY =0., SXXYY =0., SXY =0., SXYY =0., SXYZ =0., SXZ =0., SY =0., SYY =0., 
//              SYZ,SZ,VOLF,WT,X0,X1,X2,X3,XX1,XX2,XX3,Y0,Y1,Y2,
              SYZ =0., SZ =0., VOLF =0., WT = 0.,X0 =0., X1 = 0.,X2 =0., X3 =0., Y0 =0., Y1 =0., Y2 =0., 
              Y3 =0., Z00 = 0.,Z01 =0., Z02 = 0.,Z03 =0., Z10 =0., Z11 =0., Z12 =0., Z13 = 0.,Z20 =0., Z21 =0., Z22 = 0.,
//              Z23,Z30,Z31,Z32,Z33,ZXDI,ZXYDI,ZYDI,ZZ0,ZZ1,ZZ2;
              Z23 =0., Z30 =0., Z31 =0., Z32 =0., Z33 = 0.,ZXDI =0., ZXYDI =0., ZYDI = 0.0;

      int  IPEX,IPEY,IX0,IX1,IX2,IX3,IY0,IY1,IY2,IY3,NX0,NY0;

      int  NXDF,NYDF;       // input matrix dimensions

      //*     .. Local Arrays .
      double  B00XA[4],B00YA[4],B01A[4],B10A[4],CXA[3][4],
              CYA[3][4],SXA[4],SXXA[4],SYA[4],SYYA[4],XA[3][4],
              YA[3][4],Z0IA[3][4],ZI0A[3][4];

      int     IDLT[3][4] = {{-3,-2,-1,1},{-2,-1,1,2},{-1,1,2,3}};

      //* Statement Function definitions 
#define Z2F(XX1,XX2,ZZ0,ZZ1) ((ZZ1-ZZ0)*XX2/XX1 + ZZ0)
#define Z3F(XX1,XX2,XX3,ZZ0,ZZ1,ZZ2) (((ZZ2-ZZ0) * (XX3-XX1)/XX2  - (ZZ1-ZZ0) * (XX3-XX2)/XX1) * (XX3/ (XX2-XX1)) + ZZ0)

      //* Initial setting of some local variables
      NXDF = this->matrix_.columns();
      NYDF = this->matrix_.rows();
      NX0 = std::max((int)4,NXDF);
      NY0 = std::max((int)4,NYDF);

      //* Double DO-loop with respect to the input grid points
      for (IY0 = 0; IY0 < NYDF; IY0++)
      {
	  for (IX0 = 0; IX0 < NXDF; IX0++)
	  {
	      // Check for missing values
	      if ( missingValues_ )
	      {
		      if (CheckMissingValues(IX0,IY0) == 0)
		      {
			    WKZX_[IY0][IX0]  = missing();
			    WKZY_[IY0][IX0]  = missing();
			    WKZXY_[IY0][IX0] = missing();
			    continue;
		      }
	      }

              X0  = this->matrix_.regular_column(IX0);
              Y0  = this->matrix_.regular_row(IY0);
              Z00 = this->matrix_(IY0,IX0);
//            Z00 = this->matrix_.operator()(IY0,IX0);  //also valid

	      //* Part 1.  Estimation of ZXDI
	      //* Initial setting
              SMPEF = 0.0;
              SMWTF = 0.0;
              SMPEI = 0.0;
              SMWTI = 0.0;

	      //* DO-loop with respect to the primary estimate
              for (IPEX = 0; IPEX < 4; IPEX++)
	      {
	          //* Selects necessary grid points in the x direction
                  IX1 = IX0 + IDLT[0][IPEX];
                  IX2 = IX0 + IDLT[1][IPEX];
                  IX3 = IX0 + IDLT[2][IPEX];
                  if ((IX1 < 0) || (IX2 < 0) || (IX3 < 0) ||
                      (IX1 >= NX0) || (IX2 >= NX0) || (IX3 >= NX0)) continue;

		  //* Selects and/or supplements the x and z values
                  X1  = this->matrix_.regular_column(IX1) - X0;
                  Z10 = this->matrix_(IY0,IX1);
                  if (NXDF >= 4)
		  {
                      X2  = this->matrix_.regular_column(IX2) - X0;
                      X3  = this->matrix_.regular_column(IX3) - X0;
                      Z20 = this->matrix_(IY0,IX2);
                      Z30 = this->matrix_(IY0,IX3);
		  }
                  else if (NXDF == 3)
		  {
                      X2  = this->matrix_.regular_column(IX2) - X0;
                      Z20 = this->matrix_(IY0,IX2);
                      X3  = 2*this->matrix_.regular_column(2) - this->matrix_.regular_column(1) - X0;
                      Z30 = Z3F(X1,X2,X3,Z00,Z10,Z20);
		  }
                  else if (NXDF == 2)
		  {
                      X2  = 2*this->matrix_.regular_column(1) - this->matrix_.regular_column(0) - X0;
                      Z20 = Z2F(X1,X2,Z00,Z10);
                      X3  = 2*this->matrix_.regular_column(0) - this->matrix_.regular_column(1) - X0;
                      Z30 = Z2F(X1,X3,Z00,Z10);
		  }

                  DZX10 = (Z10-Z00)/X1;
                  DZX20 = (Z20-Z00)/X2;
                  DZX30 = (Z30-Z00)/X3;

		  //* Calculates the primary estimate of partial derivative zx as
		  //* the coefficient of the bicubic polynomial
                  CX1 = X2*X3/ ((X1-X2)* (X1-X3));
                  CX2 = X3*X1/ ((X2-X3)* (X2-X1));
                  CX3 = X1*X2/ ((X3-X1)* (X3-X2));
                  PEZX = CX1*DZX10 + CX2*DZX20 + CX3*DZX30;

		  //* Calculates the volatility factor and distance factor in the x
		  //* direction for the primary estimate of zx
                  SX   = X1 + X2 + X3;
                  SZ   = Z00 + Z10 + Z20 + Z30;
                  SXX  = X1*X1 + X2*X2 + X3*X3;
                  SXZ  = X1*Z10 + X2*Z20 + X3*Z30;
                  DNM  = 4.0*SXX - SX*SX;
                  B00  = (SXX*SZ-SX*SXZ)/DNM;
                  B10  = (4.0*SXZ-SX*SZ)/DNM;
                  DZ00 = Z00 - B00;
                  DZ10 = Z10 - (B00+B10*X1);
                  DZ20 = Z20 - (B00+B10*X2);
                  DZ30 = Z30 - (B00+B10*X3);
                  VOLF = DZ00*DZ00 + DZ10*DZ10 + DZ20*DZ20 + DZ30*DZ30;
                  DISF = SXX;

		  //* Calculates the EPSLN value, which is used to decide whether or
		  //* not the volatility factor is essentially zero
                  EPSLN = (Z00*Z00 + Z10*Z10 + Z20*Z20 + Z30*Z30) * 1.0E-12;

		  //* Accumulates the weighted primary estimates of zx and their weights
                  if (VOLF > EPSLN)
		  {
		      //* - For a finite weight
                      WT = 1.0/ (VOLF*DISF);
                      SMPEF = SMPEF + WT*PEZX;
                      SMWTF = SMWTF + WT;
		  }
                  else
		  {
		      //* - For an infinite weight
                      SMPEI = SMPEI + PEZX;
                      SMWTI = SMWTI + 1.0;
		  }

		  //* Saves the necessary values for estimating zxy
                  XA[0][IPEX] = X1;
                  XA[1][IPEX] = X2;
                  XA[2][IPEX] = X3;
                  ZI0A[0][IPEX] = Z10;
                  ZI0A[1][IPEX] = Z20;
                  ZI0A[2][IPEX] = Z30;
                  CXA[0][IPEX] = CX1;
                  CXA[1][IPEX] = CX2;
                  CXA[2][IPEX] = CX3;
                  SXA[IPEX] = SX;
                  SXXA[IPEX] = SXX;
                  B00XA[IPEX] = B00;
                  B10A[IPEX] = B10;
              } //end for IPX

	      //* Calculates the final estimate of zx
              if (SMWTI < 0.5) ZXDI = SMPEF/SMWTF;  //When no infinite weights exist
              else	       ZXDI = SMPEI/SMWTI;  //When infinite weights exist

	      //* End of Part 1
	      //* Part 2.  Estimation of ZYDI
	      //* Initial setting
              SMPEF = 0.0;
              SMWTF = 0.0;
              SMPEI = 0.0;
              SMWTI = 0.0;

	      //* DO-loop with respect to the primary estimate
              for (IPEY = 0; IPEY < 4; IPEY++)
	      {
		  //* Selects necessary grid points in the y direction
                  IY1 = IY0 + IDLT[0][IPEY];
                  IY2 = IY0 + IDLT[1][IPEY];
                  IY3 = IY0 + IDLT[2][IPEY];
                  if ((IY1 < 0) || (IY2 < 0) || (IY3 < 0) ||
                      (IY1 >= NY0) || (IY2 >= NY0) || (IY3 >= NY0)) continue;

		  //* Selects and/or supplements the y and z values
                  Y1  = this->matrix_.regular_row(IY1) - Y0;
                  Z01 = this->matrix_(IY1,IX0);
                  if (NYDF >= 4)
		  {
                      Y2 = this->matrix_.regular_row(IY2) - Y0;
                      Y3 = this->matrix_.regular_row(IY3) - Y0;
                      Z02 = this->matrix_(IY2,IX0);
                      Z03 = this->matrix_(IY3,IX0);
		  }
                  else if (NYDF == 3)
		  {
                      Y2 = this->matrix_.regular_row(IY2) - Y0;
                      Z02 = this->matrix_(IY2,IX0);
                      Y3 = 2*this->matrix_.regular_row(2) - this->matrix_.regular_row(1) - Y0;
                      Z03 = Z3F(Y1,Y2,Y3,Z00,Z01,Z02);
		  }
                  else if (NYDF == 2)
		  {
                      Y2 = 2*this->matrix_.regular_row(1) - this->matrix_.regular_row(0) - Y0;
                      Z02 = Z2F(Y1,Y2,Z00,Z01);
                      Y3 = 2*this->matrix_.regular_row(0) - this->matrix_.regular_row(1) - Y0;
                      Z03 = Z2F(Y1,Y3,Z00,Z01);
		  }

                  DZY01 = (Z01-Z00)/Y1;
                  DZY02 = (Z02-Z00)/Y2;
                  DZY03 = (Z03-Z00)/Y3;

		  //* Calculates the primary estimate of partial derivative zy as
		  //* the coefficient of the bicubic polynomial
                  CY1 = Y2*Y3/ ((Y1-Y2)* (Y1-Y3));
                  CY2 = Y3*Y1/ ((Y2-Y3)* (Y2-Y1));
                  CY3 = Y1*Y2/ ((Y3-Y1)* (Y3-Y2));
                  PEZY = CY1*DZY01 + CY2*DZY02 + CY3*DZY03;

		  //* Calculates the volatility factor and distance factor in the y
		  //* direction for the primary estimate of zy
                  SY = Y1 + Y2 + Y3;
                  SZ = Z00 + Z01 + Z02 + Z03;
                  SYY = Y1*Y1 + Y2*Y2 + Y3*Y3;
                  SYZ = Y1*Z01 + Y2*Z02 + Y3*Z03;
                  DNM = 4.0*SYY - SY*SY;
                  B00 = (SYY*SZ-SY*SYZ)/DNM;
                  B01 = (4.0*SYZ-SY*SZ)/DNM;
                  DZ00 = Z00 - B00;
                  DZ01 = Z01 - (B00+B01*Y1);
                  DZ02 = Z02 - (B00+B01*Y2);
                  DZ03 = Z03 - (B00+B01*Y3);
                  VOLF = DZ00*DZ00 + DZ01*DZ01 + DZ02*DZ02 + DZ03*DZ03;
                  DISF = SYY;

		  //* Calculates the EPSLN value, which is used to decide whether or
		  //* not the volatility factor is essentially zero
                  EPSLN = (Z00*Z00 + Z01*Z01 + Z02*Z02 + Z03*Z03) * 1.0E-12;

		  //* Accumulates the weighted primary estimates of zy and their weights
                  if (VOLF > EPSLN)
		  {
		      //* - For a finite weight
                      WT = 1.0/ (VOLF*DISF);
                      SMPEF = SMPEF + WT*PEZY;
                      SMWTF = SMWTF + WT;
		  }
                  else
		  {
		      //* - For an infinite weight
                      SMPEI = SMPEI + PEZY;
                      SMWTI = SMWTI + 1.0;
		  }

		  //* Saves the necessary values for estimating zxy
                  YA[0][IPEY] = Y1;
                  YA[1][IPEY] = Y2;
                  YA[2][IPEY] = Y3;
                  Z0IA[0][IPEY] = Z01;
                  Z0IA[1][IPEY] = Z02;
                  Z0IA[2][IPEY] = Z03;
                  CYA[0][IPEY] = CY1;
                  CYA[1][IPEY] = CY2;
                  CYA[2][IPEY] = CY3;
                  SYA[IPEY] = SY;
                  SYYA[IPEY] = SYY;
                  B00YA[IPEY] = B00;
                  B01A[IPEY] = B01;
              } //end for IPEY

	      //* Calculates the final estimate of zy
              if (SMWTI < 0.5)	        //When no infinite weights exist
                  ZYDI = SMPEF/SMWTF;
              else                      //When infinite weights exist
                  ZYDI = SMPEI/SMWTI;

	      //* End of Part 2
	      //* Part 3.  Estimation of ZXYDI
	      //* Initial setting
              SMPEF = 0.0;
              SMWTF = 0.0;
              SMPEI = 0.0;
              SMWTI = 0.0;

	      //* Outer DO-loops with respect to the primary estimates in the x direction
              for (IPEX = 0; IPEX < 4; IPEX++)
	      {
                  IX1 = IX0 + IDLT[0][IPEX];
                  IX2 = IX0 + IDLT[1][IPEX];
                  IX3 = IX0 + IDLT[2][IPEX];
                  if ((IX1 < 0) || (IX2 < 0) || (IX3 < 0) ||
                      (IX1 >= NX0) || (IX2 >= NX0) || (IX3 >= NX0)) continue;

		  //* Retrieves the necessary values for estimating zxy in the x direction
                  X1 = XA[0][IPEX];
                  X2 = XA[1][IPEX];
                  X3 = XA[2][IPEX];
                  Z10 = ZI0A[0][IPEX];
                  Z20 = ZI0A[1][IPEX];
                  Z30 = ZI0A[2][IPEX];
                  CX1 = CXA[0][IPEX];
                  CX2 = CXA[1][IPEX];
                  CX3 = CXA[2][IPEX];
                  SX = SXA[IPEX];
                  SXX = SXXA[IPEX];
                  B00X = B00XA[IPEX];
                  B10 = B10A[IPEX];

		  //* Inner DO-loops with respect to the primary estimates in the y direction
                  for (IPEY = 0; IPEY < 4; IPEY++)
		  {
                      IY1 = IY0 + IDLT[0][IPEY];
                      IY2 = IY0 + IDLT[1][IPEY];
                      IY3 = IY0 + IDLT[2][IPEY];
                      if ((IY1 < 0) || (IY2 < 0) || (IY3 < 0) ||
		          (IY1 >= NY0) || (IY2 >= NY0) || (IY3 >= NY0)) continue;

		      //* Retrieves the necessary values for estimating zxy in the y direction.
                      Y1 = YA[0][IPEY];
                      Y2 = YA[1][IPEY];
                      Y3 = YA[2][IPEY];
                      Z01 = Z0IA[0][IPEY];
                      Z02 = Z0IA[1][IPEY];
                      Z03 = Z0IA[2][IPEY];
                      CY1 = CYA[0][IPEY];
                      CY2 = CYA[1][IPEY];
                      CY3 = CYA[2][IPEY];
                      SY = SYA[IPEY];
                      SYY = SYYA[IPEY];
                      B00Y = B00YA[IPEY];
                      B01 = B01A[IPEY];

		      //* Selects and/or supplements the z values
                      if (NYDF >= 4)
		      {
                          Z11 = this->matrix_(IY1,IX1);
                          Z12 = this->matrix_(IY2,IX1);
                          Z13 = this->matrix_(IY3,IX1);
                          if (NXDF >= 4)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z22 = this->matrix_(IY2,IX2);
                              Z23 = this->matrix_(IY3,IX2);
                              Z31 = this->matrix_(IY1,IX3);
                              Z32 = this->matrix_(IY2,IX3);
                              Z33 = this->matrix_(IY3,IX3);
			  }
                          else if (NXDF == 3)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z22 = this->matrix_(IY2,IX2);
                              Z23 = this->matrix_(IY3,IX2);
                              Z31 = Z3F(X1,X2,X3,Z01,Z11,Z21);
                              Z32 = Z3F(X1,X2,X3,Z02,Z12,Z22);
                              Z33 = Z3F(X1,X2,X3,Z03,Z13,Z23);
			  }
                          else if (NXDF == 2)
			  {
                              Z21 = Z2F(X1,X2,Z01,Z11);
                              Z22 = Z2F(X1,X2,Z02,Z12);
                              Z23 = Z2F(X1,X2,Z03,Z13);
                              Z31 = Z2F(X1,X3,Z01,Z11);
                              Z32 = Z2F(X1,X3,Z02,Z12);
                              Z33 = Z2F(X1,X3,Z03,Z13);
			  }
		      }
                      else if (NYDF == 3)
		      {
                          Z11 = this->matrix_(IY1,IX1);
                          Z12 = this->matrix_(IY2,IX1);
                          Z13 = Z3F(Y1,Y2,Y3,Z10,Z11,Z12);
                          if (NXDF >= 4)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z22 = this->matrix_(IY2,IX2);
                              Z31 = this->matrix_(IY1,IX3);
                              Z32 = this->matrix_(IY2,IX3);
			  }
                          else if (NXDF == 3)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z22 = this->matrix_(IY2,IX2);
                              Z31 = Z3F(X1,X2,X3,Z01,Z11,Z21);
                              Z32 = Z3F(X1,X2,X3,Z02,Z12,Z22);
			  }
                          else if (NXDF == 2)
			  {
                              Z21 = Z2F(X1,X2,Z01,Z11);
                              Z22 = Z2F(X1,X2,Z02,Z12);
                              Z31 = Z2F(X1,X3,Z01,Z11);
                              Z32 = Z2F(X1,X3,Z02,Z12);
                          }

                          Z23 = Z3F(Y1,Y2,Y3,Z20,Z21,Z22);
                          Z33 = Z3F(Y1,Y2,Y3,Z30,Z31,Z32);
		      }
                      else if (NYDF == 2)
		      {
                          Z11 = this->matrix_(IY1,IX1);
                          Z12 = Z2F(Y1,Y2,Z10,Z11);
                          Z13 = Z2F(Y1,Y3,Z10,Z11);
                          if (NXDF >= 4)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z31 = this->matrix_(IY1,IX3);
			  }
                          else if (NXDF == 3)
			  {
                              Z21 = this->matrix_(IY1,IX2);
                              Z31 = Z3F(X1,X2,X3,Z01,Z11,Z21);
			  }
                          else if (NXDF == 2)
			  {
                              Z21 = Z2F(X1,X2,Z01,Z11);
                              Z31 = Z2F(X1,X3,Z01,Z11);
			  }

                          Z22 = Z2F(Y1,Y2,Z20,Z21);
                          Z23 = Z2F(Y1,Y3,Z20,Z21);
                          Z32 = Z2F(Y1,Y2,Z30,Z31);
                          Z33 = Z2F(Y1,Y3,Z30,Z31);
                      }

		      //* Calculates the primary estimate of partial derivative zxy as
		      //* the coefficient of the bicubic polynomial
                      DZXY11 = (Z11-Z10-Z01+Z00)/ (X1*Y1);
                      DZXY12 = (Z12-Z10-Z02+Z00)/ (X1*Y2);
                      DZXY13 = (Z13-Z10-Z03+Z00)/ (X1*Y3);
                      DZXY21 = (Z21-Z20-Z01+Z00)/ (X2*Y1);
                      DZXY22 = (Z22-Z20-Z02+Z00)/ (X2*Y2);
                      DZXY23 = (Z23-Z20-Z03+Z00)/ (X2*Y3);
                      DZXY31 = (Z31-Z30-Z01+Z00)/ (X3*Y1);
                      DZXY32 = (Z32-Z30-Z02+Z00)/ (X3*Y2);
                      DZXY33 = (Z33-Z30-Z03+Z00)/ (X3*Y3);
                      PEZXY = CX1* (CY1*DZXY11+CY2*DZXY12+CY3*DZXY13) +
                              CX2* (CY1*DZXY21+CY2*DZXY22+CY3*DZXY23) +
                              CX3* (CY1*DZXY31+CY2*DZXY32+CY3*DZXY33);

		      //* Calculates the volatility factor and distance factor in the x
		      //* and y directions for the primary estimate of zxy
                      B00 = (B00X+B00Y)/2.0;
                      SXY = SX*SY;
                      SXXY = SXX*SY;
                      SXYY = SX*SYY;
                      SXXYY = SXX*SYY;
                      SXYZ = X1* (Y1*Z11+Y2*Z12+Y3*Z13) +
                             X2* (Y1*Z21+Y2*Z22+Y3*Z23) +
                             X3* (Y1*Z31+Y2*Z32+Y3*Z33);
                      B11 = (SXYZ-B00*SXY-B10*SXXY-B01*SXYY)/SXXYY;
                      DZ00 = Z00 - B00;
                      DZ01 = Z01 - (B00+B01*Y1);
                      DZ02 = Z02 - (B00+B01*Y2);
                      DZ03 = Z03 - (B00+B01*Y3);
                      DZ10 = Z10 - (B00+B10*X1);
                      DZ11 = Z11 - (B00+B01*Y1+X1* (B10+B11*Y1));
                      DZ12 = Z12 - (B00+B01*Y2+X1* (B10+B11*Y2));
                      DZ13 = Z13 - (B00+B01*Y3+X1* (B10+B11*Y3));
                      DZ20 = Z20 - (B00+B10*X2);
                      DZ21 = Z21 - (B00+B01*Y1+X2* (B10+B11*Y1));
                      DZ22 = Z22 - (B00+B01*Y2+X2* (B10+B11*Y2));
                      DZ23 = Z23 - (B00+B01*Y3+X2* (B10+B11*Y3));
                      DZ30 = Z30 - (B00+B10*X3);
                      DZ31 = Z31 - (B00+B01*Y1+X3* (B10+B11*Y1));
                      DZ32 = Z32 - (B00+B01*Y2+X3* (B10+B11*Y2));
                      DZ33 = Z33 - (B00+B01*Y3+X3* (B10+B11*Y3));
                      VOLF = DZ00*DZ00 + DZ01*DZ01 + DZ02*DZ02 + DZ03*DZ03 +
                             DZ10*DZ10 + DZ11*DZ11 + DZ12*DZ12 + DZ13*DZ13 +
                             DZ20*DZ20 + DZ21*DZ21 + DZ22*DZ22 + DZ23*DZ23 +
                             DZ30*DZ30 + DZ31*DZ31 + DZ32*DZ32 + DZ33*DZ33;
                      DISF = SXX*SYY;

		      //* Calculates EPSLN
                      EPSLN = (Z00*Z00 + Z01*Z01 + Z02*Z02 + Z03*Z03 + 
			       Z10*Z10 + Z11*Z11 + Z12*Z12 + Z13*Z13 + 
                               Z20*Z20 + Z21*Z21 + Z22*Z22 + Z23*Z23 + 
			       Z30*Z30 + Z31*Z31 + Z32*Z32 + Z33*Z33) *
                               1.0E-12;

		      //* Accumulates the weighted primary estimates of zxy and their weights
                      if (VOLF > EPSLN)
		      {
		          //* - For a finite weight
                          WT = 1.0/ (VOLF*DISF);
                          SMPEF = SMPEF + WT*PEZXY;
                          SMWTF = SMWTF + WT;
		      }
                      else
		      {
		          //* - For an infinite weight
                          SMPEI = SMPEI + PEZXY;
                          SMWTI = SMWTI + 1.0;
		      }

                  } //end for IPEY
              } // end for IPEX

	      //* Calculates the final estimate of zxy
              if (SMWTI < 0.5)         //When no infinite weights exist
                  ZXYDI = SMPEF/SMWTF;
              else		       //When infinite weights exist
                  ZXYDI = SMPEI/SMWTI;

	      //* End of Part 3

              WKZX_[IY0][IX0]  = ZXDI;
              WKZY_[IY0][IX0]  = ZYDI;
              WKZXY_[IY0][IX0] = ZXYDI;

          } //end for IX0
      } //end for IY0
      return;
}

// A 7x7 area surround the point is searched

int Akima760::CheckMissingValues (int col, int lin) const
{
      int i,j,NXDF,NYDF;

      //* Initial setting of some local variables
      NXDF = std::max(4,this->matrix_.columns());
      NYDF = std::max(4,this->matrix_.rows());

      // Check a 7x7 area
      for (i = lin-3; i <= lin+3; i++)
      {
	      if ( i < 0 || i >= NYDF ) continue;

	      for (j = col-3; j <= col+3; j++)
	      {
		      if ( j < 0 || j >= NXDF ) continue;

		      if ( this->matrix_(i,j) == this->matrix_.missing() )
			      return 0;
	      }
      }

      return 1;
}

#if 0
// This subroutine is obsolet, no longer needed

void Akima760::rglctn(double XII, double YII, int& INXI, int& INYI) const
{
//*
//* Location of the desired points in a rectangular grid
//* (a supporting subroutine of the RGBI3P/RGSF3P subroutine package)
//*
//* Hiroshi Akima
//* U.S. Department of Commerce, NTIA/ITS
//* Version of 1995/08
//*
//* This subroutine locates the desired point in a rectangular grid
//* in the x-y plane.
//*
//* The grid lines can be unevenly spaced.
//*
//* The input arguments are
//*   NXD  = number of the input-grid data points in the x
//*          coordinate (must be 2 or greater),
//*   NYDF  = number of the input-grid data points in the y
//*          coordinate (must be 2 or greater),
//*   XD   = array of dimension NXD containing the x coordinates
//*          of the input-grid data points (must be in a
//*          monotonic increasing order),
//*   YD   = array of dimension NYDF containing the y coordinates
//*          of the input-grid data points (must be in a
//*          monotonic increasing order),
//*   XI   = x coordinate of the output point to be located
//*   YI   = y coordinate of the output point to be located
//*
//* The output arguments are
//*   INXI = integer where the interval number of the element is to be stored
//*   INYI = integer where the interval number of the element is to be stored
//*
//* The external arguments are
//*   NXD          = number of the input-grid data points in the x
//*                  coordinate (must be 2 or greater),
//*   NYDF         = number of the input-grid data points in the y
//*                  coordinate (must be 2 or greater),
//*   column       = array of dimension NXD containing the x coordinates
//*                  of the input-grid data points (must be in a
//*                  monotonic increasing order),
//*   row          = array of dimension NYDF containing the y coordinates
//*                  of the input-grid data points (must be in a
//*                  monotonic increasing order)
//*
//* The interval numbers are between 0 and NXD and between 0 and NYDF,
//* respectively.
//*
//* This routine was adapted to C++ by Fernando Ii, 02/04
//*
//*

      int    NXDF,NYDF;        // input matrix dimensions
      int       IMD,IMN,IMX;    // aixiliary variables

 
      //* Locates the output point by binary search. Determines INXI
      //* for which XII lies between XD(INXI) and XD(INXI+1)

//???????????????????????????
// IMPORTANT IMPORTANT IMPORTANT
      // THE OLD FORTRAN STYLE CODE ROUTINE IS RUNNING IN
      // PARALLEL WITH THE C++ CODE, TO MAKE SURE THAT THE
      // C++ CODE IS CORRECT. REMOVE THE FORTRAN STYLE CODE
      // LATER.
//???????????????????????????

      // IX/IY POINT TO 2 POSITIONS AHEAD IN THE ARRAY BECAUSE:
      // 1. FORTRAN STYLE INDEX (START WITH 1, INSTEAD OF 0)
      //    THIS IS COMPENSATE LATER IN THE CODE.
      // 2. ROUTINE 'lowerColumn' AND 'lowerRow' RETURNS THE
      //    'LOWER' VALUE INDEX, WHICH MEANS X(IX-1) OR Y(IY-1),
      //    INSTEAD OF X(IX) OR Y(IY)

      NXDF = this->matrix_.columns();
      NYDF = this->matrix_.rows();
      int ji;
      if (XII <= this->matrix_.regular_column(0))
              INXI = -1;
      else if (XII < this->matrix_.regular_column(NXDF-1))
      {
#if 1  //REMOVE LATER
              IMN = 0;
              IMX = NXDF-1;
              IMD = (IMN+IMX)/2;
ll:
              if (XII >= this->matrix_.regular_column(IMD))
		      IMN = IMD;
              else
                      IMX = IMD;

              IMD = (IMN+IMX)/2;
              if (IMD > IMN) goto ll;
	      INXI = IMD;
#endif
	      ji = this->matrix_.lowerColumn(XII);

#if 0  //REMOVE LATER
     if (JX1 != IX){
       MagLog::dev()<< "SERIOUS ERROR FINDING INDEXES" << endl;
       return -9999999.;
     }
#endif
//     MagLog::dev()<< XII << " " << INXI << " " << ji << endl;
       }
       else
              INXI = NXDF-1;

       //* Locates the output point by binary search. Determines INYI 
       //*for which YII lies between YD(INYI) and YD(INYI+1)
       if (YII <= this->matrix_.regular_row(0))
               INYI = -1;
       else if (YII < this->matrix_.regular_row(NYDF-1))
       {
#if 1  //REMOVE LATER
               IMN = 0;
               IMX = NYDF-1;
               IMD = (IMN+IMX)/2;
ll1:
	       if (YII >= this->matrix_.regular_row(IMD))
                      IMN = IMD;
               else
                      IMX = IMD;

               IMD = (IMN+IMX)/2;
               if (IMD > IMN) goto ll1;
               INYI = IMD;
#endif
	      ji = this->matrix_.lowerRow(YII);
#if 1  //REMOVE LATER
     if (ji != INYI){
       MagLog::dev()<< "SERIOUS ERROR FINDING INDEXES" << ji << " " <<  INYI << endl;
       return -9999999.;
     }
#endif
//      MagLog::dev()<< YII << " " << INYI << " " << ji << endl;
      	}
        else
               INYI = NYDF-1;

      return;
}
#endif


void Akima760::rgplnl(double XII, double YII, int IXDI, int IYDI, double& ZII) const
{

////*
////* Polynomials for rectangular-grid bivariate interpolation and
////* surface fitting
////* (a supporting subroutine of the RGBI3P/RGSF3P subroutine package)
////*
////* Hiroshi Akima
////* U.S. Department of Commerce, NTIA/ITS
////* Version of 1995/08
////*
////* This subroutine determines a polynomial in x and y for a rectangle
////* of the input grid in the x-y plane and calculates the z value for
////* the desired points by evaluating the polynomial for rectangular-
////* grid bivariate interpolation and surface fitting.
////*
////* The input arguments are
////*   NXD  = number of the input-grid data points in the x
////*          coordinate (must be 2 or greater),
////*   NYDF  = number of the input-grid data points in the y
////*          coordinate (must be 2 or greater),
////*   XD   = array of dimension NXDF containing the x coordinates
////*          of the input-grid data points (must be in a
////*          monotonic increasing order),
////*   YD   = array of dimension NYDF containing the y coordinates
////*          of the input-grid data points (must be in a
////*          monotonic increasing order),
////*   ZD   = two-dimensional array of dimension NXDF*NYDF
////*          containing the z(x,y) values at the input-grid data
////*          points,
////*   PDD  = three-dimensional array of dimension 3*NXDF*NYDF
////*          containing the estimated zx, zy, and zxy values
////*          at the input-grid data points,
////*   NIP  = number of the output points at which interpolation
////*          is to be performed,
////*   XII   = x coordinate of the output point,
////*   YII   = y coordinate of the output points,
////*   IXDXI = interval number of the input grid interval in the
////*          x direction where the x coordinate of the output
////*          point lie,
////*   IYDI = interval number of the input grid interval in the
////*          y direction where the y coordinate of the output
////*          point lie.
////*
////* The output argument is
////*   ZII   = interpolated z value at the output point.
////*
////* This routine was adapted to C++ by Fernando Ii, 02/04
////*
////*
//
//      
// Check for missing values: only need to check one of the three
// arrays (WKZX_,WKZY_,WKZXY_), since if there is a missing value
// within a 7x7 region surround the central point, all the three
// arrays point to a missing value.

      // Variables definition
      double   P00,P01,P02,P03,P10,P11;
      double   P12,P13,P20,P21,P22,P23,P30,P31,P32,P33,Q0,Q1,Q2;
      double   Q3,U,V,X0,Y0,Z00,Z01,Z0DX,Z0DY,Z10,Z11;
      double   Z1DX,Z1DY,ZDXDY,ZX00,ZX01,ZX0DY,ZX10,ZX11;
      double   ZX1DY,ZXY00,ZXY01,ZXY10,ZXY11,ZY00,ZY01,ZY0DX;
      double   ZY10,ZY11,ZY1DX;
      double   A;
      double   B;
      double   C;
      double   DF; 
      double   DX;
      double   DXSQ;
      double   DY; 
      double   DYSQ;
      int      IXD0,IXD1,IYD0,IYD1;
      int      NXDF,NYDF;

       //* Retrieves the z and partial derivative values at the origin of
       //* the coordinate for the rectangle
       NXDF = monoColumns_;
       NYDF = monoRows_;
       IXD0  = std::max(0,IXDI);
       IYD0  = std::max(0,IYDI);
       X0    = this->matrix_.regular_column(IXD0);
       Y0    = this->matrix_.regular_row(IYD0);
       Z00   = this->matrix_(IYD0,IXD0);
       ZX00  = WKZX_[IYD0][IXD0];
       ZY00  = WKZY_[IYD0][IXD0];
       ZXY00 = WKZXY_[IYD0][IXD0];
       if (ZX00 == missing()) 
       {
		ZII = missing();
		return;
       }

// CHECK THESE INDEXES LATER ????????????????????????????

       //* Case 1.  When the rectangle is inside the data area in both the
       //* x and y directions
       if (IXDI >= 0 && IXDI < NXDF-1 && IYDI >= 0 && IYDI < NYDF-1)
       {
	          //* Retrieves the z and partial derivative values at the other three
	          //* vertexes of the rectangle
                  IXD1 = IXD0 + 1;
                  DX = this->matrix_.regular_column(IXD1) - X0;
                  DXSQ = DX*DX;
                  IYD1 = IYD0 + 1;
                  DY = this->matrix_.regular_row(IYD1) - Y0;
                  DYSQ = DY*DY;
                  Z10 = this->matrix_(IYD0,IXD1);
                  Z01 = this->matrix_(IYD1,IXD0);
                  Z11 = this->matrix_(IYD1,IXD1);
                  ZX10 = WKZX_[IYD0][IXD1];
                  ZX01 = WKZX_[IYD1][IXD0];
                  ZX11 = WKZX_[IYD1][IXD1];
                  ZY10 = WKZY_[IYD0][IXD1];
                  ZY01 = WKZY_[IYD1][IXD0];
                  ZY11 = WKZY_[IYD1][IXD1];
                  ZXY10 = WKZXY_[IYD0][IXD1];
                  ZXY01 = WKZXY_[IYD1][IXD0];
                  ZXY11 = WKZXY_[IYD1][IXD1];
		  if (ZX10 == missing() || ZX01 == missing() || ZX11 == missing()) 
		  {
			 ZII = missing();
			 return;
		  }

		  //* Calculates the polynomial coefficients
                  Z0DX = (Z10-Z00)/DX;
                  Z1DX = (Z11-Z01)/DX;
                  Z0DY = (Z01-Z00)/DY;
                  Z1DY = (Z11-Z10)/DY;
                  ZX0DY = (ZX01-ZX00)/DY;
                  ZX1DY = (ZX11-ZX10)/DY;
                  ZY0DX = (ZY10-ZY00)/DX;
                  ZY1DX = (ZY11-ZY01)/DX;
                  ZDXDY = (Z1DY-Z0DY)/DX;
                  A = ZDXDY - ZX0DY - ZY0DX + ZXY00;
                  B = ZX1DY - ZX0DY - ZXY10 + ZXY00;
                  C = ZY1DX - ZY0DX - ZXY01 + ZXY00;
                  DF= ZXY11 - ZXY10 - ZXY01 + ZXY00;
                  P00 = Z00;
                  P01 = ZY00;
                  P02 = (2.0* (Z0DY-ZY00)+Z0DY-ZY01)/DY;
                  P03 = (-2.0*Z0DY+ZY01+ZY00)/DYSQ;
                  P10 = ZX00;
                  P11 = ZXY00;
                  P12 = (2.0* (ZX0DY-ZXY00)+ZX0DY-ZXY01)/DY;
                  P13 = (-2.0*ZX0DY+ZXY01+ZXY00)/DYSQ;
                  P20 = (2.0* (Z0DX-ZX00)+Z0DX-ZX10)/DX;
                  P21 = (2.0* (ZY0DX-ZXY00)+ZY0DX-ZXY10)/DX;
                  P22 = (3.0* (3.0*A-B-C)+DF)/ (DX*DY);
                  P23 = (-6.0*A+2.0*B+3.0*C-DF)/ (DX*DYSQ);
                  P30 = (-2.0*Z0DX+ZX10+ZX00)/DXSQ;
                  P31 = (-2.0*ZY0DX+ZXY10+ZXY00)/DXSQ;
                  P32 = (-6.0*A+3.0*B+2.0*C-DF)/ (DXSQ*DY);
                  P33 = (2.0* (2.0*A-B-C)+DF)/ (DXSQ*DYSQ);

	      //* Evaluates the polynomial
              U = XII - X0;
              V = YII - Y0;
              Q0 = P00 + V* (P01+V* (P02+V*P03));
              Q1 = P10 + V* (P11+V* (P12+V*P13));
              Q2 = P20 + V* (P21+V* (P22+V*P23));
              Q3 = P30 + V* (P31+V* (P32+V*P33));
              ZII = Q0 + U* (Q1+U* (Q2+U*Q3));
	  }
	  //* End of Case 1

	  //* Case 2.  When the rectangle is inside the data area in the x
	  //* direction but outside in the y direction
          else if ((IXDI >= 0 && IXDI < NXDF-1) && (IYDI < 0 || IYDI >= NYDF-1))
	  {
	      //* Retrieves the z and partial derivative values at the other
	      //* vertex of the semi-infinite rectangle
                  IXD1 = IXD0 + 1;
                  DX = this->matrix_.regular_column(IXD1) - X0;
                  DXSQ = DX*DX;
                  Z10 = this->matrix_(IYD0,IXD1);
                  ZX10 = WKZX_[IYD0][IXD1];
                  ZY10 = WKZY_[IYD0][IXD1];
                  ZXY10 = WKZXY_[IYD0][IXD1];
		  if (ZX10 == missing())
		  {
			 ZII = missing();
			 return;
		  }

		  //* Calculates the polynomial coefficients
                  Z0DX = (Z10-Z00)/DX;
                  ZY0DX = (ZY10-ZY00)/DX;
                  P00 = Z00;
                  P01 = ZY00;
                  P10 = ZX00;
                  P11 = ZXY00;
                  P20 = (2.0* (Z0DX-ZX00)+Z0DX-ZX10)/DX;
                  P21 = (2.0* (ZY0DX-ZXY00)+ZY0DX-ZXY10)/DX;
                  P30 = (-2.0*Z0DX+ZX10+ZX00)/DXSQ;
                  P31 = (-2.0*ZY0DX+ZXY10+ZXY00)/DXSQ;

	      //* Evaluates the polynomial
              U = XII - X0;
              V = YII - Y0;
              Q0 = P00 + V*P01;
              Q1 = P10 + V*P11;
              Q2 = P20 + V*P21;
              Q3 = P30 + V*P31;
              ZII = Q0 + U* (Q1+U* (Q2+U*Q3));
	  }
	  //* End of Case 2

	  //* Case 3.  When the rectangle is outside the data area in the x
	  //* direction but inside in the y direction
          else if ((IXDI < 0 || IXDI >= NXDF-1) && (IYDI >= 0 && IYDI < NYDF-1))
	  {
	      //* Retrieves the z and partial derivative values at the other
	      //* vertex of the semi-infinite rectangle
                  IYD1 = IYD0 + 1;
                  DY = this->matrix_.regular_row(IYD1) - Y0;
                  DYSQ = DY*DY;
                  Z01 = this->matrix_(IYD1,IXD0);
                  ZX01 = WKZX_[IYD1][IXD0];
                  ZY01 = WKZY_[IYD1][IXD0];
                  ZXY01 = WKZXY_[IYD1][IXD0];
		  if (ZX01 == missing())
		  {
			  ZII = missing();
			  return;
		  }

		  //* Calculates the polynomial coefficients
                  Z0DY = (Z01-Z00)/DY;
                  ZX0DY = (ZX01-ZX00)/DY;
                  P00 = Z00;
                  P01 = ZY00;
                  P02 = (2.0* (Z0DY-ZY00)+Z0DY-ZY01)/DY;
                  P03 = (-2.0*Z0DY+ZY01+ZY00)/DYSQ;
                  P10 = ZX00;
                  P11 = ZXY00;
                  P12 = (2.0* (ZX0DY-ZXY00)+ZX0DY-ZXY01)/DY;
                  P13 = (-2.0*ZX0DY+ZXY01+ZXY00)/DYSQ;

	      //* Evaluates the polynomial
              U = XII - X0;
              V = YII - Y0;
              Q0 = P00 + V* (P01+V* (P02+V*P03));
              Q1 = P10 + V* (P11+V* (P12+V*P13));
              ZII = Q0 + U*Q1;
	  }
	  //* End of Case 3

	  //* Case 4.  When the rectangle is outside the data area in both the
	  //* x and y direction
          else if ((IXDI < 0 || IXDI >= NXDF-1) && (IYDI < 0 || IYDI >= NYDF-1))
	  {
	      //* Calculates the polynomial coefficients
              P00 = Z00;
              P01 = ZY00;
              P10 = ZX00;
              P11 = ZXY00;

	      //* Evaluates the polynomial
              U = XII - X0;
              V = YII - Y0;
              Q0 = P00 + V*P01;
              Q1 = P10 + V*P11;
              ZII = Q0 + U*Q1;
	  }
	  //* End of Case 4

      return;
}



double Akima760::rgbi3p(double XI, double YI) const
{
//*
//* Rectangular-grid bivariate interpolation
//* (a master subroutine of the RGBI3P/RGSF3P subroutine package)
//*
//* Hiroshi Akima
//* U.S. Department of Commerce, NTIA/ITS
//* Version of 1995/08
//*
//* This subroutine performs interpolation of a bivariate function,
//* z(x,y), on a rectangular grid in the x-y plane.  It is based on
//* the revised Akima method.
//*
//* In this subroutine, the interpolating function is a piecewise
//* function composed of a set of bicubic (bivariate third-degree)
//* polynomials, each applicable to a rectangle of the input grid
//* in the x-y plane.  Each polynomial is determined locally.
//*
//* This subroutine has the accuracy of a bicubic polynomial, i.e.,
//* it interpolates accurately when all data points lie on a
//* surface of a bicubic polynomial.
//*
//* The grid lines can be unevenly spaced.
//*
//* The input arguments are
//*   MD  = mode of computation
//*       = 1 for new XD, YD, or ZD data (default)
//*       = 2 for old XD, YD, and ZD data,
//*   NXDF = number of the input-grid data points in the x
//*         coordinate (must be 2 or greater),
//*   NYDF = number of the input-grid data points in the y
//*         coordinate (must be 2 or greater),
//*   XD  = array of dimension NXDF containing the x coordinates
//*         of the input-grid data points (must be in a
//*         monotonic increasing order),
//*   YD  = array of dimension NYDF containing the y coordinates
//*         of the input-grid data points (must be in a
//*         monotonic increasing order),
//*   ZD  = two-dimensional array of dimension NXDF*NYDF
//*         containing the z(x,y) values at the input-grid data
//*         points,
//*   NIP = number of the output points at which interpolation
//*         of the z value is desired (must be 1 or greater),
//*   XI  = array of dimension NIP containing the x coordinates
//*         of the output points,
//*   YI  = array of dimension NIP containing the y coordinates
//*         of the output points.
//*
//* The output arguments are
//*   ZI  = array of dimension NIP where the interpolated z
//*         values at the output points are to be stored,
//*   IER = error flag
//*       = 0 for no errors
//*       = 1 for NXDF = 1 or less
//*       = 2 for NYDF = 1 or less
//*       = 3 for identical XD values or
//*               XD values out of sequence
//*       = 4 for identical YD values or
//*               YD values out of sequence
//*       = 5 for NIP = 0 or less.
//*
//* The other argument is
//*   WK  = three dimensional array of dimension 3*NXDF*NYDF used
//*         internally as a work area.
//*
//* The very fisrt call to this subroutine and the call with a new
//* XD, YD, and ZD array must be made with MD=1.  The call with MD=2
//* must be preceded by another call with the same XD, YD, and ZD
//* arrays.  Between the call with MD=2 and its preceding call, the
//* WK array must not be disturbed.
//*
//* The constant in the PARAMETER statement below is
//*   NIPIMX = maximum number of output points to be processed
//*            at a time.
//* The constant value has been selected empirically.
//*
//* This subroutine calls the RGLCTN and RGPLNL subroutines.
//*
//* This routine was adapted to C++ by Fernando Ii, 02/04
//*
//* Specification statements
//*     .. Parameters ..
//

//*   .. Scalar Arguments ..
//      int          IER,MD,NIP,NXDF,NYDF;

//*   .. Array Arguments ..
//      double       WK[NYDF][NXDF][3],XD[NXDF],XI[NIP],YD[NYDF],YI[NIP],
//                   ZD[NYDF][NXDF],ZI[NIP];

      int    INXI,INYI;  // input point
      double ZI;         // interpolated value

      cout << XI << ", " << YI << endl;
      if ( XI == 0 && YI > 97)
        cout << "STOP" << endl;
      // Locates the output point
      // It is not called anymore, but it is kept here
      // for test purpose
      //int col, row;
      //rglctn(XI,YI,col,row );

      if (XI <= this->matrix_.regular_column(0)) INXI = -1;
      else if (XI < this->matrix_.regular_column(this->matrix_.columns()-1)) INXI = this->matrix_.lowerColumn(XI);
      else INXI = this->matrix_.columns()-1;

      if (YI <= this->matrix_.regular_row(0)) INYI = -1;
      else if (YI < this->matrix_.regular_row(this->matrix_.rows()-1)) INYI = this->matrix_.lowerRow(YI);
      else INYI = this->matrix_.rows()-1;

//MagLog::dev()<< XI << " " << YI << "   " << INXI << " " << INYI << "   " << col << " " << row << endl;

      // Calculates the z values at the output point
      rgplnl(XI,YI,INXI,INYI,ZI);

      return ZI;
}



int Akima760::rowIndex(double row) const
{
     map<double, int>::const_iterator i = rowsMap_.find(row);
     if ( i != rowsMap_.end() ) return (*i).second;
     return -1;
}



int Akima760::columnIndex(double row) const
{
      map<double, int>::const_iterator i = columnsMap_.find(row);
      if ( i != columnsMap_.end() ) return (*i).second;
      return -1;
}


void Akima760::boundRow(double r, double& row1, int& index1, double& row2, int& index2) const
{
//	unsigned int  nb =  rows_.size();

	// first test if increasing!
  if ( same(r, rows_.back())  ) {
    index1 = index2 =  -1;
    row1 = row2 = rows_.back();
    return;
  }

	if ( rows_.back() - rows_.front() > 0 ) {
		index2 = 0;
		while ( index2 < rows_.size() && rows_[index2] < r ) {
			index2++;
		}

		index1 = (index2) ? index2-1 : 0;
		row1 = rows_[index1];
		row2 = rows_[index2];
	}
	else {
		index1 = 0;
		while ( index1 < rows_.size() && rows_[index1] >  r ) {
			index1++;
		}
		index2 = (index1 == rows_.size() -1 ) ? index1 :  index1+1;
		row1 = rows_[index1];
		row2 = rows_[index2];
	}
}



void Akima760::boundColumn(double r, double& column1, int& index1, double& column2, int& index2) const
{
//	unsigned int  nb =  rows_.size();
 if ( same(r, columns_.back())  ) {
    index1 = index2 =  -1;
    return;
  }
	// first test if increasing!
	if ( columns_.back() - columns_.front() > 0 )
	{
		index2 = 0;
		while ( index2 < columns_.size() && columns_[index2] < r )
		{
				index2++;
		}
		index1 = (index2) ? index2-1 : 0;
		column1 = columns_[index1];
		column2 = columns_[index2];
	}
	else {
		index1 = 0;
		while (  index1 < columns_.size() && columns_[index1] >  r )
		{
			index1++;
		}
		index2 = (index1 == columns_.size() -1 ) ? index1 : index1+1;
		column1 = columns_[index1];
		column2 = columns_[index2];
	}
}
