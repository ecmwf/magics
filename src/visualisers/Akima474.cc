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

/*! \file Akima474.cc
    Implementation of Akima474 class.
    
    Magics Team - ECMWF 2004
   
    Created: Wed 14-Apr-2004
    
*/

#include "Akima474Method.h"
#include "MagLog.h"
#include "Timer.h"
//#include "Filter.h"  //test, remove later

using namespace magics;


Akima474::Akima474(const AbstractMatrix& matrix, const Akima474MethodAttributes& attr) :
    MatrixHandler(matrix),
    mono_(matrix), 
//    matrix1_(mono_), 
//    mono_(matrix1_),
  
    attr_(attr)
{
      // Compute matrix output sizes
	double aux = (mono_.regular_column( mono_.columns()-1) - mono_.regular_column(0)) / attr_.resolutionX_;
       if ( (double)(int(aux)) != aux ) aux += 1.; //next integer number
       ncols_ = int(aux + 1.);                  //must include the first and the last input coordinates

       aux = (mono_.regular_row( mono_.rows()-1) - mono_.regular_row(0)) / attr_.resolutionY_;
       if ( (double)(int(aux)) != aux ) aux += 1.; //next integer number
       nrows_ = int(aux + 1.);                  //must include the first and the last input coordinates

       // Check for missing values
       missingValues_ = mono_.hasMissingValues() ? true : false;

//Test. If this is ok create a new function setRowAxis
#if 0
int ind;
double val;
rowsAxis_.reserve(nrows_);
for (ind = 0; ind < nrows_; ind++)
{
	val = ind*attr_.resolutionY_ + mono_.regular_row(0);
        rowsAxis_.push_back(val);
}
columnsAxis_.reserve(ncols_);
for (ind = 0; ind < ncols_; ind++)
{
	val = ind*attr_.resolutionX_ + mono_.regular_column(0);
        columnsAxis_.push_back(val);
}
#endif
}


double Akima474::regular_row(int i) const
{
//Remove later. Why this function is called so many times ???
//static long itest=0;
//MagLog::debug() << "Akima474 row=" << itest++ << "\n";

	return (i*attr_.resolutionY_ + mono_.regular_row(0));
//	return rowsAxis_[i];  //this could make it faster
}


double Akima474::row(int i, int) const
{


	return regular_row(i);

}


double Akima474::regular_column(int j) const
{ 
//Remove later. Why this function is called so many times ???
//static long jtest=0;
//MagLog::debug() << "Akima474 column=" << jtest++ << "\n";

	return (j*attr_.resolutionX_ + mono_.regular_column(0));
//	return columnsAxis_[j];  //this could make it faster
}


double Akima474::column(int, int j) const
{


	return regular_column(j);

}


double Akima474::operator()(int  i, int  j) const
{
#if 0
         // Calculate coordinates of (l,c) in the output matrix
	 double col = (regular_column(j) - mono_.regular_column(0)) / Iresx_;
	 double lin = (regular_row(i) - mono_.regular_row(0)) / Iresy_;

	 // Compute interpolated value
	 return InterpolateBicubicAt(lin,col);
#else
//MagLog::dev()<< i << " " << j << " " << regular_row(i) << " " << regular_column(j) << endl;
	return itplbv(regular_column(j),regular_row(i));

#endif
}


double Akima474::itplbv (double col, double lin) const
{

//     ALGORITHM 474 COLLECTED ALGORITHMS FROM ACM.
//     ALGORITHM APPEARED IN COMM. ACM, VOL. VV, NO. NN,
//     P. 000.
//      SUBROUTINE ITPLBV(LX, LY, X, Y, Z, N, U, V, W)
// BIVARIATE INTERPOLATION
// THIS SUBROUTINE INTERPOLATES, FROM VALUES OF THE FUNCTION
// GIVEN AT INPUT GRID POINTS IN AN X-Y PLANE AND FOR A GIVEN
// SET OF POINTS IN THE PLANE, THE VALUES OF A SINGLE-VALUED
// BIVARIATE FUNCTION Z = Z(X,Y).
// THE METHOD IS BASED ON A PIECE-WISE FUNCTION COMPOSED OF
// A SET OF BICUBIC POLYNOMIALS IN X AND Y.  EACH POLYNOMIAL
// IS APPLICABLE TO A RECTANGLE OF THE INPUT GRID IN THE X-Y
// PLANE.  EACH POLYNOMIAL IS DETERMINED LOCALLY.
//
// This routine was adapted to C++ by Fernando Ii, 05/04
//
// THE INPUT PARAMETERS ARE:
// LX  = NUMBER OF INPUT GRID POINTS IN THE X COORDINATE
//       (MUST BE 2 OR GREATER)
// LY  = NUMBER OF INPUT GRID POINTS IN THE Y COORDINATE
//       (MUST BE 2 OR GREATER)
// X   = ARRAY OF DIMENSION LX STORING THE X COORDINATES
//       OF INPUT GRID POINTS (IN ASCENDING ORDER)
// Y   = ARRAY OF DIMENSION LY STORING THE Y COORDINATES
//       OF INPUT GRID POINTS (IN ASCENDING ORDER)
// Z   = DOUBLY-DIMENSIONED ARRAY OF DIMENSION (LX,LY)
//       STORING THE VALUES OF THE FUNCTION (Z VALUES)
//       AT INPUT GRID POINTS
// N   = NUMBER OF POINTS AT WHICH INTERPOLATION OF THE
//       Z VALUE IS DESIRED (MUST BE 1 OR GREATER)
// U   = ARRAY OF DIMENSION N STORING THE X COORDINATES
//       OF DESIRED POINTS
// V   = ARRAY OF DIMENSION N STORING THE Y COORDINATES
//       OF DESIRED POINTS
// THE OUTPUT PARAMETER IS
// W   = ARRAY OF DIMENSION N WHERE THE INTERPOLATED Z
//       VALUES AT DESIRED POINTS ARE TO BE DISPLAYED
//       SOME VARIABLES INTERNALLY USED ARE
// ZA  = DIVIDED DIFFERENCE OF Z WITH RESPECT TO X
// ZB  = DIVIDED DIFFERENCE OF Z WITH RESPECT TO Y
// ZAB = SECOND ORDER DIVIDED DIFFERENCE OF Z WITH
//       RESPECT TO X AND Y
// ZX  = PARTIAL DERIVATIVE OF Z WITH RESPECT TO X
// ZY  = PARTIAL DERIVATIVE OF Z WITH RESPECT TO Y
// ZXY = SECOND ORDER PARTIAL DERIVATIVE OF Z WITH
//       RESPECT TO X AND Y

// DECLARATION STATEMENTS
      double a, b, c, d, e;
      double Z33;

      double ZA[2][5], ZB[5][2], ZAB[3][3], ZX[4][4], ZY[4][4], ZXY[4][4];
      double &Z3A1=ZA[0][0], &Z3A2=ZA[0][1], &Z3A3=ZA[0][2], &Z3A4=ZA[0][3], &Z3A5=ZA[0][4],
	       &Z4A1=ZA[1][0], &Z4A2=ZA[1][1], &Z4A3=ZA[1][2], &Z4A4=ZA[1][3], &Z4A5=ZA[1][4];
      double &Z3B1=ZB[0][0], &Z3B2=ZB[1][0], &Z3B3=ZB[2][0], &Z3B4=ZB[3][0], &Z3B5=ZB[4][0],
	       &Z4B1=ZB[0][1], &Z4B2=ZB[1][1], &Z4B3=ZB[2][1], &Z4B4=ZB[3][1], &Z4B5=ZB[4][1];

      double &ZA2B2=ZAB[0][0], &ZA3B2=ZAB[0][1], &ZA4B2=ZAB[0][2],
	       &ZA2B3=ZAB[1][0], &ZA3B3=ZAB[1][1], &ZA4B3=ZAB[1][2],
	       &ZA2B4=ZAB[2][0], &ZA3B4=ZAB[2][1], &ZA4B4=ZAB[2][2];

      double &ZX33=ZX[1][1], &ZX43=ZX[1][2], &ZX34=ZX[2][1], &ZX44=ZX[2][2];
      double &ZY33=ZY[1][1], &ZY43=ZY[1][2], &ZY34=ZY[2][1], &ZY44=ZY[2][2];
      double &ZXY33=ZXY[1][1], &ZXY43=ZXY[1][2], &ZXY34=ZXY[2][1], &ZXY44=ZXY[2][2];

      double &P00=Z33, &P01=ZY33, &P10=ZX33, &P11=ZXY33;

//      double LX0,ZX(1)), (LXM1,ZX(4)), (LXM2,ZX(13)),
//     * (LXP1,ZX(16)), (LY0,ZY(1)), (LYM1,ZY(4)), (LYM2,ZY(13)),
//     * (LYP1,ZY(16)), (IX,ZXY(1)), (IY,ZXY(4)), (IXPV,ZXY(13)),
//     * (IYPV,ZXY(16))

//      int (IMN,JX), (IMX,JY), (JXM2,JX1), (JYM2,JY1)
//      double (UK,DX), (VK,DY)

//      double (A1,A5,B1,B5,ZX(2),A,Q0),
      double A1,A5,B1,B5,Q0;

//     * (A2,ZX(5),B,Q1), 
      double A2=0.,Q1; 

//    (A4,ZX(8),C,Q2)
      double A4=0.,Q2;

//    (B2,ZY(2),D,Q3),
      double B2=0.,Q3;

//     * (B4,ZY(14),E),
      double B4=0.;

//    (X2,ZX(3),A3SQ),
      double X2=0., A3SQ;

//    (X4,ZX(9)), (X5,ZX(12)),
      double X4,X5=0.;

//     * (Y2,ZX(14)), (Y4,ZY(3),B3SQ), (Y5,ZX(15),P02),
      double Y2=0.,Y4,B3SQ,Y5=0.,P02;

//     * (Z23,ZY(5),P03), (Z24,ZY(8),P12), (Z32,ZY(9),P13),
      double Z23 = 0.,P03 = 0.,Z24 = 0.,P12 = 0.,Z32 = 0.,P13 = 0.;

//     * (Z34,ZY(12),P20), (Z35,ZY(15),P21), (Z42,ZXY(2),P22),
      double Z34,P20,Z35=0.,P21,Z42=0.,P22;

//     * (Z43,ZXY(5),P23), (Z44,ZXY(3),P30), (Z45,ZXY(8),P31),
      double Z43,P23,Z44,P30,Z45=0.,P31;

//     * (Z53,ZXY(9),P32), (Z54,ZXY(12),P33), (W2,WY2,W4),
      double Z53=0.,P32,Z54=0.,P33,W2,WY2,W4;

//     * (W3,WY3,W1,W5), (WX2,ZXY(14)), (WX3,ZXY(15))
      double W3,WY3,W1,W5,WX2,WX3;

//      DIMENSION X(LX), Y(LY), Z(LX,LY), U(N), V(N), W(N)

      // Auxiliary variables
      int    IX,IY,JX,JY,JX1,JY1;
      int    JXM2,JXML,JYM2,JYML;
      int    LX,LY,LX0,LXM1,LXM2,LXP1,LY0,LYM1,LYM2,LYP1;
      int    INXI,INYI;
      double UK,VK,DX,DY,SW;
      
      double X3,Y3,A3,B3,ZX3B3,ZX4B3,ZY3A3,ZY4A3;
      
      double WK;

      // PRELIMINARY PROCESSING
      // SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES
      LX   = mono_.columns();
      LY   = mono_.rows();
      LX0  = LX;
      LXM1 = LX0 - 1;
      LXM2 = LXM1 - 1;
      LXP1 = LX0 + 1;
      LY0  = LY;
      LYM1 = LY0 - 1;
      LYM2 = LYM1 - 1;
      LYP1 = LY0 + 1;

      // ERROR CHECK
      if (LXM2 < 0 || LYM2 < 0)
      {
	      MagLog::debug() << "LX/LY = 1 OR LESS" << endl;
	      MagLog::debug() << LX << " " << LY << endl;
	      return mono_.missing();
      }

      // INITIAL SETTING OF PREVIOUS VALUES OF IX AND IY
//      IXPV = 0;
//      IYPV = 0;

      // ROUTINES TO LOCATE THE DESIRED POINT

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

      // TO FIND OUT THE IX VALUE FOR WHICH
      // (U(K).GE.X(IX-1)).AND.(U(K).LT.X(IX))

      UK = col;
      VK = lin;

      if (UK <= mono_.regular_column(0)) INXI = -1;
      else                       INXI = mono_.lowerColumn(UK);

      if (VK <= mono_.regular_row(0)) INYI = -1;
      else                    INYI = mono_.lowerRow(VK);

      if (LXM2 == 0) IX = 2;
      else if (UK >= mono_.regular_column(LX0-1)) IX = LXP1;
      else if (UK < mono_.regular_column(0)) IX = 1;
      else
      {
#if 0  //REMOVE LATER
	      int IMN,IMX;
	      IMN = 2;
	      IMX = LX0;
ll:
	      IX = (IMN+IMX)/2;
	      if (UK >= mono_.regular_column(IX-1))
	       IMN = IX + 1;
	      else
               IMX = IX;

	      if (IMX > IMN) goto ll;
	      IX = IMX;
	      JX1 = IX;
#endif
	      IX = mono_.lowerColumn(UK)+2;

#if 0  //REMOVE LATER
     if (JX1 != IX){
       MagLog::dev()<< "SERIOUS ERROR FINDING INDEXES" << endl;
       return -9999999.;
     }
#endif
//      MagLog::dev()<< UK << " " << VK << "   " << IX << "   " << JX1 << endl;

      }
      // TO FIND OUT THE IY VALUE FOR WHICH
      // (V(K).GE.Y(IY-1)).AND.(V(K).LT.Y(IY))
      if (LYM2 == 0) IY = 2;
      else if (VK >= mono_.regular_row(LY0-1)) IY = LYP1;
      else if (VK < mono_.regular_row(0)) IY = 1;
      else
      {
#if 0  //REMOVE LATER
	      int IMN,IMX;
	      IMN = 2;
	      IMX = LY0;
ll1:
	      IY = (IMN+IMX)/2;
	      if (VK >= mono_.regular_row(IY-1))
	       IMN = IY + 1;
	      else
	       IMX = IY;

	      if (IMX > IMN) goto ll1;
	      IY = IMX;
	      JY1 = IY;
#endif
	      IY = mono_.lowerRow(VK)+2;
#if 0  //REMOVE LATER
     if (JY1 != IY){
       MagLog::dev()<< "SERIOUS ERROR FINDING INDEXES" << endl;
       return -9999999.;
     }
#endif
//      MagLog::dev()<< UK << " " << VK << "   " << IY << "   " << JY1 << endl;
      }

      // TO CHECK IF THE DESIRED POINT IS IN THE SAME RECTANGLE
      // AS THE PREVIOUS POINT.  IF YES, SKIP TO THE COMPUTATION
      // OF THE POLYNOMIAL
// maybe this is a good idea to increase performance
//  160   IF (IX.EQ.IXPV .AND. IY.EQ.IYPV) GO TO 690
//      IXPV = IX;
//      IYPV = IY;

      // ROUTINES TO PICK UP NECESSARY X, Y, AND Z VALUES, TO
      // COMPUTE THE ZA, ZB, AND ZAB VALUES, AND TO ESTIMATE THEM
      // WHEN NECESSARY
      JX = IX;
      if (JX == 1) JX = 2;
      if (JX == LXP1) JX = LX0;
      JY = IY;
      if (JY == 1) JY = 2;
      if (JY == LYP1) JY = LY0;
      JXM2 = JX - 2;
      JXML = JX - LX0;
      JYM2 = JY - 2;
      JYML = JY - LY0;

      // CHECK FOR MISSING VALUES
      if ( missingValues_ ) 
	      if (this->CheckMissingValues(JX,JY) == 0)
		      return this->missing();

      // IN THE CORE AREA, I.E., IN THE RECTANGLE THAT CONTAINS
      // THE DESIRED POINT
      X3 = mono_.regular_column(JX-2);
      X4 = mono_.regular_column(JX-1);

      A3 = 1.0/(X4-X3);
      Y3 = mono_.regular_row(JY-2);
      Y4 = mono_.regular_row(JY-1);
      B3 = 1.0/(Y4-Y3);
      Z33 = mono_(JY-2,JX-2);
      Z43 = mono_(JY-2,JX-1);
      Z34 = mono_(JY-1,JX-2);
      Z44 = mono_(JY-1,JX-1);
      Z3A3 = (Z43-Z33)*A3;
      Z4A3 = (Z44-Z34)*A3;
      Z3B3 = (Z34-Z33)*B3;
      Z4B3 = (Z44-Z43)*B3;
      ZA3B3 = (Z4B3-Z3B3)*A3;

      // IN THE X DIRECTION
      if (LXM2 == 0)
      {
	      Z3A2 = Z3A3;
	      Z4A2 = Z4A3;
	      Z3A4 = Z3A3 + Z3A3 - Z3A2;
	      Z4A4 = Z4A3 + Z4A3 - Z4A2;
	      goto l190;
      }
      else if (JXM2 != 0)
      {
	      X2 = mono_.regular_column(JX-3);
	      A2 = 1.0/(X3-X2);
	      Z23 = mono_(JY-2,JX-3);
	      Z24 = mono_(JY-1,JX-3);
	      Z3A2 = (Z33-Z23)*A2;
	      Z4A2 = (Z34-Z24)*A2;
	      if (JXML == 0)
	      {
		      Z3A4 = Z3A3 + Z3A3 - Z3A2;
		      Z4A4 = Z4A3 + Z4A3 - Z4A2;
		      goto l190;
	      }
      }
      X5 = mono_.regular_column(JX);
      A4 = 1.0/(X5-X4);
      Z53 = mono_(JY-2,JX);
      Z54 = mono_(JY-1,JX);
      Z3A4 = (Z53-Z43)*A4;
      Z4A4 = (Z54-Z44)*A4;
      if (JXM2 == 0)
      {
	      Z3A2 = Z3A3 + Z3A3 - Z3A4;
	      Z4A2 = Z4A3 + Z4A3 - Z4A4;
      }

l190:
      ZA2B3 = (Z4A2-Z3A2)*B3;
      ZA4B3 = (Z4A4-Z3A4)*B3;
      if (JX <= 3)
      {
	      Z3A1 = Z3A2 + Z3A2 - Z3A3;
	      Z4A1 = Z4A2 + Z4A2 - Z4A3;
      }
      else
      {
	      A1 = 1.0/(X2-mono_.regular_column(JX-4));
	      Z3A1 = (Z23-mono_(JY-2,JX-4))*A1;
	      Z4A1 = (Z24-mono_(JY-1,JX-4))*A1;
      }
 
      if (JX >= LXM1)
      {
	      Z3A5 = Z3A4 + Z3A4 - Z3A3;
	      Z4A5 = Z4A4 + Z4A4 - Z4A3;
      }
      else
      {
	      A5 = 1.0/(mono_.regular_column(JX+1)-X5);
	      Z3A5 = (mono_(JY-2,JX+1)-Z53)*A5;
	      Z4A5 = (mono_(JY-1,JX+1)-Z54)*A5;
      }

      // IN THE Y DIRECTION
      if (LYM2 == 0)
      {
	      Z3B2 = Z3B3;
	      Z4B2 = Z4B3;
	      Z3B4 = Z3B3 + Z3B3 - Z3B2;
	      Z4B4 = Z4B3 + Z4B3 - Z4B2;
	      goto l270;
      }
      else if (JYM2 != 0)
      {
	      Y2 = mono_.regular_row(JY-3);
	      B2 = 1.0/(Y3-Y2);
	      Z32 = mono_(JY-3,JX-2);
	      Z42 = mono_(JY-3,JX-1);
	      Z3B2 = (Z33-Z32)*B2;
	      Z4B2 = (Z43-Z42)*B2;
	      if (JYML == 0)
	      {
		      Z3B4 = Z3B3 + Z3B3 - Z3B2;
		      Z4B4 = Z4B3 + Z4B3 - Z4B2;
		      goto l270;
	      }
       }
       Y5 = mono_.regular_row(JY);
       B4 = 1.0/(Y5-Y4);
       Z35 = mono_(JY,JX-2);
       Z45 = mono_(JY,JX-1);
       Z3B4 = (Z35-Z34)*B4;
       Z4B4 = (Z45-Z44)*B4;
       if (JYM2 == 0)
       {
	       Z3B2 = Z3B3 + Z3B3 - Z3B4;
	       Z4B2 = Z4B3 + Z4B3 - Z4B4;
       }

l270:
       ZA3B2 = (Z4B2-Z3B2)*A3;
       ZA3B4 = (Z4B4-Z3B4)*A3;
       if (JY <= 3)
       {
	      Z3B1 = Z3B2 + Z3B2 - Z3B3;
	      Z4B1 = Z4B2 + Z4B2 - Z4B3;
       }
       else
       {
	      B1 = 1.0/(Y2-mono_.regular_row(JY-4));
	      Z3B1 = (Z32-mono_(JY-4,JX-2))*B1;
	      Z4B1 = (Z42-mono_(JY-4,JX-1))*B1;
       }

       if (JY >= LYM1)
       {
	      Z3B5 = Z3B4 + Z3B4 - Z3B3;
	      Z4B5 = Z4B4 + Z4B4 - Z4B3;
       }
       else
       {
	      B5 = 1.0/(mono_.regular_row(JY+1)-Y5);
	      Z3B5 = (mono_(JY+1,JX-2)-Z35)*B5;
	      Z4B5 = (mono_(JY+1,JX-1)-Z45)*B5;
       }
 
       // IN THE DIAGONAL DIRECTIONS
       if (LXM2 == 0)
       {
	       ZA2B2 = ZA3B2;
	       ZA4B2 = ZA3B2;
	       ZA2B4 = ZA3B4;
	       ZA4B4 = ZA3B4;
       }
       else if (LYM2 == 0)
       {
	       ZA2B2 = ZA2B3;
	       ZA2B4 = ZA2B3;
	       ZA4B2 = ZA4B3;
	       ZA4B4 = ZA4B3;
       }
       else if (JXML == 0)
       {
	       if (JYM2 == 0)
	       {
		       ZA2B4 = (Z3B4-(mono_(JY,JX-3)-Z24)*B4)*A2;
	               ZA2B2 = ZA2B3 + ZA2B3 - ZA2B4;
	       }
	       else
	       {
		       ZA2B2 = (Z3B2-(Z23-mono_(JY-3,JX-3))*B2)*A2;
	               if (JYML == 0)
			       ZA2B4 = ZA2B3 + ZA2B3 - ZA2B2;
		       else
			       ZA2B4 = (Z3B4-(mono_(JY,JX-3)-Z24)*B4)*A2;
	       }
	       ZA4B2 = ZA3B2 + ZA3B2 - ZA2B2;
	       ZA4B4 = ZA3B4 + ZA3B4 - ZA2B4;
       }
       else if (JYM2 == 0)
       {
	       ZA4B4 = ((mono_(JY,JX)-Z54)*B4-Z4B4)*A4;
	       ZA4B2 = ZA4B3 + ZA4B3 - ZA4B4;
               if (JXM2 == 0)
	       {
		       ZA2B2 = ZA3B2 + ZA3B2 - ZA4B2;
		       ZA2B4 = ZA3B4 + ZA3B4 - ZA4B4;
	       }
	       else
	       {
		       ZA2B4 = (Z3B4-(mono_(JY,JX-3)-Z24)*B4)*A2;
		       ZA2B2 = ZA2B3 + ZA2B3 - ZA2B4;
	       }
       }
       else
       {
	       ZA4B2 = ((Z53-mono_(JY-3,JX))*B2-Z4B2)*A4;
	       if (JYML == 0)
	       {
		       ZA4B4 = ZA4B3 + ZA4B3 - ZA4B2;
		       if (JXM2 == 0)
		       {
			       ZA2B2 = ZA3B2 + ZA3B2 - ZA4B2;
			       ZA2B4 = ZA3B4 + ZA3B4 - ZA4B4;
		       }
		       else
		       {
			       ZA2B2 = (Z3B2-(Z23-mono_(JY-3,JX-3))*B2)*A2;
			       ZA2B4 = ZA2B3 + ZA2B3 - ZA2B2;
		       }
	       }
	       else
	       {
		       ZA4B4 = ((mono_(JY,JX)-Z54)*B4-Z4B4)*A4;
		       if (JXM2 == 0)
		       {
			       ZA2B2 = ZA3B2 + ZA3B2 - ZA4B2;
			       ZA2B4 = ZA3B4 + ZA3B4 - ZA4B4;
		       }
		       else
		       {
			       ZA2B2 = (Z3B2-(Z23-mono_(JY-3,JX-3))*B2)*A2;
			       ZA2B4 = (Z3B4-(mono_(JY,JX-3)-Z24)*B4)*A2;
		       }
		}
       }

       //NUMERICAL DIFFERENTIATION   ---   TO DETERMINE PARTIAL
       // DERIVATIVES ZX, ZY, AND ZXY AS WEIGHTED MEANS OF DIVIDED
       // DIFFERENCES ZA, ZB, AND ZAB, RESPECTIVELY
       for (JY=2; JY <= 3; JY++)
       {
          for (JX=2; JX <= 3; JX++)
	  {
		  W2 = abs(ZA[JY-2][JX+1]-ZA[JY-2][JX]);
		  W3 = abs(ZA[JY-2][JX-1]-ZA[JY-2][JX-2]);
		  SW = W2 + W3;
                  if (SW == 0.0)
	          {
			  WX2 = 0.5;
			  WX3 = 0.5;
	          }
		  else
		  {
			  WX2 = W2/SW;
			  WX3 = W3/SW;
		  }

		  ZX[JY-1][JX-1] = WX2*ZA[JY-2][JX-1] + WX3*ZA[JY-2][JX];
		  W2 = abs(ZB[JY+1][JX-2]-ZB[JY][JX-2]);
		  W3 = abs(ZB[JY-1][JX-2]-ZB[JY-2][JX-2]);
		  SW = W2 + W3;
		  if (SW == 0.0)
		  {
			  WY2 = 0.5;
			  WY3 = 0.5;
		  }
		  else
		  {
			  WY2 = W2/SW;
			  WY3 = W3/SW;
		  }

		  ZY[JY-1][JX-1]  = WY2*ZB[JY-1][JX-2] + WY3*ZB[JY][JX-2];
		  ZXY[JY-1][JX-1] = WY2*(WX2*ZAB[JY-2][JX-2] + WX3*ZAB[JY-2][JX-1]) + WY3*(WX2*ZAB[JY-1][JX-2] + WX3*ZAB[JY-1][JX-1]);
	  }
       }

       // WHEN (U(K).LT.X(1)).OR.(U(K).GT.X(LX))
       if (IX == LXP1 || IX == 1)
       {
          if (IX == LXP1)
          {
	       W4 = A2*(3.0*A3+A2);
	       W5 = 2.0*A3*(A3-A2) + W4;
	       for (JY = 2; JY <= 3; JY++)
	       {
		       ZX[JY-1][3] = (W4*ZA[JY-2][3] + W5*ZA[JY-2][4]) / (W4+W5);
		       ZY[JY-1][3] = ZY[JY-1][2] + ZY[JY-1][2] - ZY[JY-1][1];
		       ZXY[JY-1][3] = ZXY[JY-1][2] + ZXY[JY-1][2] - ZXY[JY-1][1];
	               for (JX = 2; JX <= 3; JX++)
		       {
			       ZX[JY-1][JX-1]  = ZX[JY-1][JX];
			       ZY[JY-1][JX-1]  = ZY[JY-1][JX];
			       ZXY[JY-1][JX-1] = ZXY[JY-1][JX];
		       }
		}

	       X3 = X4;
	       Z33 = Z43;
	       for (JY = 1; JY <= 5; JY++)
	             ZB[JY-1][0] = ZB[JY-1][1];

	       A3 = A2;
	       JX = 3;
          }
          else if (IX == 1)
          {
	       W2 = A4*(3.0*A3+A4);
	       W1 = 2.0*A3*(A3-A4) + W2;
	       for (JY = 2; JY <= 3; JY++)
	       {
		       ZX[JY-1][0] = (W1*ZA[JY-2][0] + W2*ZA[JY-2][1]) / (W1+W2);
		       ZY[JY-1][0] = ZY[JY-1][1] + ZY[JY-1][1] - ZY[JY-1][2];
		       ZXY[JY-1][0] = ZXY[JY-1][1] + ZXY[JY-1][1] - ZXY[JY-1][2];
		       for (JX1 = 2; JX1 <= 3; JX1++)
		       {
			       JX = 5 - JX1;
			       ZX[JY-1][JX-1] = ZX[JY-1][JX-2];
			       ZY[JY-1][JX-1] = ZY[JY-1][JX-2];
			       ZXY[JY-1][JX-1] = ZXY[JY-1][JX-2];
		       }
	        }

		X3 = X3 - 1.0/A4;
		Z33 = Z33 - Z3A2/A4;
		for (JY = 1; JY <= 5; JY++)
			ZB[JY-1][1] = ZB[JY-1][0];

		for (JY = 2; JY <= 4; JY++)
			ZB[JY-1][0] = ZB[JY-1][0] - ZAB[JY-2][0]/A4;

		A3 = A4;
		JX = 1;
          }

          ZA[0][2] = ZA[0][JX];
          for (JY = 1; JY <= 3; JY++)
	       ZAB[JY-1][1] = ZAB[JY-1][JX-1];
       }  

       // WHEN (V(K).LT.Y(1)).OR.(V(K).GT.Y(LY))

       if (IY == LYP1 || IY == 1)
       {
          if (IY == LYP1)
          {
	       W4 = B2*(3.0*B3+B2);
	       W5 = 2.0*B3*(B3-B2) + W4;
	       for (JX = 2; JX <= 3; JX++)
	       {
		       if (!( (JX == 3 && IX == LXP1) || (JX == 2 && IX == 1) ))
		       {
			       ZY[3][JX-1] = (W4*ZB[3][JX-2] + W5*ZB[4][JX-2]) / (W4+W5);
			       ZX[3][JX-1] = ZX[2][JX-1] + ZX[2][JX-1] - ZX[1][JX-1];
			       ZXY[3][JX-1] = ZXY[2][JX-1] + ZXY[2][JX-1] - ZXY[1][JX-1];
		       }

		       for (JY = 2; JY <= 3; JY++)
		       {
			       ZY[JY-1][JX-1] = ZY[JY][JX-1];
			       ZX[JY-1][JX-1] = ZX[JY][JX-1];
			       ZXY[JY-1][JX-1] = ZXY[JY][JX-1];
		       }
	       }

	       Y3 = Y4;
	       Z33 = Z33 + Z3B3/B3;
	       Z3A3 = Z3A3 + ZA3B3/B3;
	       Z3B3 = Z3B4;
	       ZA3B3 = ZA3B4;
	       B3 = B2;
          }
          else if (IY == 1)
          {
	       W2 = B4*(3.0*B3+B4);
	       W1 = 2.0*B3*(B3-B4) + W2;
	       for (JX = 2; JX <= 3; JX++)
	       {
		       if ( !( (JX == 3 && IX == LXP1) || (JX == 2 && IX == 1) ) )
		       {
			       ZY[0][JX-1] = (W1*ZB[0][JX-2] + W2*ZB[1][JX-2]) / (W1+W2);
			       ZX[0][JX-1] = ZX[1][JX-1] + ZX[1][JX-1] - ZX[2][JX-1];
			       ZXY[0][JX-1] = ZXY[1][JX-1] + ZXY[1][JX-1] - ZXY[2][JX-1];
		       }

		       for (JY1 = 2; JY1 <= 3; JY1++)
		       {
			       JY = 5 - JY1;
			       ZY[JY-1][JX-1] = ZY[JY-2][JX-1];
			       ZX[JY-1][JX-1] = ZX[JY-2][JX-1];
			       ZXY[JY-1][JX-1] = ZXY[JY-2][JX-1];
		       }
	        }

	       Y3 = Y3 - 1.0/B4;
	       Z33 = Z33 - Z3B2/B4;
	       Z3A3 = Z3A3 - ZA3B2/B4;
	       Z3B3 = Z3B2;
	       ZA3B3 = ZA3B2;
	       B3 = B4;
          }

         if (IX == 1 || IX == LXP1)
         {
		 JX = IX/LXP1 + 2;
		 JX1 = 5 - JX;
		 JY = IY/LYP1 + 2;
		 JY1 = 5 - JY;
		 ZX[JY-1][JX-1]  = ZX[JY-1][JX1-1] + ZX[JY1-1][JX-1] - ZX[JY1-1][JX1-1];
		 ZY[JY-1][JX-1]  = ZY[JY-1][JX1-1] + ZY[JY1-1][JX-1] - ZY[JY1-1][JX1-1];
		 ZXY[JY-1][JX-1] = ZXY[JY-1][JX1-1] + ZXY[JY1-1][JX-1] - ZXY[JY1-1][JX1-1];
	  }
       }

       // DETERMINATION OF THE COEFFICIENTS OF THE POLYNOMIAL

       ZX3B3 = (ZX34-ZX33)*B3;
       ZX4B3 = (ZX44-ZX43)*B3;
       ZY3A3 = (ZY43-ZY33)*A3;
       ZY4A3 = (ZY44-ZY34)*A3;
       a = ZA3B3 - ZX3B3 - ZY3A3 + ZXY33;
       b = ZX4B3 - ZX3B3 - ZXY43 + ZXY33;
       c = ZY4A3 - ZY3A3 - ZXY34 + ZXY33;
       d = ZXY44 - ZXY43 - ZXY34 + ZXY33;
       e = a + a - b - c;
       A3SQ = A3*A3;
       B3SQ = B3*B3;
       P02 = (2.0*(Z3B3-ZY33)+Z3B3-ZY34)*B3;
       P03 = (-2.0*Z3B3+ZY34+ZY33)*B3SQ;
       P12 = (2.0*(ZX3B3-ZXY33)+ZX3B3-ZXY34)*B3;
       P13 = (-2.0*ZX3B3+ZXY34+ZXY33)*B3SQ;
       P20 = (2.0*(Z3A3-ZX33)+Z3A3-ZX43)*A3;
       P21 = (2.0*(ZY3A3-ZXY33)+ZY3A3-ZXY43)*A3;
       P22 = (3.0*(a+e)+d)*A3*B3;
       P23 = (-3.0*e-b-d)*A3*B3SQ;
       P30 = (-2.0*Z3A3+ZX43+ZX33)*A3SQ;
       P31 = (-2.0*ZY3A3+ZXY43+ZXY33)*A3SQ;
       P32 = (-3.0*e-c-d)*B3*A3SQ;
       P33 = (d+e+e)*A3SQ*B3SQ;

       // COMPUTATION OF THE POLYNOMIAL
       DY = VK - Y3;
       Q0 = P00 + DY*(P01+DY*(P02+DY*P03));
       Q1 = P10 + DY*(P11+DY*(P12+DY*P13));
       Q2 = P20 + DY*(P21+DY*(P22+DY*P23));
       Q3 = P30 + DY*(P31+DY*(P32+DY*P33));
       DX = UK - X3;
       WK = Q0 + DX*(Q1+DX*(Q2+DX*Q3));

       return WK;
}


// The following points are searched:
//
//  -    -    X    X     -    -
//  -    X    X    X     X    -
//  X    X    X    X     X    X
//  X    X    X    X    col   X
//  -    X    X   lin    X    -
//  -    -    X    X     -    -
//

int Akima474::CheckMissingValues (int col, int lin) const
{
      const int NIND = 24;
      int i,ix,iy;

      int ind[NIND][2] = {              {-4,-2},{-4,-1},
			        {-3,-3},{-3,-2},{-3,-1},{-3,0},
			{-2,-4},{-2,-3},{-2,-2},{-2,-1},{-2,0},{-2,1},
			{-1,-4},{-1,-3},{-1,-2},{-1,-1},{-1,0},{-1,1},
			        { 0,-3},{ 0,-2},{ 0,-1},{ 0,0},
			                { 1,-2},{ 1,-1}
                        };

      for (i = 0; i < NIND; i++)
      {
	      ix = col + ind[i][1];
	      iy = lin + ind[i][0];
MagLog::dev()<< ix << " " << iy << " " << mono_.columns() << " " << mono_.rows() << endl;
	      if ( iy < 0 || iy >= mono_.rows() ||
		   ix < 0 || ix >= mono_.columns() )
		      continue;

	      if ( mono_(iy,ix) == mono_.missing() )
	      {
		      MagLog::debug() <<  mono_(iy,ix) << endl;
		      return 0;
	      }
      }

      return 1;
}

#if 0

double Akima474::InterpolateBicubicAt (double lin, double col) const
{
	int i,j;   // auxiliary variables

//remove later
#if 1
if ((col < 2.) || (col >= (float)(mono_.columns()-3)) || (lin < 2.) || (lin >= (float)(mono_.rows()-3)))
{
    int ll, cc;
    if (col < 1.)                        cc = 0;
    else if (col < 2.)                   cc = 1;
    else if (col >= mono_.columns()-2) cc = mono_.columns()-1;
    else if (col >= mono_.columns()-3) cc = mono_.columns()-2;
    else                                 cc = (int)col;
		
    if (lin < 1.)                      ll = 0;
    else if (lin < 2.)                 ll = 1;
    else if (lin >= mono_.rows()-2)  ll = mono_.rows()-1;
    else if (lin >= mono_.rows()-3)  ll = mono_.rows()-2;
    else                               ll = (int)lin;

    return mono_(ll,cc);
}
#endif

// put it back later
#if 0
	if ( (col < 0.) || (col >= mono_.columns()-1.) || (lin < 0.) || (lin >= mono_.rows()-1.) )
	{
		double ll, cc;
		if (col < 0.)                      cc = 0;
		else if (col >= mono_.columns()) cc = mono_.columns()-1;
		else                               cc = col;
		
		if (lin < 0.)                      ll = 0;
		else if (lin >= mono_.rows())    ll = mono_.rows()-1;
		else                               ll = lin;

		return mono_(ll,cc);
	}
#endif

	// Set coordinates
	int u = (int)col;
	int v = (int)lin;
	double dx = col - (double)u;
//F	dy = (double)v - y;
	double dy = lin - (double)v;

        // Set the vx and vy values
//F1
#if 0 
	double vx[6], vy[6];
	for(i=0; i<6; i++)
	{
		vx[i] = Iresx_/2. + (double)(u-2+i)*Iresx_;
//F		vy[i] = (float)(Y2() - ResolutionY()/2. - (float)(v-2+i)*ResolutionY());
		vy[i] = Iresy_/2. + (double)(v-2+i)*Iresy_;
	}
#endif
        // Define the coefficients of the polinomium q[j]
	double q[4];
	double vz[6], vzaux[6];

	dx *= Iresx_;
	for(j = 0; j < 6; j++)
	{
                // Set the z values
		for(i = 0; i < 6; i++)
			vz[i] = mono_(v-2+j,u-2+i);

//F1		if ( Def_polynom(vx,vz,q) == false )
		if ( Def_polynom(Iresx_,vz,q) == false )
		{
			MagLog::debug() << "Error at Akima474::InterpolateBicubicAt" << "\n";
			return mono_.missing();
		}

                // Calculate intermediate z values vzaux
		if (q[0] >= mono_.missing())
			vzaux[j] = mono_.missing();
		else
			vzaux[j] = q[0] + q[1]*dx + q[2]*dx*dx + q[3]*dx*dx*dx;
			
	}

	// Interpolate at opposite direction
//F1	if ( Def_polynom(vy,vzaux,q) == false )
	if ( Def_polynom(Iresy_,vzaux,q) == false )
	{
		 MagLog::debug() << "Error at Akima474::InterpolateBicubicAt" << "\n";
		 return mono_.missing();
	}
	dy *= Iresy_;

	double  zvalue;
	if ( q[0] >= mono_.missing() )
		zvalue = InterpolateAt(lin, col);
	else
		zvalue = q[0] + q[1]*dy + q[2]*dy*dy + q[3]*dy*dy*dy;

	return zvalue;
}


//F1bool Akima474::Def_polynom(double* x, double* y, double* p) const
bool Akima474::Def_polynom(double x, double* y, double* p) const
{
	short	i;
	double	m[6],  	// tangent vector
		auxd1,	// auxiliar variable
		d2,d3; 	// slope at points 2 e 3

//F1	if( x[3] == x[2] ) return false; // points with the same x value

	// Check the existence of dummy values among the y elements
	if( y[2] >= mono_.missing() || y[3] >= mono_.missing() )
	{
		p[0]=p[1]=p[2]=p[3]=mono_.missing();
		return true;
	}
	else if( y[0] >= mono_.missing() || y[1] >= mono_.missing() || y[4] >= mono_.missing() || y[5] >= mono_.missing() )
	{
		p[0] = y[2];
//F		p[1] = (y[3]-y[2])/(x[3]-x[2]);
		p[1] = (y[3]-y[2])/x;
		p[2] = 0.; p[3] = 0.; 

		return true;
	}
	
	// Calculates the slope d2 (in the point 2) and d3 (in the point 3)
	for (i = 0; i < 5; i++)
	{
//F1		if( x[i+1] == x[i] ) m[i] = (double)1.0e30;
//F1		else                 m[i] = (y[i+1]-y[i]) / (x[i+1]-x[i]);
		m[i] = (y[i+1]-y[i]) / x;
	}

	if(((m[0]==m[1])&&(m[2]==m[3])) || ((m[1]==m[2])&&(m[3]==m[4])))
	{
		d2 = .5 * (m[1]+m[2]);
		d3 = .5 * (m[2]+m[3]);
	}
	else
	{
		d2 = (fabs(m[3]-m[2])*m[1] + fabs(m[1]-m[0])*m[2]) /
		     (fabs(m[3]-m[2]) + fabs(m[1]-m[0]));

		d3 = (fabs(m[4]-m[3])*m[2] + fabs(m[2]-m[1])*m[3]) /
		     (fabs(m[4]-m[3]) + fabs(m[2]-m[1]));
	}
	
	//Define the polinomium coefficients	
//F1	auxd1 = x[3] - x[2];
	auxd1 = x;
	p[0]  = y[2];
	p[1]  = d2;
	p[2]  = (3.*m[2] - 2.*d2 - d3) / auxd1;
	p[3]  = (d2 + d3 - 2.*m[2]) / (auxd1*auxd1);

	return true;
}

// METHOD TO CALCULATE A Z VALUE USING BILINEAR INTERPOLATION

double Akima474::InterpolateAt (double lin, double col) const
{
	double	dx, dy, dx1, dy1, p11, p12, p21, p22;
	int	u, v;

	if (col < 0. || col >= (double)(mono_.columns()-1) || lin < 0. || lin >= (double)(mono_.rows()-1))
		return mono_.missing();

        // Compute weights
	u   = (int)col;
	v   = (int)lin;
	dx  = col - (double)u;
	dy  = lin - (double)v;
	dx1 = 1. - dx;
	dy1 = 1. - dy;

	// Retrieve points
	p11 = mono_(v,u);
	p12 = mono_(v+1,u);
	p21 = mono_(v,u+1);
	p22 = mono_(v+1,u+1);

	// Check for dummy values
	if (p11 >= mono_.missing())
	{
		if (p12 >= mono_.missing())
			return mono_.missing();
		else
		{
			if (p21 >= mono_.missing())
				return mono_.missing();
			else
			{
				if (p22 >= mono_.missing())
					return mono_.missing();
				else
					p11 = (p12 + p21 + p22) / 3.;
			}
		}
	}
	else
	{
		if (p12 >= mono_.missing())
		{
			if (p21 >= mono_.missing())
				return mono_.missing();
			else
			{
				if (p22 >= mono_.missing())
					return mono_.missing();
				else
					p12 = (p11 + p21 + p22) / 3.;
			}
		}
		else
		{
			if (p21 >= mono_.missing())
			{
				if (p22 >= mono_.missing())
					return mono_.missing();
				else
					p21 = (p11 + p12 + p22) / 3.;
			}
			else
				if (p22 >= mono_.missing())
					p22 = (p11 + p12 + p21) / 3.;
		}
	}

	// Interpolation
	p11 = p11*dy1 + p12*dy;
	p22 = p21*dy1 + p22*dy;

	return(p11 * dx1 + p22 * dx);
}
#endif

//TEST, REMOVE LATER
#if 0
#define   NXI 19
#define   NYI 23

void Akima474::test_build_data()
{

float     XIMN=-0.5;
float	  XIMX= 8.5;
float     YIMN=-0.5;
float 	  YIMX=10.5;

double    XI[NXI],YI[NYI];
double    ZI[NYI][NXI];      

      float     ANXIM1,ANYIM1,DXI,DYI;
      int    ISEC,IXD,IXI,IXIMN,IXIMX,IYD,IYI,IYIR,MD,NXD1,NYD1;

      //     ..Local Arrays ..
      double     DZI[NYI][NXI];

      // Initializations

     double ZIE[NYI][NXI] = { -.847,-.533,
                      -.274,-.117,-.031,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .401,.250,.119,.043,.011,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,-.665,-.376,-.143,-.033,-.007,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
		      .000,.000,.000,.000,
		      .000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,2.449,
                      1.368,.537,.149,.025,.000,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      5.083,3.200,1.642,.700,.187,.000,.000,.000,.000,
                      .000,.000,.000,.000,.000,.000,.000,.000,.000,
                      .000,6.588,5.234,3.878,2.542,1.188,.253,.026,
                      .026,.007,.000,.000,.000,.000,.000,.000,.000,
                      .000,.000,.000,8.017,7.400,6.400,4.800,2.963,
                      1.400,.457,.100,.027,.000,.000,.000,.000,.000,
		      .000,.000,.000,.000,.000,
		      11.055,
                      9.670,8.083,6.305,4.786,3.421,2.043,1.112,.565,
                      .131,-.019,.000,.000,.000,.000,.000,.000,.000,
                      .000,14.492,12.000,9.746,8.000,6.594,5.300,4.081,
                      2.900,1.697,.600,.059,.000,.000,.000,.000,.000,
                      .000,.000,.000,15.999,14.376,12.657,10.774,8.620,
                      6.659,5.291,4.392,3.926,3.005,1.223,.139,.051,
                      .025,.009,.000,.000,.000,-.005,15.525,16.800,
                      16.749,14.400,10.956,8.100,6.735,6.900,7.298,
                      6.200,3.010,.600,.248,.100,.024,.000,.006,.000,
                      -.025,15.876,19.280,20.563,17.856,13.242,10.219,
                      10.577,11.999,10.170,7.053,5.198,3.543,1.831,
		      .350,-.130,.168,.408,.168,-.224,
		      17.700,
                      21.800,23.531,20.500,15.087,12.800,15.817,17.600,
                      11.477,5.800,6.988,7.600,4.410,.800,-.392,.600,
                      1.261,.600,-.417,17.913,22.788,24.944,21.881,
                      16.302,14.382,18.557,20.807,11.916,4.561,7.327,
                      8.518,5.133,1.284,-.013,1.201,1.998,1.200,-.065,
                      16.383,22.400,25.330,22.500,16.796,14.600,19.172,
                      22.500,13.159,4.700,6.689,7.200,4.392,1.800,
                      1.150,2.100,2.734,2.100,1.025,18.109,26.756,
                      31.311,28.143,21.004,18.237,24.236,28.979,17.970,
                      7.469,10.467,11.985,9.022,6.833,6.901,8.292,
                      9.186,8.524,7.101,24.667,37.200,44.007,40.000,
                      30.508,27.000,34.974,41.300,27.136,14.100,20.473,
                      24.500,20.557,17.300,17.639,20.200,21.826,20.800,
		      18.458,
		      33.414,
                      48.009,56.017,51.561,40.817,36.922,45.856,52.860,
                      37.376,23.200,30.839,36.192,31.969,28.037,28.437,
                      31.604,33.579,32.332,29.561,44.842,58.200,65.537,
                      61.500,51.657,47.900,55.899,62.300,47.891,34.600,
                      41.239,45.500,41.479,38.200,38.591,41.200,42.823,
                      41.700,39.192,58.284,68.917,74.644,71.333,63.413,
                      60.125,66.293,71.400,59.129,47.725,52.451,54.592,
		      50.842,48.483,48.639,50.142,51.089,50.200,48.268 };

	int i,j;

//*     ..
//* Calculation
//* Opens the output file and writes the input data.
//
      NXD1 = mono_.columns();
      NYD1 = mono_.rows();
      printf("\nPRG3P  Original Data  ZD(YD,XD)\n\n       ");
      for (IXD = 0; IXD < NXD1; IXD++) printf("%7.1f",mono_.regular_column(IXD));
      for (IYD = NYD1-1; IYD >= 0; IYD--)
      {
	      printf("\n%7.1f",mono_.regular_row(IYD));
	      for (IXD = 0; IXD < NXD1; IXD++) printf("%7.1f",mono_(IYD,IXD));
      }

//* Program check for the RGBI3P subroutine
//* - Performs interpolation and calculates the differences.
      DXI = XIMX - XIMN;
      ANXIM1 = NXI - 1;
      printf("\n\nXI %d\n",NXI);
      for (IXI = 0; IXI < NXI; IXI++)
      {
	      XI[IXI] = XIMN + DXI*float(IXI)/ANXIM1;
	      printf("%5.1f",XI[IXI]);
      }

      DYI = YIMX - YIMN;
      ANYIM1 = NYI - 1;
      printf("\n\nYI %d\n",NYI);
      for (IYI = 0; IYI < NYI; IYI++)
      {
	      YI[IYI] = YIMN + DYI*float(IYI)/ANYIM1;
	      printf("%5.1f",YI[IYI]);
      }

      printf("\n\n PROCESSING\n");

      for (IYI = 0; IYI < NYI; IYI++)
      {
	      for (IXI = 0; IXI < NXI; IXI++)
	      {
		      InterpolateBicubicAt(XI[IXI],YI[IYI],ZI[IYI][IXI]);
		      DZI[IYI][IXI] = ZI[IYI][IXI] - ZIE[IYI][IXI];
	      }
      }

//* - Writes the calculated results.
      printf("\n\n\nTPRG3P  Program Check for RGBI3P   Calculated ZI Values\n");
      for (ISEC = 1; ISEC < 3; ISEC++)
      {
	      if (ISEC == 1)
	      {
		  IXIMN = 0;
		  IXIMX = 11;
	      }
	      else
	      {
		  IXIMN = 8;
		  IXIMX = NXI;
	      }

	      printf("\n\n                ZI(XI,YI)\n\n     ");
	      for (IXI = IXIMN; IXI < IXIMX; IXI++)
		  printf("%7.1f",XI[IXI]);

	      for (IYI = NYI-1; IYI >= 0; IYI--)
	      {
		      printf("\n%7.1f",YI[IYI]);
		      for (IXI = IXIMN; IXI < IXIMX; IXI++)
			      printf("%7.2f",ZI[IYI][IXI]);
	      }
      }

//* - Writes the differences.
      printf("\n\n\nTPRG3P  Program Check for RGBI3P   Differences\n");
      for (ISEC = 1; ISEC < 3; ISEC++)
      {
	      if (ISEC == 1)
	      {
		  IXIMN = 0;
		  IXIMX = 11;
	      }
	      else
	      {
		  IXIMN = 8;
		  IXIMX = NXI;
	      }

	      printf("\n\n           DZI(YI,XI)\n\n       ");
	      for (IXI = IXIMN; IXI < IXIMX; IXI++)
		  printf("%7.1f",XI[IXI]);

	      for (IYI = NYI-1; IYI >= 0; IYI--)
	      {
		      printf("\n%7.1f",YI[IYI]);
		      for (IXI = IXIMN; IXI < IXIMX; IXI++)
			      printf("%7.2f",DZI[IYI][IXI]);
	      }
      }

      printf("\n\nEND TEST\n");
}

// to test high/low

void Akima474::test_build_data_hl()
{
	Matrix matrix3(11, 9);
#define   NXD 9
#define   NYD 11
	double     XD[NXD] = { 0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0 };
	double     YD[NYD] = { 0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0 };
	double     ZD[NYD][NXD] = { 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				    0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				    0.0,0.0,-3.0,0.0,0.0,0.0,0.0,0.0,0.0,
				    3.2,0.7,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
				    7.4,4.8,1.4,0.1,0.0,0.0,0.0,0.0,0.0,
				    12.0,8.0,5.3,2.9,0.6,0.0,0.0,0.0,0.0,
				    16.8,14.4,8.1,6.9,6.2,0.6,0.1,0.0,0.0,
				    21.8,20.5,12.8,17.6,5.8,7.6,0.8,0.6,0.6,
				    22.4,22.5,14.6,22.5,4.7,7.2,1.8,2.1,2.1,
				    37.2,40.0,27.0,41.3,14.1,24.5,17.3,20.2,20.8,
				    58.2,61.5,47.9,62.3,34.6,45.5,38.2,41.2,41.7 };

	int IXD,IYD;

            for (int r = 0; r < NYD; r++)
                for (int c = 0; c < NXD; c++) 
                    matrix3.push_back(ZD[r][c]);

            for (int r = 0; r < NYD; r++) matrix3.rowsAxis().push_back(YD[r]);
            for (int c = 0; c < NXD; c++) matrix3.columnsAxis().push_back(XD[c]);
            matrix3.setMapsAxis();

	   
	    MatrixHandler mm1(matrix3);

      int dim=5;
      MinMaxFilter fil(mm1,dim,dim);
      fil.test_build_data();
}
#endif
