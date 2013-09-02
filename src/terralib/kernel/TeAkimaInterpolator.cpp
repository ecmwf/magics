void TeFitLinearSurface (int idv, vector<TeTinVertex>& xyzd, vector<TePolyCoef>& cfl1)
{
// Least squares fit of a linear surface (plane) to z(x,y) values
// Adapted from SUBROUTINE SDLS1P(NDP,XD,YD,ZD,IPC,NCP,CFL1) by
// Hiroshi Akima, U.S. Department of Commerce, NTIA/ITS
// Version of 1995/05
// This subroutine performs the least squares fit of a linear
// surface (plane) to a data point set consisting of the data
// point in question and several data points closest to it.
// The input arguments are
//   IDV  = data point number,
//   XYZD = array of dimension NDP containing the x, y coordinates
//          and the z values at the data points,
// The output argument is
//   CFL1 = two-dimensional array of dimension 2*NDP, where
//          the coefficients (a10, a01) of the least squares
//          fit, first-degree polynomial calculated at the
//          IDPth data point.
	list<int> ipc;
	list<int>::iterator vi;
	TeTin::vertexSecondNeighVertices(idv, ipc);
	vi = ipc.begin();
	if ( ipc.size() == 1 )
	{
		double x2x1 = xyzd[*vi].x() - xyzd[idp].x();
		double y2y1 = xyzd[*vi].y() - xyzd[idp].y();
		double z2z1 = xyzd[*vi].value() - xyzd[idp].value();
		double daux = z2z1/(x2x1*x2x1 + y2y1*y2y1);
		cfl1[idv].a10( x2x1*daux );
		cfl1[idv].a01( y2y1*daux );
	}
	else
	{
		double sx(0.), sy(0.), sxx(0.), sxy(0.), syy(0.), sz(0.), sxz(0.), syz(0.);
		double x, y, z;
		int i, idpi = idv;
		for (i = 0; i < 10; i++)
		{
			if (vi == ipc.end())
				break;
			x = xyzd[idpi];
			y = xyzd[idpi];
			z = xyzd[idpi];
			sx += x;
			sy += y;
			sxx += (x * x);
			sxy += (x * y);
			syy += (y * y);
			sz += z;
			sxz += (x * z);
			syz += (y * z);
			idpi = *vi;
			vi++;
		}
		double an = (double) (i > ipc.size() ? ipc.size() : 10);
		double a11 = (an * sxx) - (sx * sx);
		double a12 = (an * sxy) - (sx * sy);
		double a22 = (an * syy) - (sy * sy);
		double b1 = (an * sxz) - (sx * sz);
		double b2 = (an * syz) - (sy * sz);
		double dlt = (a11 * a22) - (a12 * a12);
		cfl1[idv].a10( ((b1 * a22) - (b2 * a12))/dlt );
		cfl1[idv].a01( ((b2 * a11) - (b1 * a12))/dlt );
	}
}

bool TeCalcPolynomialCoef (int idv, vector<TeTinVertex>& xyzd, int** ipc,
						   vector<TePolyCoef>& cf, vector<int>& ncp, vector<int>& iord )
{
// Coefficients of the third-degree polynomial for z(x,y)
// Adapted from SUBROUTINE SDCF3P(NDP,XD,YD,ZD,IPC,CF,NCP,IORD) by
// Hiroshi Akima,  U.S. Department of Commerce, NTIA/ITS
// Version of 1995/05
// This subroutine calculates, for each data point, coefficients
// of the third-degree polynomial for z(x,y) fitted to the set of
// 10 data points consisting of the data point in question and
// nine data points closest to it.  When the condition number of
// the matrix associated with the 10 data point set is too large,
// this subroutine calculates coefficients of the second-degree
// polynomial fitted to the set of six data points consisting of
// the data point in question and five data points closest to it.
// When the condition number of the matrix associated with the six
// data point set is too large, this subroutine calculates
// coefficients of the first-degree polynomial fitted to the set of
// three data points closest to the data point in question.  When
// the condition number of the matrix associated with the three data
// point set is too large, this subroutine calculates coefficients
// of the first-degree polynomial fitted to the set of two data
// points consisting of the data point in question and one data
// point closest to it, assuming that the plane represented by the
// polynomial is horizontal in the direction which is at right
// angles to the line connecting the two data points.
// The input arguments are
//   IDV  = data point number,
//   XYZD = array containing the x y
//         coordinates and the z values at the data points,
// The output arguments are
//   CF  = coefficients of the polynomial
//         (a10, a20, a30, a01, a11, a21, a02, a12, a03)
//         calculated at the IDPth data point.
//   NCP = integer array of dimension NDP, where the numbers
//         of the closest points used are to be stored.
//   IORD = integer array of dimension NDP containing the
//          degree of the polynomial used to compute PDD.
// The constant in the first PARAMETER statement below is
//   CNRMX = maximum value of the ratio of the condition
//           number of the matrix associated with the point
//           set to the number of points.
// The constant value has been selected empirically.
	double cnrmx = 3.5e+7;	//      PARAMETER        (CNRMX=3.5E+07)
	cf[idp].Init(0.0);
	TeMatrix aa;
	aa.Init( 10, 10 );
	double cfi[10],	b[10], cn;
	list<int> ipc;
	list<int>::iterator vi;
	TeTin::vertexSecondNeighVertices(idv, ipc);
	int i, idpi = idv;
	if (ipc.size >= 9)	// Calculates the coefficients of the set of
	{	// linear equations with the 10-point data point set.
		vi = ipc.begin();
		for (i = 0; i < 10; i++)
		{
			x = xyzd[idpi].x();
			y = xyzd[idpi].y();
			aa(i,0) = 1.0;
			aa(i,1) = x;
			aa(i,2) = x * x;
			aa(i,3) = x * x * x;
			aa(i,4) = y;
			aa(i,5) = x * y;
			aa(i,6) = x * x * y;
			aa(i,7) = y * y;
			aa(i,8) = x * y * y;
			aa(i,9) = y * y * y;
			b[i] = xyzd[idpi].value();
			idpi = *vi;
			vi++;
		}
		if ( ! TeSolveLinearEquations (aa, b, cfi, cn);
		{
			if (cn < cnrmx * 10.)
			{
				cf[idp].a10 (cfi[1]);
				cf[idp].a20 (cfi[2]);
				cf[idp].a30 (cfi[3]);
				cf[idp].a01 (cfi[4]);
				cf[idp].a11 (cfi[5]);
				cf[idp].a21 (cfi[6]);
				cf[idp].a02 (cfi[7]);
				cf[idp].a12 (cfi[8]);
				cf[idp].a03 (cfi[9]);
				ncp[idp] = 9;
				iord[idp] = 3;
			}
		}
	}
	else if (ipc.size() >= 6)	// Calculates the coefficients of the set of
	{	// linear equations with the 6-point data point set.
		aa.Init( 6, 6 );
		idpi = idp;
		vi = ipc.begin();
		for ( i = 0; i < 6; i++)
		{
			x = xyzd[idpi].x();
			y = xyzd[idpi].y();
			aa[i,0] = 1.0;
			aa[i,1] = x;
			aa[i,2] = x * x;
			aa[i,3] = y;
			aa[i,4] = x * y;
			aa[i,5] = y * y;
			b[i] = xyzd[idpi].value();
			idpi = *vi;
			vi++;
		}
		if ( TeSolveLinearEquations (aa, b, cfi, cn) )
		{
			if (cn < cbrmx * 6.)
			{
				cf[idv].a10 (cfi[1]);
				cf[idv].a20 (cfi[2]);
				cf[idv].a01 (cfi[3]);
				cf[idv].a11 (cfi[4]);
				cf[idv].a02 (cfi[5]);
				ncp[idv] = 5;
				iord[idv] = 2;
			}
		}
	}
	else if (ipc.size() >= 3)	// Calculates the coefficients of the set of
	{	// linear equations/ with the 3-point data point set.
		aa.Init( 3, 3 );
		vi = ipc.begin();
		for (i = 0; i < 3; i++)
		{
			idpi = *vi;
			vi++;
			x = xyzd[idpi].x();
			y = xyzd[idpi].y();
			aa[i,0] = 1.0;
			aa[i,1] = x;
			aa[i,2] = y;
			b[i] = xyzd[idpi].value();
		}
		if ( TeSolveLinearEquations (aa, b, cfi, cn) )
		{
			if (cn < cbrmx * 3.)
			{
				cf[idv].a10 (cfi[1]);
				cf[idv].a01 (cfi[2]);
				ncp[idv] = 3;
				iord[idv] = 1;
			}
		}
	}
	else if (ipc.size() >= 1)	// Calculates the coefficients of the set of
	{// linear equations with the 2-point data point set when applicable.
		double x2x1 = xyzd[*vi].x() - xyzd[idp].x();
		double y2y1 = xyzd[*vi].y() - xyzd[idp].y();
		double z2z1 = xyzd[*vi].value() - xyzd[idp].value();
		double daux = z2z1/(x2x1*x2x1 + y2y1*y2y1);
		cf[idv].a10( x2x1*daux );
		cf[idv].a01( y2y1*daux );
		ncp[idp] = 1;
		iord[idp] = 0;
	}
	else
		return false;
	return true;
}										//      END

void TeCalcPartialDerivatives (int idv, vector<TeTinVertex>& xyzd, vector<TePartialDeriv>& pdd )
{
// Partial derivatives for bivariate interpolation and surface
// fitting for scattered data
// Adapted from SUBROUTINE SDPD3P(NDP,XD,YD,ZD,PDD,CF3,CFL1,DSQ,IDSQ,IPC,NCP,IORD)
// by Hiroshi Akima, U.S. Department of Commerce, NTIA/ITS
// Version of 1995/05
// This subroutine estimates partial derivatives of the first and
// second orders at the data points for bivariate interpolation
// and surface fitting for scattered data.  In most cases, this
// subroutine has the accuracy of a cubic (third-degree)
// polynomial.
//
// The input arguments are
//   IDV  = data point number,
//   XYZD = array containing the x y and the z values
//          at the data points.
// The output arguments are
//   PDD  = estimated zx, zy, zxx, zxy, and zyy values
//          at the IDV data point.
// The constant in the first PARAMETER statement below is
//   NPEMX = maximum number of primary estimates.
// The constant value has been selected empirically.
//      PARAMETER        (NPEMX=25)
	list<int> ipc;
	list<int>::iterator vi;
	TeTin::vertexSecondNeighVertices(idv, ipc);
	vi = ipc.begin();
	int i, idpi;
	double x = xyzd[idv].x();
	double y = xyzd[idv].y();
	TePartialDeriv pdpe[25];
	for (i = 0; i < 25; i++)
	{
		idpi = *vi;
		pdpe[i].zx ( cf3[idpi].a10() + x*(2.*cf3[idpi].a20() + x*3.*cf3[idpi].a30()) +
					 y*(cf3[idpi].a11() + 2.*cf3[idpi].a21()*x + cf3[idpi].a12()*y) );
		pdpe[i].zy ( cf3[idpi].a01() + y*(2.*cf3[idpi].a02() + y*3.*cf3[idpi].a03()) +
					 x*(cf3[idpi].a11() + 2.*cf3[idpi].a12()*y + cf3[idpi].a21()*x) );
		pdpe[i].zxx( 2.*cf3[idpi].a20() + 6.*cf3[idpi].a30()*x + 2.*cf3[idpi].a21()*y );
		pdpe[i].zxy( cf3[idpi].a11() + 2.*cf3[idpi].a21()*x + 2.*cf3[idpi].a12()*y );
		pdpe[i].zyy( 2.*cf3[idpi].a02() + 6.*cf3[idpi].a03()*y + 2.*cf3[idpi].a12()*x );

		vi++;
		if ( vi == ipc.end() )
			break;
	}
	int npe = i;
	if (npe == 1)	// Only one qualified point set
		pdd[idv] = pdpe[0];
	else
	{	// Weighted values of partial derivatives.
		TePartialDeriv ampdpe;	// Calculates the probability weight.
		for (i = 0; i < npe; i++)
			ampdpe = ampdpe + pdpe[i];
		ampdpe = ampdpe/(double)npe;

		TePartialDeriv sspdpe;	// Calculates the unbiased estimate of variance
		for (i = 0 ; i < npe; i++)
			sspdpe = sspdpe + (pdpe[i]-ampdpe)*(pdpe[i]-ampdpe);
		sspdpe = sspdpe/(double)(npe-1);

		for (i = 0; i < npe; i++)
		{
			double alpwt = 0.;
			if (sspdpe.zx() != 0.)
				alpwt += ((pdpe[i].zx()-ampdpe.zx())^2)/sspdpe.zx();
			if (sspdpe.zy() != 0.)
				alpwt += ((pdpe[i].zy()-ampdpe.zy())^2)/sspdpe.zy();
			if (sspdpe.zxx() != 0.)
				alpwt += ((pdpe[i].zxx()-ampdpe.zxx())^2)/sspdpe.zxx();
			if (sspdpe.zyy() != 0.)
				alpwt += ((pdpe[i].zyy()-ampdpe.zyy())^2)/sspdpe.zyy();
			if (sspdpe.zxy() != 0.)
				alpwt += ((pdpe[i].zxy()-ampdpe.zxy())^2)/sspdpe.zxy();
			pwt[i] = exp(-alpwt/2.);
		}
	// Calculates the reciprocal of the volatility weight.
		vi = ipc.begin();
		for (i = 0; i < npe; i++)
		{
			idpi = *vi;
			zx = cfl1[idpi].a10();
			zy = cfl1[idpi].a01();
			rvwt[i] = ((pdpe[i].zx()-zx)*(pdpe[i].zx()-zx) +
					   (pdpe[i].zy()-zy)*(pdpe[i].zy()-zy))*
					   (pdpe[i].zxx()*pdpe[i].zxx() +
					   2.*pdpe[i].zxy()*pdpe[i].zxy() +
						pdpe[i].zyy()*pdpe[i].zyy());
			vi++;
		}
	// Calculates the weighted values of partial derivatives.
		TePartialDeriv pddik, pddii;
		double smwtf = 0., smwti = 0. wtf, wti;
		for (i = 0; i < npe; i++)
		{
			if (rvwt[i] > 1e-38)
			{
				wtf = pwt[i]/rvwt[i];
				pddif = pddif + pdpe[i]*wtf;
				smwtf = smwtf + wtf;
			}
			else
			{
				wti = pwt[i];
				pddii = pddii + pdpe[i]*wti;
				smwti = smwti + wti;
			}
		}
		if (smwti < 0.)
			pdd[idv] = pddif/smwtf;
		else
			pdd[idv] = pddii/smwti;
		}
	}
}

void TeCalcZPolynomial (int ndp, vector<TeTinVertex>& xyzd, vector<TePartialDeriv>& pdd,
							   vector<int>& iord )
{
//      SUBROUTINE SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,NIP,XI,YI,KTLI,
//     +                  ITLI,ZI)
//
// Polynomials
// (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
//
// Hiroshi Akima
// U.S. Department of Commerce, NTIA/ITS
// Version of 1995/05
//
// This subroutine determines a polynomial in x and y for each
// triangle or rectangle in the x-y plane and calculates the z
// value by evaluating the polynomial for the desired points,
// for bivariate interpolation and surface fitting for scattered
// data.
//
// The input arguments are
//   NDP  = number of data points,
//   XD   = array of dimension NDP containing the x
//          coordinates of the data points,
//   YD   = array of dimension NDP containing the y
//          coordinates of the data points,
//   ZD   = array of dimension NDP containing the z
//          values at the data points,
//   NT   = number of triangles,
//   IPT  = two-dimensional integer array of dimension 3*NT
//          containing the point numbers of the vertexes of
//          the triangles,
//   NL   = number of border line segments,
//   IPL  = two-dimensional integer array of dimension 2*NL
//          containing the point numbers of the end points of
//          the border line segments,
//   PDD  = two-dimensional array of dimension 5*NDP
//          containing the partial derivatives at the data
//          points,
//   NIP  = number of output points at which interpolation is
//          to be performed,
//   XI   = array of dimension NIP containing the x
//          coordinates of the output points,
//   YI   = array of dimension NIP containing the y
//          coordinates of the output points,
//   KTLI = integer array of dimension NIP, each element
//          containing the code for the type of the piece of
//          the plane in which each output point lies
//        = 1 for a triangle inside the data area
//        = 2 for a rectangle on the right-hand side of a
//            border line segment
//        = 3 for a triangle between two rectangles on the
//            right-hand side of two consecutive border
//            line segments
//        = 4 for the triangle which is an overlap of two
//            rectangles on the right-hand side of two
//            consecutive border line segments,
//   ITLI = integer array of dimension NIP containing the
//          triangle numbers or the (second) border line
//          segment numbers corresponding to the output
//          points.
//
// The output argument is
//   ZI   = array of dimension NIP, where the calculated z
//          values are to be stored.
//
//
// Specification statements
//     .. Scalar Arguments ..
      INTEGER          NDP,NIP,NL,NT
//     ..
//     .. Array Arguments ..
      REAL             PDD(5,NDP),XD(NDP),XI(NIP),YD(NDP),YI(NIP),
     +                 ZD(NDP),ZI(NIP)
      INTEGER          IPL(2,NL),IPT(3,NT),ITLI(NIP),KTLI(NIP)
//     ..
//     .. Local Scalars ..
      REAL             A,AA,AB,ACT2,AD,ADBC,AP,B,BB,BC,BDT2,BP,C,CC,CD,
     +                 CP,D,DD,DLT,DP,DX,DY,E1,E2,G1,G2,H1,H2,H3,LUSQ,
     +                 LVSQ,P0,P00,P01,P02,P03,P04,P05,P1,P10,P11,P12,
     +                 P13,P14,P2,P20,P21,P22,P23,P3,P30,P31,P32,P4,P40,
     +                 P41,P5,P50,SPUV,U,V,WT1,WT2,X0,XII,Y0,YII,Z0,ZII,
     +                 ZII1,ZII2
      INTEGER          I,IDP,IIP,ILI,IR,ITLII,ITLIPV,K,KTLII,KTLIPV
//     ..
//     .. Local Arrays ..
      REAL             PD(5,3),X(3),Y(3),Z(3),ZU(3),ZUU(3),ZUV(3),ZV(3),
     +                 ZVV(3)
//     ..
//     .. Intrinsic Functions ..
      INTRINSIC        MOD
//     ..
// Outermost DO-loop with respect to the output point
	for (iip = 0; iip < nip; iip++)			//      DO 120 IIP = 1,NIP
	{
		ktlii = ktli[iip];					//          KTLII = KTLI(IIP)
		itlii = itli[iip];					//          ITLII = ITLI(IIP)
		if (iip == 0 )						//          IF (IIP.EQ.1) THEN
		{
			ktlipv = 0;						//              KTLIPV = 0
			itlipv = 0;						//              ITLIPV = 0
		}
		else								//          ELSE
		{
			ktlipv = ktli[iip-1];			//              KTLIPV = KTLI(IIP-1)
			itlipv = itli[iip-1];			//              ITLIPV = ITLI(IIP-1)
		}									//          END IF
		TeTinPolynomial tinPoly;
// Part 1.  Calculation of ZII by interpolation
		switch (ktlii)
		{
			case 1:						//          IF (KTLII.EQ.1) THEN
			{
				if ( (ktlii != ktlipv) || (itlii != itlipv))//	IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
	// Calculates the coefficients when necessary.
					TeTin::defineTrianglePolynomial ( itlii, tinPoly );
				xyz[iip].value( tinPoly.Solve( xyzd[iip].location()) );
			}									//          END IF
	// Part 2.  Calculation of ZII by extrapolation in the rectangle
			case 2:						//          IF (KTLII.EQ.2) THEN
	// Calculates the coefficients when necessary.
			{
				if ( (ktlii != ktlipv) || (itlii != itlipv))//	IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
					TeTin::defineEdgePolynomial ( itlii, tinPoly );
				xyz[iip].value( tinPoly.Solve( xyzd[iip].location()) );
	// Part 3.  Calculation of ZII by extrapolation in the triangle
			case 3:					//          IF (KTLII.EQ.3) THEN
			{
	// Calculates the coefficients when necessary.
				if ( (ktlii != ktlipv) || (itlii != itlipv))//	IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
	// Loads coordinate and partial derivative values at the vertex
	// of the triangle.
					TeTin::defineVertexPolynomial ( itlii, tinPoly );

				xyz[iip].value( tinPoly.Solve( xyzd[iip].location()) );
			}									//          END IF

	// Part 4.  Calculation of ZII by extrapolation in the triangle
	//          which is an overlap of two rectangles.
			case 4:						//          IF (KTLII.EQ.4) THEN
			{
				int itlim1 = border[(itlii - 1)%nl];
				TeCoord2D pt0 = TeTin::edge(itlii).from();
				TeCoord2D pt1 = TeTin::edge(itlii).to();
				TeCoord2D pt2;
				if ( TeTin::edge(itlim1).from() == pt0 )
				{
					pt2 = TeTin::edge(itlim1).to();
				}
				else if ( TeTin::edge(itlim1).to() == pt0 )
				{
					pt2 = TeTin::edge(itlim1).to();
				}
				else
				{
					pt2 = pt0;
					pt0 = pt1;
					pt1 = pt2;
					if ( TeTin::edge(itlim1).from() == pt0 )
						pt2 = TeTin::edge(itlim1).to();
					else
						pt2 = TeTin::edge(itlim1).from();
				}
				TeTin::defineEdgePolynomial ( border[itlii], tinPoly );
				double z1 = tinPoly.Solve( xyzd[iip].location() );
				double wt1 = ((pt1.x()-pt0.x())*(xyzd[iip].x()-pt0.x())+
							(pt1.y()-pt0.y())*(xyzd[iip].y()-pt0.x()))^2.;

				TeTin::defineEdgePolynomial ( border[itlim1], tinPoly );
				double z2 = tinPoly.Solve( xyzd[iip].location() );
				double wt2 = ((pt2.x()-pt0.x())*(xyzd[iip].x()-pt0.x())+
							(pt2.y()-pt0.y())*(xyzd[iip].y()-pt0.x()))^2.;

				xyz[iip].value( (wt1*z1 + wt2*z2) / (wt1+wt2) );
			}								//          END IF
	}									//  120 CONTINUE
}										//      END

void TeRegressionMatrixSetRow (double xk, double yk, double zk, double xi,
							   double yi, double zi, double s1, double s2,
							   double s3, double w, double* row)
{
// Adapted from SUBROUTINE SETRO3(XK,YK,ZK,XI,YI,ZI,S1,S2,S3,W,ROW)
// from SRFPACK by Robert J. Renka, Dept. of Computer Science,
// Univ. of North Texas,(817) 565-2767,01/25/97
//   This subroutine sets up the I-th row of an augmented re-
// gression matrix for a weighted least squares fit of a
// cubic function f(x,y) to a set of data values z, where
// f(XK,YK) = ZK.  The first four columns (cubic terms) are
// scaled by S3, the next three columns (quadratic terms)
// are scaled by S2, and the eighth and ninth columns (lin-
// ear terms) are scaled by S1.
// On input:
//       XK,YK = Coordinates of node K.
//       ZK = Data value at node K to be interpolated by f.
//       XI,YI,ZI = Coordinates and data value at node I.
//       S1,S2,S3 = Scale factors.
//       W = Weight associated with node I.
// The above parameters are not altered by this routine.
//       ROW = Array of length 10.
// On output:
//       ROW = Array containing a row of the augmented re-
//             gression matrix.
	double dx = xi - xk;				//      DX = XI - XK
	double dy = yi - yk;				//      DY = YI - YK
	double w1 = s1*w;					//      W1 = S1*W
	double w2 = s2*w;					//      W2 = S2*W
	double w3 = s3*w;					//      W3 = S3*W
	row[0] = dx*dx*dx*w3;				//      ROW(1) = DX*DX*DX*W3
	row[1] = dx*dx*dy*w3;				//      ROW(2) = DX*DX*DY*W3
	row[2] = dx*dy*dy*w3;				//      ROW(3) = DX*DY*DY*W3
	row[3] = dy*dy*dy*w3;				//      ROW(4) = DY*DY*DY*W3
	row[4] = dx*dx*w2;					//      ROW(5) = DX*DX*W2
	row[5] = dx*dy*w2;					//      ROW(6) = DX*DY*W2
	row[6] = dy*dy*w2;					//      ROW(7) = DY*DY*W2
	row[7] = dx*w1;						//      ROW(8) = DX*W1
	row[8] = dy*w1;						//      ROW(9) = DY*W1
	row[9] = (zi-zk)*w;					//      ROW(10) = (ZI-ZK)*W
}										//      RETURN
										//      END

void TeGivensRotate (int n, double c, double s, vector<double>& x,
					 vector<double>& y)
{
// Adapted from SUBROUTINE ROTATE(N,C,S,X,Y) from SRFPACK by
// Robert J. Renka, Dept. of Computer Science, Univ. of North Texas
// (817) 565-2767 09/01/88
//   This subroutine applies the Givens rotation  ( C  S)  to
//                                                (-S  C)
// the 2 by N matrix  (X(1) ... X(N)) .
//                    (Y(1) ... Y(N))
// On input:
//       N = Number of columns to be rotated.
//       C,S = Elements of the Givens rotation.  Refer to
//             subroutine GIVENS.
// The above parameters are not altered by this routine.
//       X,Y = Arrays of length .GE. N containing the compo-
//             nents of the vectors to be rotated.
// On output:
//       X,Y = Arrays containing the rotated vectors (not
//             altered if N < 1).
	int i;
	double xi, yi;
	for (i = 0; i < n; i++)			//      DO 10 I = 1,N
	{
		xi = x[i];					//          XI = X(I)
		yi = y[i];					//          YI = Y(I)
		x[i] =  c*xi + s*yi;		//          X(I) = C*XI + S*YI
		y[i] = -s*xi + c*yi;		//          Y(I) = -S*XI + C*YI
	}								//   10 CONTINUE
}									//      RETURN
									//      END

void TeGivensBuild (double& a, double& b, double& c, double& s)
{
// Adapted from SUBROUTINE GIVENS(A,B,C,S) from SRFPACK by
// Robert J. Renka, Dept. of Computer Science, Univ. of North Texas
// (817) 565-2767 09/01/88
//   This subroutine constructs the Givens plane rotation,
//       G = ( C  S) , where C*C + S*S = 1,
//           (-S  C)
// which zeros the second component of the vector (A,B)**T
// (transposed).  Subroutine ROTATE may be called to apply
// the transformation to a 2 by N matrix.
// On input:
//       A,B = Components of the vector defining the rota-
//             tion.  These are overwritten by values R
//             and Z (described below) which define C and S.
// On output:
//       A = Signed Euclidean norm R of the input vector:
//           R = +/-SQRT(A*A + B*B)
//       B = Value Z such that:
//             C = SQRT(1-Z*Z) and S=Z if ABS(Z) .LE. 1, and
//             C = 1/Z and S = SQRT(1-C*C) if ABS(Z) > 1.
//       C = +/-(A/R) or 1 if R = 0.
//       S = +/-(B/R) or 0 if R = 0.
	double aa = a;				//      AA = A
	double bb = b;				//      BB = B
	if (fabs(aa) > fabs(bb))	//      IF (ABS(AA).LE.ABS(BB)) GO TO 10
	{	// ABS(A) > ABS(B).
		u = aa + aa;			//      U = AA + AA
		v = bb/u;				//      V = BB/U
		r = sqrt(.25+v*v)*u;	//      R = SQRT(.25+V*V)*U
		c = aa/r;				//      C = AA/R
		s = v*(c+c);			//      S = V* (C+C)
// Note that R has the sign of A, C > 0, and S has
//   SIGN(A)*SIGN(B).
		b = s;					//      B = S
		a = r;					//      A = R
	}							//      RETURN
	else
	{	// ABS(A) .LE. ABS(B).
		if (bb != 0.)			//   10 IF (BB.EQ.0.) GO TO 20
		{
			u = bb + bb;		//      U = BB + BB
			v = aa/u;			//      V = AA/U
// Store R in A.
			a = sqrt(.25+v*v)*u;//      A = SQRT(.25+V*V)*U
			s = bb/a;			//      S = BB/A
			c = v*(s+s);		//      C = V* (S+S)
// Note that R has the sign of B, S > 0, and C has
//   SIGN(A)*SIGN(B).
			b = 1.;				//      B = 1.
			if (c != 0)			//      IF (C.NE.0.) B = 1./C
				b = 1./c;
		}						//      RETURN
		else
		{	// A = B = 0.
			c = 1.;				//   20 C = 1.
			s = 0.;				//      S = 0.
		}						//      RETURN
	}
}								//      END

void TeEstimateCubicDerivatives (int k)
{
// Adapted from SUBROUTINE GRADC(K,NCC,LCC,N,X,Y,Z,LIST,LPTR,LEND,DX,DY,DXX,DXY,
//  DYY,IER) from SRFPACK by Robert J. Renka, Dept. of Computer Science
//   Univ. of North Texas (817) 565-2816 01/25/97
//   Given a Delaunay triangulation of N points in the plane
// with associated data values Z, this subroutine estimates
// first and second partial derivatives at node K.  The der-
// ivatives are taken to be the partials at K of a cubic
// function which interpolates Z(K) and fits the data values
// at a set of nearby nodes in a weighted least squares
// sense.  A Marquardt stabilization factor is used if neces-
// sary to ensure a well-conditioned system.  Thus, a unique
// solution exists if there are at least 10 noncollinear
// nodes.
//       K = Index of the node at which derivatives are to be
//           estimated.
//       N = Number of nodes in the triangulation.
// On output:
//       DX,DY = Estimated first partial derivatives at node
//               K unless IER < 0.
//       DXX,DXY,DYY = Estimated second partial derivatives
//                     at node K unless IER < 0.
	TeMatrix a;
	a.Init( 10, 10 );
	int lmn = 14, lmx = 30;				//      PARAMETER        (LMN=14,LMX=30)
	int lmin = min(lmn,n);				//      LMIN = MIN(LMN,N)
    int lmax = min(lmx,n);				//      LMAX = MIN(LMX,N)
	double rtol = 1.0e-5, dtol = 0.01;	// Tolerance for detecting an
										// ill-conditioned system.
	list<int> npts;	// Indexes of a sequence of nodes ordered by distance from K.
	TeTin::vertexSecondNeighVertices(k, npts);
	double sum = 0.;	//Sum of squared distances between node K and
						// the nodes used in the least squares fit
	double ds = 0;		//Squared distance between nodes K and NPTS(LNP)
	double dist2;
	int i, j;
	list<int>::iterator vii;
	vii = npts.begin();
	for (i = 0; i < 29; i++)
	{
		dist2 = TeSquaredDistance ( xyzd[*vii].location() - xyzd[k].location() );
		vii++;
		if (i == 11)
		{
			ds = dist2;
			sum += dist2;
		}
		else if (i > 11)
		{
			if ((dist2-ds)/ds < rtol)
				sum += dist2;
			else
			{
				i++;
				break;
			}
		}
		else
			sum += dist2;
		if ( vii == npts.end() )
		{
			i++;
			break;
		}
	}
    int lnp = i;
	double rs = dist2;
    if (lnp == 29)
		rs = 1.1 * dist2;
// There are LNP-2 equations corresponding to nodes NPTS(2),
//   ...,NPTS(LNP-1).
	double sfs = (double)(lnp)/sum;	//   40 SFS = REAL(LNP-2)/SUM
	double sf = sqrt(sfs);				//      SF = SQRT(SFS)
	double sfc = sf*sfs;				//      SFC = SF*SFS
	double rin = 1./(sqrt(rs));			//      RIN = 1./SQRT(RS)
	double xk = xyzd[k].x();			//      XK = X(KK)
	double yk = xyzd[k].y();			//      YK = Y(KK)
	double zk = xyzd[k].value();		//      ZK = Z(KK)
	double w;
	vii = npts.begin();
	for (i = 0; i < 9; i++)				//      DO 60 I = 1,9
	{
		w = 1./( TeSquaredDistance ( xyzd[*vii].location() - xyzd[k].location() ) - rin;
//          W = 1./DIST(I+1) - RIN
		TeRegressionMatrixSetRow (xk, yk, zk, xyzd[*vii].x(), xyzd[*vii].y(),
								  xyzd[*vii].value(), sf, sfs, sfc, w, a.Row(i));
		if (i == 0)						//          IF (I.EQ.1) GO TO 60
			continue;
		for (j = 0; j < i; j++)		//          DO 50 J = 1,I - 1
		{
			TeGivensBuild (a(j,j), a(j,i), c, s);// CALL GIVENS(A(J,J),A(J,I),C,S)
			TeGivensRotate (10-j, c, s, a(j+1,j), a(j+1,i));
		}								//   50     CONTINUE
	}									//   60 CONTINUE
// Add the additional equations to the system using the last column of A.
	i = 10;								//      I = 11
	while (i < lnp)								//      I = 11
	{
		i++;
		w = 1./( TeSquaredDistance ( xyzd[*vii].location() - xyzd[k].location() ) - rin;
		TeRegressionMatrixSetRow (xk, yk, zk, xyzd[*vii].x(), xyzd[*vii].y(),
								  xyzd[*vii].value(), sf, sfs, sfc, w, a.Row(9));
		for (j = 0; j < 9; j++)		//          DO 80 J = 1,9
		{
			TeGivensBuild (a(j,j), a(j,9), c, s);
			TeGivensRotate (10-j, c, s, a(j+1,j), a(j+1,9));
		}								//   80     CONTINUE
		vii++;
		if (i == lnp-1)
		{
		// Test the system for ill-conditioning.
			dmin = fabs(a(0,0));
			for (j = 0; j < 9; j++)
				dmin = (fabs(a(j,j) < dmin) ? fabs(a(j,j) : dmin;
			if ( (dmin/w < dtol) && (lnp < 30) && (vii != npts.end()) )
			{
				lnp++;					//     LNP = LNP + 1
				if (lnp < 30)					//          IF (LNP.LE.LMAX) THEN
					rs = TeSquaredDistance ( xyzd[*vii].location() - xyzd[k].location() ) - rin;
				rin = 1./sqrt(1.1*rs);	//          RIN = 1./SQRT(1.1*RS)
			}
		}
	}
// Stabilize the system by damping third partials -- add
//   multiples of the first four unit vectors to the first
//   four equations.
	stf = w;							//      STF = W
	for (i = 0; i < 4; i++)				//      DO 110 I = 1,4
	{
		a(i,9) = stf;					//          A(I,10) = STF
		for (j = i+1; j < 10; j++)		//          DO 90 J = I + 1,10
			a(j,9) = 0.;				//              A(J,10) = 0.
										//   90     CONTINUE
		for (j = i; j < 9; j++)			//          DO 100 J = I,9
		{
			TeGivensBuild (a(j,j), a(j,9), c, s);
			TeGivensRotate (10-j, c, s, a(j=1,j), a(j+1,9));
		}								//  100     CONTINUE
	}									//  110 CONTINUE
// Test the damped system for ill-conditioning.
	dmin = fabs(a(4,4));
	for (j = 5; j < 9; j++)
		dmin = (fabs(a(j,j) < dmin) ? fabs(a(j,j) : dmin;
	//      DMIN = MIN(ABS(A(5,5)),ABS(A(6,6)),ABS(A(7,7)),ABS(A(8,8)),
	//     +       ABS(A(9,9)))
	if (dmin/w < dtol)					//      IF (DMIN/W.LT.DTOL) GO TO 140
	// No unique solution due to collinear nodes.
			return false;							//      RETURN

// Solve the 9 by 9 triangular system for the last 5
//   components (first and second partial derivatives).
	dy =   a(9,8)											/a(8,8);
	dx =  (a(9,7)								 -a(8,7)*dy)/a(7,7);
	dyy = (a(9,6)					   -a(7,6)*dx-a(8,6)*dy)/a(6,6);
	dxy = (a(9,5)			-a(6,5)*dyy-a(7,5)*dx-a(8,5)*dy)/a(5,5);
	dxx = (a(9,4)-a(5,4)*dxy-a(6,4)*dyy-a(7,4)*dx-a(8,4)*dy)/a(4,4);
// Scale the solution components.
	dx = sf*dx;							//      DX = SF*DX
	dy = sf*dy;							//      DY = SF*DY
	dxx = 2.*sfs*dxx;					//      DXX = 2.*SFS*DXX
	dxy = sfs*dxy;						//      DXY = SFS*DXY
	dyy = 2.*sfs*dyy;					//      DYY = 2.*SFS*DYY
	return true;
}										//      RETURN
										//      END
/*
	zz.Init( aa.Nrow(), aa.Ncol() );
	
// Calculation of inverse matrix of AA
	for (ij = 0; ij < n; ij++)			//   DO 110 IJ = 1,N
	{
// Finds out the element having the maximum absolute value in the
// IJ th row.
		aamx = fabs (aa(ij,ij));		//		AAMX = ABS(AA(IJ,IJ))
		jmx = ij;						//      JMX = IJ
		for ( j = ij; j < n; j++)		//      DO 40 J = IJ,N
		{
			if ( fabs (aa(ij,j) > aamx )//			IF (ABS(AA(IJ,J)).GT.AAMX) THEN
			{
				aamx = fabs (aa(ij,j));	//				AAMX = ABS(AA(IJ,J))
				jmx = j;				//				JMX = J
			}							//			END IF
		}								//40	CONTINUE
// Switches two columns in such a way that the element with the
// maximum value is on the diagonal.
		for (i = 0; i < n; i++)			//      DO 50 I = 1,N
		{
			aaijmx = aa(i,ij);			//			AAIJMX = AA(I,IJ)
			aa(i,ij) = aa (i,jmx);		//          AA(I,IJ) = AA(I,JMX)
			aa(i,jmx) = aaijmx;			//          AA(I,JMX) = AAIJMX
		}								//50    CONTINUE
        kjmx = k[ij];					//		KJMX = K(IJ)
        k[ij] = k[jmx];					//		K(IJ) = K(JMX)
        k[jmx] = kjmx;					//		K(JMX) = KJMX
// Makes the diagonal element to be unity.
        aaijij = aa(ij,ij);				//		AAIJIJ = AA(IJ,IJ)
//CHANGED from Valtulina : IF (AAIJIJ.EQ.0.0) GO TO 210
        if (fabs(aaijij) < 1.0e-8)		//		IF (ABS(AAIJIJ).LT.1.0E-8) GO TO 210 
        {
			//GOTO 210
// Special case where the determinant is zero
			for (i = 0; i < n; i++)		//210	DO 220 I = 1,N
				x(i) = 0.0;				//			X(I) = 0.0
										//220	CONTINUE
			det = 0.0;					//		DET = 0.0
			return;						//      RETURN
		}
		for (j = ij; j < n; j++)		//		DO 60 J = IJ,N
			aa(ij,j) = aa(ij,j)/aaijij;	//			AA(IJ,J) = AA(IJ,J)/AAIJIJ
										//60    CONTINUE
        for (jj = 0; j < n; j++)		//		DO 70 JJ = 1,N
			ee(ij,jj) = ee(ij,jj)/aaijij;//			EE(IJ,JJ) = EE(IJ,JJ)/AAIJIJ
										//70    CONTINUE
// Eliminates the lower left elements.
		if (ij < n)						//		IF (IJ.LT.N) THEN
		{
			ijp1 = ij + 1;				//			IJP1 = IJ + 1
            for ( i = ijp1; i < n; i++)	//			DO 100 I = IJP1,N
            {
				aaiij = aa (i,ij);		//				AAIIJ = AA(I,IJ)
                for ( j = ijp1; j < n; j++)	//			DO 80 J = IJP1,N
					aa(i,j) = aa(i,j) - aa(ij,j)*aaiij; //	AA(I,J) = AA(I,J) - AA(IJ,J)*AAIIJ
										//80            CONTINUE
                for (jj = 0; jj < n; jj++)	//			DO 90 JJ = 1,N
					ee(i,jj)= ee(i,jj) - ee(ij,jj)*aaij;//	EE(I,JJ) = EE(I,JJ) - EE(IJ,JJ)*AAIIJ
										//90            CONTINUE
			}							//100       CONTINUE
		}								//      END IF
// Calculates the determinant.
										//DELETED from Valtulina
										//DELETED          IF (IJ.EQ.1) THEN
										//DELETED              DET = 0.0
										//DELETED              SGN = 1.0
										//DELETED          END IF
										//DELETED          SGN = SGN* ((-1)** (IJ+JMX))
										//DELETED          DET = DET + LOG(ABS(AAIJIJ))
	}									//110 CONTINUE
										//DELETED      IF (DET.LT.85.0) THEN
										//DELETED          DET = SGN*EXP(DET)
										//DELETED      ELSE
										//DELETED          DET = SGN*1.0E38
										//DELETED      END IF

//ADDED from Valtulina : at this point DET must be not equal 0
	det = 1.0;							//		DET=1.0
// Calculates the elements of the inverse matrix.
	for (ijr = 0; ijr < n; ijr++)		//		DO 140 IJR = 1,N
    {
		ij = n + 1 - ijr;				//			IJ = N + 1 - IJR
		if (ij < n)						//			IF (IJ.LT.N) THEN
        {
			ijp1 = ij + 1;				//				IJP1 = IJ + 1
			for (j=ijp1; j < n; j++)	//				DO 130 J = IJP1,N
				for (jj = 0; jj < n; jj++ )//			DO 120 JJ = 1,N
					ee(ij,jj) = ee(ij,jj) -aa(ij,j)*ee(j,jj)//  EE(IJ,JJ) = EE(IJ,JJ) - AA(IJ,J)*EE(J,JJ)
										//120				CONTINUE
										//130           CONTINUE
		}								//          END IF
	}									//140	CONTINUE
	for ( j = 0; j < n; j++ )			//      DO 160 J = 1,N
    {
			i = k[j];					//			I = K(J)
			for ( jj = 0; jj < n; jj++ )//			DO 150 JJ = 1,N
				zz(i,jj) = ee(j,jj);	//				ZZ(I,JJ) = EE(J,JJ)
										//150   CONTINUE
	}									//160	CONTINUE
*/
/*
// Outermost DO-loop with respect to the data point
	for (idp1 = 0; idp1 < ndp; idp1++)	//      DO 310 IDP1 = 1,NDP
// Selects data point sets for sets of primary estimates of partial
// derivatives.
// - Selects a candidate.
	{
		npe = 0;						//          NPE = 0
		for (idp2 = 0; idp2 < ndp; idp2++)//        DO 80 IDP2 = 1,NDP
		{
			ncp2 = ncp[idp2];			//              NCP2 = NCP(IDP2)
			ncp2p1 = ncp2 + 1;			//              NCP2P1 = NCP2 + 1
			bool equalfound = true;
			if (idp2 != idp1)			//              IF (IDP2.EQ.IDP1) GO TO 20
			{
				equalfound = false;
				for (j = 0; j < ncp2; j++)	//              DO 10 J = 1,NCP2
				{
					if (ipc[j,idp2] == idp1)//                  IF (IPC(J,IDP2).EQ.IDP1) GO TO 20
					{
						equalfound = true;	//GOTO 20
						break;
					}
				}							//   10         CONTINUE
			}
			if (! equalfound)				//              GO TO 80
				continue;					//   80     CONTINUE
			ipcpe[0,npe+1] = idp2;			//   20         IPCPE(1,NPE+1) = IDP2
			for (j = 0; j < ncp2; j++)		//              DO 30 J = 1,NCP2
				ipcpe[j+1,npe+1] = ipc[j,idp2];//               IPCPE(J+1,NPE+1) = IPC(J,IDP2)
											//   30         CONTINUE
			for (j1 = 0; j1 < ncp2; j1++)	//              DO 50 J1 = 1,NCP2
			{
				jmn = j1;					//                  JMN = J1
				imn = ipcpe[j1,npe+1];		//                  IMN = IPCPE(JMN,NPE+1)
				for (j2 = j1; j1 < ncp2p1; j2++)//              DO 40 J2 = J1,NCP2P1
				{
					if (ipcpe[j2,npe+1] < imn)//                    IF (IPCPE(J2,NPE+1).LT.IMN) THEN
					{
						jmn = j2;			//                          JMN = J2
						imn = ipcpe[jmn,npe+1];//                       IMN = IPCPE(JMN,NPE+1)
					}						//                      END IF
				}							//   40             CONTINUE
				ipcpe[jmn, npe+1] = ipcpe[j1,npe+1];//          IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
				ipcpe[j1,npe+1] = imn;		//                  IPCPE(J1,NPE+1) = IMN
			}								//   50         CONTINUE
// - Checks whether or not the candidate has already been included.
			if (npe > 0)					//              IF (NPE.GT.0) THEN
			{
				for (ipe1 = 0; ipe1 < npe; ipe1++)//            DO 70 IPE1 = 1,NPE
				{
					idppe1 = idppe[ipe1];	//                      IDPPE1 = IDPPE(IPE1)
					if (ncp2 != ncp[idppe1]	//                      IF (NCP2.NE.NCP(IDPPE1)) GO TO 70
					{
						//GOTO 70
						continue;			//   70             CONTINUE
					}
					for (j = 0; j < ncp2p1; j++)//                  DO 60 J = 1,NCP2P1
					{
						if (ipcpe[j,npe+1] != ipcpe[j,ipe1] )//         IF (IPCPE(J,NPE+1).NE.
											//     +                        IPCPE(J,IPE1)) GO TO 70
						{
							//GOTO 70
							break;
						}
					}						//   60             CONTINUE
					if (j != ncp2p1) //GOTO 70
						continue;			//   70             CONTINUE
					break;					//                  GO TO 80
				}							//   70             CONTINUE
				if (ipe1 != npe)
					continue;				//                  GO TO 80
			}								//              END IF
			npe = npe+1;					//              NPE = NPE + 1
			idppe[npe] = idp2;				//              IDPPE(NPE) = IDP2
			if (npe > npemx)				//              IF (NPE.GE.NPEMX) GO TO 90
			{
				break;	//GOTO 90
			}
		}									//   80     CONTINUE
											//   90     CONTINUE
// Adds additional closest data points when necessary.
		if (npe < npeamn)					//          IF (NPE.LT.NPEAMN) THEN
		{
			for (jj = 0; jj < 9; jj++)		//              DO 150 JJ = 1,9
			{
				idp2 = ipc[jj,idp1];		//                  IDP2 = IPC(JJ,IDP1)
				ncp2 = ncp[idp2];			//                  NCP2 = NCP(IDP2)
				ncp2p1 = ncp2 + 1;			//                  NCP2P1 = NCP2 + 1
				ipcpe[0,npe+1] = idp2;		//                  IPCPE(1,NPE+1) = IDP2
				for (j=0; j < ncp2; j++)	//                  DO 100 J = 1,NCP2
					ipcpe[j+1, npe=1] = ipc[j,idp2];//              IPCPE(J+1,NPE+1) = IPC(J,IDP2)
											//  100             CONTINUE
				for (j1=0; j1 < ncp2; j1++)	//                  DO 120 J1 = 1,NCP2
				{
					jmn = j1;				//                      JMN = J1
					imn = ipcpe[jmn,npe+1]	//                      IMN = IPCPE(JMN,NPE+1)
					for (j2=j1 ; j2 < ncp2p1; j2++)//               DO 110 J2 = J1,NCP2P1
					{
						if (ipcpe[j2, npe+1] < imn)//                   IF (IPCPE(J2,NPE+1).LT.IMN) THEN
						{
							jmn = j2;		//                              JMN = J2
							imn = ipcpe[jmn, npe+1]//                       IMN = IPCPE(JMN,NPE+1)
						}					//                          END IF
					}						//  110                 CONTINUE
					ipcpe[jmn, npe+1] = ipcpe[j1, npe+1];//         IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
					ipcpe[j1,npe+1] = imn;	//                      IPCPE(J1,NPE+1) = IMN
				}							//  120             CONTINUE
				if (npe > 0)				//                  IF (NPE.GT.0) THEN
				{
					for (ipe1=0; ipe1 < npe; ipe1++)//              DO 140 IPE1 = 1,NPE
					{
						idppe1 = idppe[ipe1];//                         IDPPE1 = IDPPE(IPE1)
						if (ncp2 != ncp[idppe1])//                      IF (NCP2.NE.NCP(IDPPE1)) GO TO 140
							continue;//GOTO 140-140                 CONTINUE
						for (j=0; j < ncp2p1; j++)//                    DO 130 J = 1,NCP2P1
						{
							if (ipcpe[j,npe+1] != ipcpe[j,ipe1] )
											//                              IF (IPCPE(J,NPE+1).NE.
                                            //     +                            IPCPE(J,IPE1)) GO TO 140
								break;		//  140                 CONTINUE
						}					//  130                     CONTINUE
						if (j != ncp2p1)
							continue;		//  140                 CONTINUE
						break;				//                          GO TO 150
					}
					if (ipe1 != npe)
						continue;
				}							//				  END IF
				npe = npe+1;				//                  NPE = NPE + 1
				idppe[npe] = idp2;			//                  IDPPE(NPE) = IDP2
				if (npe > npeamx)			//                  IF (NPE.GE.NPEAMX) GO TO 160
					break;
			}								//  150         CONTINUE
		}									//          END IF
											//  160     CONTINUE
*/