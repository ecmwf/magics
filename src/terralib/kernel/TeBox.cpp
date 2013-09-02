/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/

#include "TeBox.h"
#include "TeGeometry.h"

void zoomIn ( TeBox& box, double t )
{
		TeCoord2D c = box.center ();
		double w    = box.width ()*t/2.;
		double h    = box.height ()*t/2.;
		box.x1_ = c.x() - w;
		box.x2_ = c.x() + w;
		box.y1_ = c.y() - h;
		box.y2_ = c.y() + h;
}


void zoomOut ( TeBox& box, double t )
{
		TeCoord2D c = box.center ();
		double w = box.width ()/t/2.;
		double h = box.height ()/t/2.;
		box.x1_ = c.x() - w;
		box.x2_ = c.x() + w;
		box.y1_ = c.y() - h;
		box.y2_ = c.y() + h;
}

// --- UPDATE BOX

void
updateBox ( TeBox& box, const TeCoord2D& pt )
{
	if ( pt.x() <= box.x1_ )
		box.x1_  = pt.x();
	
	if ( pt.x() >= box.x2_ )
		 box.x2_ =  pt.x();

	if ( pt.y() <= box.y1_ )
		box.y1_ =  pt.y();

	if ( pt.y() >= box.y2_ )
		box.y2_ =  pt.y();
}

void
updateBox ( TeBox& box, const TeBox& other )
{
		if ( other.x1_ <= box.x1_ ) 
			box.x1_ = other.x1_;
		if ( other.x2_ >= box.x2_ )
			box.x2_ = other.x2_;
		if ( other.y1_ <= box.y1_ )
			box.y1_ = other.y1_;
		if ( other.y2_ >= box.y2_ )
			box.y2_ = other.y2_;
}

void
updateBox ( TeBox& box, const TeGeometry& geo )
{
		updateBox ( box, geo.box() );
}

TeBox 
makeBox(double x1, double y1, double x2, double y2, const double& tol)
{
	double xlo, xhi;

	if(x1 > x2)
	{
		xhi = x1;
		xlo = x2;
	}
	else
	{
		xhi = x2;
		xlo = x1;
	}

	double ylo, yhi;	
	if(y1 > y2)
	{
		yhi = y1;
		ylo = y2;
	}
	else
	{
		yhi = y2;
		ylo = y1;
	}

	return TeBox(xlo - tol, ylo - tol, xhi + tol, yhi + tol);
}


TeBox 
adjustToCut(TeBox& box, double bWidth, double bHeight)
{
	double auxD;
	int auxI;

	int magicX; 
	auxD = box.x1()/bWidth;
	auxI = (int)(box.x1()/bWidth);
	if (box.x1() < 0 && (auxD - auxI) != 0)
		magicX = (int) (box.x1()/bWidth - 1);
	else
		magicX = auxI;

	int magicY;
	auxD = box.y1()/bHeight;
	auxI = (int)(box.y1()/bHeight);
	if (box.y1() < 0 && (auxD - auxI) != 0)
		magicY  = (int)(box.y1()/bHeight - 1);
	else
		magicY  = auxI;
		
	double xi = magicX*bWidth;
	double yi = magicY*bHeight;

	int magicX2;
	auxD = box.x2()/bWidth;
	auxI = (int)(box.x2()/bWidth);
	if ((box.x2() < 0) || (auxD - auxI) == 0)
		magicX2 = (int) (box.x2()/bWidth);
	else
		magicX2 = (int) (box.x2()/bWidth + 1);


	int magicY2;
	auxD = box.y2()/bHeight;
	auxI = (int)(box.y2()/bHeight);
	if ((box.y2() < 0) || (auxD - auxI) == 0)
		magicY2 = (int) (box.y2()/bHeight);
	else
		magicY2 = (int) (box.y2()/bHeight + 1);


	double xf = (magicX2)*bWidth;
	double yf = (magicY2)*bHeight;

	return TeBox(xi,yi,xf,yf);
}

TePolygon polygonFromBox( TeBox& bb )
{
	TePolygon poly;
	TeLine2D line;
	
	TeCoord2D ll (bb.x1_, bb.y1_);
	TeCoord2D ul (bb.x1_, bb.y2_);
	TeCoord2D ur (bb.x2_, bb.y2_);
	TeCoord2D lr (bb.x2_, bb.y1_);

	line.add(ll);
	line.add(ul);
	line.add(ur);
	line.add(lr);
	line.add(ll);

	TeLinearRing ring(line);
	poly.add(ring);
	return poly;
}

int adjustBox( TeBox& bb )
{
	int precision = 4;
	double factor =  pow(10., precision);
	double tol = 1 / factor;
	while (( TeCompareDouble( bb.x1_, bb.x2_, precision ) == true ) || 
		   ( TeCompareDouble( bb.y1_, bb.y2_, precision ) == true ) )
	{
		precision--;
		tol = 1 / pow(10., precision);
		if ( TeCompareDouble( bb.x1_, bb.x2_, precision ) == true )
		{
			bb.x1_ -= tol;
			bb.x2_ += tol;
		}
		if ( TeCompareDouble( bb.y1_, bb.y2_, precision ) == true )
		{
			bb.y1_ -= tol;
			bb.y2_ += tol;
		}
		if (precision == 0)
			break;		
	}
	return precision;
}
