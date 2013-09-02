/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#ifdef WIN32 
#pragma warning ( disable: 4786 ) 
#endif

#include "TeVectorRemap.h"

TeBox TeRemapBox ( TeBox& box, TeProjection* projFrom, TeProjection* projTo)
{
	if (projFrom == 0 || projTo == 0)
		return box;

	if (*projFrom == *projTo)
		return box;

	if(projFrom->name() == "NoProjection" || projTo->name() == "NoProjection")
		return box;

	if (projFrom->datum().name() != projTo->datum().name())
	{
		projFrom->setDestinationProjection(projTo);
		projTo->setDestinationProjection(projFrom);
	}

	TeCoord2D	pll = box.lowerLeft (),
				pur = box.upperRight (),
				pul (pll.x(),pur.y()),
				plr (pur.x(),pll.y());

// Evaluate coordinates of box corners in Lat/Long

	pll = projFrom->PC2LL (pll);
	pur = projFrom->PC2LL (pur);
	pul = projFrom->PC2LL (pul);
	plr = projFrom->PC2LL (plr);

// Evaluate the minimum box that includes all four corners

	pll.x(std::min(pll.x(),pul.x()));
	pll.y(std::min(pll.y(),plr.y()));
	plr.x(std::max(plr.x(),pur.x()));
	plr.y(std::min(pll.y(),plr.y()));
	pur.x(std::max(pur.x(),plr.x()));
	pur.y(std::max(pul.y(),pur.y()));
	pul.x(std::min(pll.x(),pul.x()));
	pul.y(std::max(pul.y(),pur.y()));


// Bring coordinates of box corners to target projection

	pll = projTo->LL2PC (pll);
	pur = projTo->LL2PC (pur);
	pul = projTo->LL2PC (pul);
	plr = projTo->LL2PC (plr);

// Evaluate the minimum box that includes all four corners

	return TeBox (	std::min(pll.x(),pul.x()),
					std::min(pll.y(),plr.y()),
					std::max(pur.x(),plr.x()),
					std::max(pul.y(),pur.y()));
}

void
TeVectorRemap(TeCoord2D& c1, TeProjection* p1, TeCoord2D& c2, TeProjection* p2)
{
	p1->setDestinationProjection(p2);
	TeCoord2D ll = p1->PC2LL(c1);
	c2 = p2->LL2PC(ll);
}

void 
TeVectorRemap(TeText& t1, TeProjection* p1, TeText& t2, TeProjection* p2)
{
	p1->setDestinationProjection(p2);
	t2 = t1;
	TeCoord2D nl = p1->PC2LL(t1.location());
	TeCoord2D p =  p2->LL2PC(nl);
	t2.setLocation(p);
}

void TeVectorRemap(TeCell& c1, TeProjection* p1, TeCell& c2, TeProjection* p2)
{	
	c2 = c1;
	TeBox nb = TeRemapBox(c1.box(),p1,p2);
	c2.setBox(nb);
}

TeBox TeRemapBoxPC2Geodetic (const TeBox& box, TeProjection* proj)
{
	if (proj == 0)
		return box;

	proj->setDestinationProjection(0);

	TeCoord2D	pll = box.lowerLeft (),
				pur = box.upperRight (),
				pul (pll.x(),pur.y()),
				plr (pur.x(),pll.y());

	pll = proj->PC2LL (pll);
	pur = proj->PC2LL (pur);
	pul = proj->PC2LL (pul);
	plr = proj->PC2LL (plr);

// Evaluate the minimum box that includes all four corners

	return TeBox (	std::min(pll.x(),pul.x()),
					std::min(pll.y(),plr.y()),
					std::max(pur.x(),plr.x()),
					std::max(pul.y(),pur.y()));
}

TeBox TeRemapBoxGeodetic2PC (const TeBox& box, TeProjection* proj)
{
	if (proj == 0)
		return box;

	proj->setDestinationProjection(0);

	TeCoord2D	pll = box.lowerLeft (),
				pur = box.upperRight (),
				pul (pll.x(),pur.y()),
				plr (pur.x(),pll.y());

	pll = proj->LL2PC (pll);
	pur = proj->LL2PC (pur);
	pul = proj->LL2PC (pul);
	plr = proj->LL2PC (plr);

// Evaluate the minimum box that includes all four corners

	return TeBox (	std::min(pll.x(),pul.x()),
					std::min(pll.y(),plr.y()),
					std::max(pur.x(),plr.x()),
					std::max(pul.y(),pur.y()));
}

