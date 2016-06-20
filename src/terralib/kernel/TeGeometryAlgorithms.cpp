/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2004 INPE and Tecgraf/PUC-Rio.

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

#include "TeGeometryAlgorithms.h"
#include "TeDefines.h"

#include "TeIntersector.h"
#include "TeFragmentation.h"

#include <algorithm>
#include <set>


using namespace std;

//---------- Local Functions ans Structures ----------//

//! Instead of using arctan, use this to compare angles, so given two points return a number to be used in place of the angle formed by the segment.
double Theta(const TeCoord2D& c1, const TeCoord2D& c2)
{
	register double dx = c2.x() - c1.x();
	register double ax = fabs(dx);
	register double dy = c2.y() - c1.y();
	register double ay = fabs(dy);
	register double t = 0.0;

	if((dx == 0.0) && (dy == 0.0))
		t = 0.0;
	else
		t = dy / (ax + ay);

	if(dx < 0.0)
		t = 2 - t;
	else
		if(dy < 0.0)
			t = 4.0 + t;

	return t * 90.0;
}

//! This struct is used for compare angles between two points and an anchor.
struct ThetaOrder
{
	//! A fixed coordinate to be used to compare.
	TeCoord2D anchor;

	ThetaOrder(const TeCoord2D& c)
		: anchor(c)
	{
	}

	// Return true if c1 and anchor has a smaller angle than c2 and anchor.
	bool operator()(const TeCoord2D& c1, const TeCoord2D& c2) const
	{
		register double t1 = Theta(anchor, c1);
		register double t2 = Theta(anchor, c2);

		if(t1 < t2)
			return true;
		
		if(t1 > t2)
			return false;

		if(c1.x() < c2.x())
			return true;

		if(c1.x() > c2.x())
			return false;

		return false;		
	}
};

/*! \struct xOrder
    \brief  Returns true if c1 < c2 otherwise, returns false.
 */
template<class T> struct xOrder
{
	bool operator()(T c1, T c2)
	{
		if(c1.x() < c2.x())
			return true;

		if(c1.x() > c2.x())
			return false;

		if(c1.y() < c2.y())
			return true;		

		return false;
	}
};

/*! \fn double TeArea(TeLinearRing& r);
    \brief This function returns twice the area of a ring treated as a simple polygon (no holes).
	\param r  The ring.

	This function returns twice the area of a ring treated as a simple polygon (no holes).
	May return a negative number => it will be used in Orientation tests.
*/
double Te2Area(const TeLinearRing& r)
{
	double S2 = 0.0;

	TeLinearRing::iterator it = r.begin();
	TeLinearRing::iterator end = r.end() - 1;
	while(it != end)
	{
		S2 += (it->x() + (it + 1)->x()) * ((it + 1)->y() - it->y());
		++it;
	}

	return S2;
}

/*! \fn short TeConverseRelation(const short& rel)
    \brief This function returns the complements relation of one given relation.
	\param rel  The relation.
*/
inline short ConverseRelation(const short& rel)
{
	switch(rel)
	{
		case TeCOVEREDBY  :  return TeCOVERS;
		case TeWITHIN     :  return TeCONTAINS;
		case TeCOVERS     :  return TeCOVEREDBY;
		case TeCONTAINS   :  return TeWITHIN;
		default			  :  return rel;
	}	
}

/*! \fn bool MayTouches(const TeLine2D& lRed, const TeLine2D& lBlue, TeINTERSECTOR2::TeReportTree& report)
    \brief This function verifies if the two lines may touches.
	\param lRed   The first line.
	\param lBlue  The second line.
	\param report The intersection point list.
*/
bool MayTouches(const TeLine2D& lRed, const TeLine2D& lBlue, TeINTERSECTOR2::TeVectorBoundaryIP& report)
{
	if(lRed.isRing() && lBlue.isRing())
		return false;

	TeINTERSECTOR2::TeVectorBoundaryIP::iterator it = report.begin();
	TeINTERSECTOR2::TeVectorBoundaryIP::iterator it_end = report.end();


	// Get the boundaries' coordinate.
	TeCoord2D line1_boundary1 = lRed[0];
	TeCoord2D line1_boundary2 = lRed[lRed.size() - 1u];

	TeCoord2D line2_boundary1 = lBlue[0];
	TeCoord2D line2_boundary2 = lBlue[lBlue.size() - 1u];

	// Verifies if intersection points are on boundary of one of the blue line
	while(it != it_end)
	{	// Get intersection point coordinate.

		for(unsigned int i = 0; i < it->coords_.size(); ++i)
		{
			TeCoord2D ip_coord = it->coords_[i];

			if(lRed.isRing())
			{
				if(!(TeEquals(ip_coord, line2_boundary1) || TeEquals(ip_coord, line2_boundary2)))
					return false;
			}
			else if(lBlue.isRing())
			{
				if(!(TeEquals(ip_coord, line1_boundary1) || TeEquals(ip_coord, line1_boundary2)))
					return false;
			}
			else
			{
				if(!(TeEquals(ip_coord, line1_boundary1) || TeEquals(ip_coord, line1_boundary2) || TeEquals(ip_coord, line2_boundary1) || TeEquals(ip_coord, line2_boundary2)))
					return false;
			}
		}

		++it;
	}

	return true;		// The lines touches.
}

/*! \fn short Relation(const TeLine2D& lRed, const TeLine2D& lBlue, TeINTERSECTOR2::TeReportTree& report, const short& relation)
    \brief This function returns the relation between two lines.
	\param lRed     The first line.
	\param lBlue    The second line.
	\param report   The intersection point list.
	\param relation The relation that stop the search.

	This function returns the relation between lines. May be: TeDISJOINT, TeTOUCHES, TeWITHIN, TeCROSSES, TeCOVEREDBY, TeEQUALS.
	Obs: Doesn't do box elimination. You must implement the test in your own functions.
*/
short Relation(const TeLine2D& lRed, const TeLine2D& lBlue, TeINTERSECTOR2::TeVectorBoundaryIP& report, const short& relation)
{
	if(report.size() == 0u)
		return TeDISJOINT;

	// Do fragmentation
	//bool hasContour = false;	// This will decide between CROSSES and OVERLAPS.

	bool mayTouches = MayTouches(lRed, lBlue, report);

	// Stop to check, because touches can't occur anymore
	if(relation == TeTOUCHES && !mayTouches)
		return TeUNDEFINEDREL;


	TeLineSet redFragments;
	TeLineSet redBoundaryFragments;
	TeLine2D lRedAux;
	lRedAux.copyElements(lRed);
	TeLineSet lSet; lSet.add(lRedAux);

	TeFragmentBoundary(lSet, report, redBoundaryFragments, redFragments);

	unsigned int redFragmentsSize = redFragments.size();

	short mask = TeUNKNOWNPOSITION;

	if(redBoundaryFragments.size() > 0u)
		mask |= TeINSIDE;

	// Do a position test for each fragment and stop if all relations have been found.
	for(unsigned int j = 0u; j < redFragmentsSize; ++j)
	{	
		TeCoord2D middle;			

		if(redFragments[j].size() ==  2u)	// If the fragment has two points I need to check the middle point of this fragment.
			TeGetMiddlePoint(redFragments[j][0u], redFragments[j][1u], middle);
		else	// If the fragment has more than two points so I check one point between the end points.
			middle = redFragments[j][(unsigned int)((double(redFragments[j].size()) / 2.0 + 0.5)) - 1];

		if(TeIsOnLine(middle, lBlue))
			mask |= TeINSIDE;
		else
			mask |= TeOUTSIDE;		

		if((mask & TeOUTSIDE) && (mask & TeINSIDE))
			break;		
	}

	// If the intersection is only at end points and there is no part of red line inside blue line so they touches
	if(mayTouches && mask == TeOUTSIDE)
	{
		return TeTOUCHES;
	}
	
	// Stop to check, because touches can't occur anymore
	if(relation == TeTOUCHES)
		return TeUNDEFINEDREL;


	// if there is no fragments in line blue interiors, so line red cross line blue
	if(mask == TeOUTSIDE)
		return TeCROSSES;

	// Stop to check, because crosses can't occur anymore
	if(relation == TeCROSSES)
		return TeUNDEFINEDREL;

	TeCoord2D firstRedCoord = lRed[0];
	TeCoord2D lastRedCoord  = lRed[lRed.size() - 1];

	TeCoord2D firstBlueCoord = lBlue[0];
	TeCoord2D lastBlueCoord  = lBlue[lBlue.size() - 1];		


	// If there is no part of red line outside blue line => must decide within, covered by and equals
	if(!(mask & TeOUTSIDE))
	{
		// So, if we arrived here, blue line contains red line, but needs to decide between equal, coveredby and within.

		if(TeEquals(firstRedCoord, firstBlueCoord) || TeEquals(firstRedCoord, lastBlueCoord))
		{
			if(TeEquals(lastRedCoord, firstBlueCoord) || TeEquals(lastRedCoord, lastBlueCoord))
			{
				return TeEQUALS;
			}
			else
			{
				if(lBlue.isRing())
					return TeWITHIN;
				else
					return TeCOVEREDBY;
			}
		}

		if(TeEquals(lastRedCoord, firstBlueCoord) || TeEquals(lastRedCoord, lastBlueCoord))
		{
			if(lBlue.isRing())
				return TeWITHIN;
			else
				return TeCOVEREDBY;
		}

		return TeWITHIN;
	}

	return TeUNDEFINEDREL;
}

/*! \fn short Relation(const TeLine2D& line, const TeLinearRing& ring, const short& relation)
    \brief This function returns the relation between a line and a linear ring (treated as a simple polygon with no holes).
	\param line     The line.
	\param ring     The simple polygon ring.
	
	This function returns how the points of input line are related with interior, boundary, and
	exterior of polygon. The mask may be a combination of the following values: TeOUTSIDE, TeINSIDE and TeBOUNDARY.
	Obs: Do box elimination.
*/
short Relation(const TeLine2D& line, const TeLinearRing& ring)
{
	if(TeDisjoint(line.box(), ring.box()))
		return TeOUTSIDE;

	TeINTERSECTOR2::TeVectorBoundaryIP report;

	if(TeINTERSECTOR2::TeSafeIntersections(line, ring, report))
	{
		short mask = TeBOUNDARY;	// It will be at least intersections between boundaries.

		// Fragments line.
		TeLineSet lSet;
		TeLineSet boundaryFragments;
		TeLine2D lineAux;
		lineAux.copyElements(line);
		TeLineSet auxLineSet; auxLineSet.add(lineAux);
		
		TeFragmentBoundary(auxLineSet, report, boundaryFragments, lSet);


		unsigned int lSetSize = lSet.size();

		// Do a position test for each fragment and stop if al relations has been found.
		for(unsigned int j = 0; j < lSetSize; ++j)
		{	
			TeCoord2D middle;
			
			if(lSet[j].size() ==  2)	// If the fragment has two points I need to check the middle point of this fragment.
				TeGetMiddlePoint(lSet[j][0], lSet[j][1], middle);
			else	// If the fragment has more than two points so I check one point between the end points.
				middle = lSet[j][(unsigned int)((double(lSet[j].size()) / 2.0 + 0.5)) - 1];

			
			mask |= TeRelation(middle, ring);

			if((mask & TeOUTSIDE) && (mask & TeINSIDE))
				break;			
		}

		return mask;
	}
	else
		return TeRelation(line[0], ring);	// Needs to test only one point of the line, and its location defines the line location.
}

/*! \fn short TopologicRelation(const TeLine2D& line, const TeLinearRing& r)
    \brief This function returns the relation between a line and a linear ring (treated as a simple polygon with no holes).
	\param line     The line.
	\param r        The simple polygon ring.

	This function returns the topologic relation between the line and
	the ring. The result may be: DISJOINT, WITHIN, TOUCHES, CROSSES or COVERED BY.
	Obs: Doesn't do box elimination. Just uses from TeRelation.
*/
short TopologicRelation(const TeLine2D& line, const TeLinearRing& r)
{
	short mask = Relation(line, r);

	if(mask & TeBOUNDARY)	// TOUCHES or CROSSES or COVERED BY
	{
		if(mask & TeOUTSIDE)	// TOUCHES or CROSSES
		{
			if(mask & TeINSIDE)
				return TeCROSSES;
			else
				return TeTOUCHES;
		}
		else	// COVERED BY
			return TeCOVEREDBY;

	}
	else	// DISJOINT or WITHIN
	{
		if(mask & TeINSIDE)	
			return TeWITHIN;
		else				
			return TeDISJOINT;
	}
}

/*! \fn short TopologicRelation(const TeLinearRing& rRed, const TeLinearRing& rBlue)
    \brief This function returns the relationship between two rings (treated as simple polygons with no holes).
	\param rRed  The ring to test relationship.
	\param rBlue The ring to test relationship.

	This function returns the relationship between two rings.
*/
short TopologicRelation(const TeLinearRing& rRed, const TeLinearRing& rBlue)
{
	// See the intersection between the points of rRed and the three components of rBlue (Boundary, Exterior, Interior)
	short rel = Relation(rRed, rBlue);

	if((rel & TeOUTSIDE) && (rel & TeINSIDE) && (rel & TeBOUNDARY))
		return TeOVERLAPS;

	if((rel & TeOUTSIDE) && !(rel & TeINSIDE) && (rel & TeBOUNDARY))
	{
		if(TeWithinOrCoveredByOrEquals(rBlue.box(), rRed.box()))
		{
			short rel_aux = Relation(rBlue, rRed);

			if((rel_aux & TeINSIDE) && !(rel_aux & TeOUTSIDE) && (rel_aux & TeBOUNDARY))
				return TeCOVERS;
		}
		
		return TeTOUCHES;
	}

	if((rel & TeOUTSIDE) && !(rel & TeINSIDE) && !(rel & TeBOUNDARY))
	{
		if(TeWithinOrCoveredByOrEquals(rBlue.box(), rRed.box()))
		{
			if(TeRelation(rBlue[0], rRed) == TeINSIDE)
				return TeCONTAINS;
		}
		
		return TeDISJOINT;
	}

	if(!(rel & TeOUTSIDE) && (rel & TeINSIDE) && !(rel & TeBOUNDARY))
		return TeWITHIN;

	if(!(rel & TeOUTSIDE) && (rel & TeINSIDE) && (rel & TeBOUNDARY))
		return TeCOVEREDBY;

	if(rel == TeBOUNDARY)
		return TeEQUALS;

	return TeINTERSECTS;
}

/*! \fn short TeTopologicRelation(const TeLinearRing& rRed, const TeLinearRing& rBlue, vector<TeLinearRing>& rings, short& rel)
    \brief This function returns the relationship between the ring and the polygon inner rings.
	\param rRed  The ring to test relationship.
	\param pBlue The polygon to test relationship.
	\param rings Inner rings from blue polygon that is inside or is covered by red external polygon.
	\param rel   The relation between external red ring and external blue ring.

	This function returns the relationship between the ring and the polygon inner rings.
*/
short LookAtInnerRings(const TeLinearRing& rRed, const TePolygon& pBlue, vector<TeLinearRing>& rings, short& rel)
{
	register unsigned int i = 1;
	register unsigned int nRings = pBlue.size();

	for(; i < nRings; ++i)
	{
		switch(TopologicRelation(rRed, pBlue[i]))
		{
			case TeCOVEREDBY : return TeTOUCHES;

			case TeWITHIN	 : return TeDISJOINT;

			case TeOVERLAPS  : return TeOVERLAPS;

			case TeEQUALS    : return TeTOUCHES;

			case TeDISJOINT  : continue;	// this ring is outside the external ring, so it doesn't contribute to the relationship.

			case TeTOUCHES    : rel = TeCOVEREDBY;	// change rel if rel is within.
				                continue;

			case TeCOVERS    : rings.push_back(pBlue[i]);
							   rel = TeCOVEREDBY;
							   break;

			case TeCONTAINS  : rings.push_back(pBlue[i]);
							   break;
		}
	}

	return TeUNDEFINEDREL;
}

/*! \fn short TestInnerRings(const TePolygon& pRed, vector<TeLinearRing>& rings)
    \brief This function returns the relationship between the ring and the polygon inner rings.
	\param pRed  The inner rings from pRed to test relationship.
	\param rings Inner rings from blue polygon that is inside or is covered by red external polygon.

	This function returns the relationship between the inner ring of pRed and the inner rings in rings.
*/
short TestInnerRings(const TePolygon& pRed, vector<TeLinearRing>& rings)
{
	unsigned int nRedRings = pRed.size();
	unsigned int nBlueRings = rings.size();

	if((nRedRings - 1) != nBlueRings)
		return TeOVERLAPS;

	unsigned int i = 1;
	unsigned int j = 0;

	bool find = false;

	set<unsigned int> blueRingsProcessed;

	set<unsigned int> redRingsContainsCovers;

	short rel = 0;

	for(; i < nRedRings; ++i)
	{
		find = false;

		for(j = 0; j < nBlueRings; ++j)
		{
			if(blueRingsProcessed.find(j) != blueRingsProcessed.end())
				continue;

			switch(TopologicRelation(pRed[i], rings[j]))
			{
				case TeDISJOINT  : continue;	// so it doesn't contribute to the relationship yet.

				case TeEQUALS    :
									rel |= TeEQUALS;
					                blueRingsProcessed.insert(j);
									find = true;
									break;

				case TeCONTAINS  :
									if(redRingsContainsCovers.find(i) != redRingsContainsCovers.end())
										return TeOVERLAPS;
									
									rel |= TeCONTAINS;

									redRingsContainsCovers.insert(i);
									blueRingsProcessed.insert(j);
									find = true;
									break;

				case TeCOVERS	:
									if(redRingsContainsCovers.find(i) != redRingsContainsCovers.end())
										return TeOVERLAPS;
									
									rel |= TeCOVERS;

									redRingsContainsCovers.insert(i);
									blueRingsProcessed.insert(j);
									find = true;
									break;
									

				default          :  return TeOVERLAPS;
			}

			if(find)
				break;
		}

		if(!find)
			return TeOVERLAPS;
	}

	return rel;
}

//---------- Topological Function ----------//

// EQUALS
template<> bool TeEquals(const TeCoord2D& c1, const TeCoord2D& c2)
{
	if(TeGeometryAlgorithmsPrecision::IsDifferent(c1.x(), c2.x()))
		return false;

	if(TeGeometryAlgorithmsPrecision::IsDifferent(c1.y(), c2.y()))
		return false;

	return true;
}

template<> bool TeEquals(const TePoint& p1, const TePoint& p2)
{
	return TeEquals(p1.location(), p2.location());
}

template<> bool TeEquals(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeEquals(redLine.box(), blueLine.box()))
		return TeRelation(redLine, blueLine, TeEQUALS) == TeEQUALS;
	else
		return false;
}

template<> bool TeEquals(const TePolygon& redPol, const TePolygon& bluePol)
{
	if(TeEquals(redPol.box(), bluePol.box()))
	{
		if(redPol.size() != bluePol.size())
			return false;

		return TeRelation(redPol, bluePol) == TeEQUALS;
	}
	else
		return false;
}

template<> bool TeEquals( const TePolygonSet& ps1, const TePolygonSet& ps2 )
{
  if( ps1.size() == ps2.size() ) {
    TePolygonSet::iterator it1 = ps1.begin();
    TePolygonSet::iterator it1_end = ps1.end();
    TePolygonSet::iterator it2 = ps2.begin();
    
    while( it1 != it1_end ) {
      if( ! TeEquals( (*it1), (*it2) ) ) {
        return false;
      }
      
      ++it1;
      ++it2;
    }
  
    return true;
  } else {
    return false;
  }
}

template<> bool TeEquals(const TeBox& bx1, const TeBox& bx2)
{
	if(TeGeometryAlgorithmsPrecision::IsDifferent(bx1.x1(), bx2.x1()))
		return false;

	if(TeGeometryAlgorithmsPrecision::IsDifferent(bx1.y1(), bx2.y1()))
		return false;

	if(TeGeometryAlgorithmsPrecision::IsDifferent(bx1.x2(), bx2.x2()))
		return false;
	
	if(TeGeometryAlgorithmsPrecision::IsDifferent(bx1.y2(), bx2.y2()))
		return false;

	return true;
}

template<> bool TeEquals(const TeCell& cell1, const TeCell& cell2)
{
	return TeEquals(cell1.box(), cell2.box());
}


// DISJOINT
bool TeDisjoint(const TeCoord2D& c1, const TeCoord2D& c2)
{
	if(TeGeometryAlgorithmsPrecision::IsDifferent(c1.x(), c2.x()))
		return true;

	if(TeGeometryAlgorithmsPrecision::IsDifferent(c1.y(), c2.y()))
		return true;

	return false;
}

bool TeDisjoint(const TeCoord2D& c, const TeBox& b)
{
	// c to the right of b
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.x(), b.x2())) 
		return true;

	// c to the left of b
	if(TeGeometryAlgorithmsPrecision::IsGreater(b.x1(), c.x())) 
		return true;

	// c is above b
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.y(), b.y2())) 
		return true;

	// c is below b 
	if(TeGeometryAlgorithmsPrecision::IsGreater(b.y1(), c.y())) 
		return true;

	return false;
}

bool TeDisjoint(const TeBox& bx1, const TeBox& bx2)
{
	// B1 to the right of B2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.x1(), bx2.x2())) 
		return true;

	// B1 to the left of B2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.x1(), bx1.x2())) 
		return true;

	// B2 is above B1
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.y1(), bx1.y2())) 
		return true;

	// B2 is below B1 
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.y1(), bx2.y2())) 
		return true;

	return false;
}

bool TeDisjoint(const TeCoord2D& c, const TeLine2D& l)
{
	return TeRelation(c, l) == TeOUTSIDE;
}

bool TeDisjoint(const TeCoord2D& c, const TePolygon& pol)
{
	return TeRelation(c, pol) == TeOUTSIDE;
}

bool TeDisjoint(const TePoint& p1, const TePoint& p2)
{
	return TeDisjoint(p1.location(), p2.location());
}

bool TeDisjoint(const TePoint& p, const TeLine2D& l)
{
	return TeDisjoint(p.location(), l);
}

bool TeDisjoint(const TePoint& p, const TePolygon& pol)
{
	return TeDisjoint(p.location(), pol);
}

bool TeDisjoint(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeDisjoint(redLine.box(), blueLine.box()))
		return true;

	return !TeINTERSECTOR2::TeIntersects(redLine, blueLine);
}

bool TeDisjoint(const TeLine2D& l, const TePolygon& pol)
{
	if(TeDisjoint(l.box(), pol.box()))
		return true;

	return TeRelation(l, pol) == TeDISJOINT;
}

bool TeDisjoint(const TePolygon& redPol, const TePolygon& bluePol)
{
	if(TeDisjoint(redPol.box(), bluePol.box()))
		return true;
	else
		return TeRelation(redPol, bluePol) == TeDISJOINT;
}

bool TeDisjoint(const TeCell& cell1, const TeCell& cell2)
{
	return TeDisjoint(cell1.box(), cell2.box());
}

bool TeDisjoint(const TeCell& cell, const TeLine2D& line)
{
	return TeDisjoint(line, TeMakePolygon(cell.box()));
}

bool TeDisjoint(const TeCell& cell, const TePolygon& pol)
{
	return TeDisjoint(pol, TeMakePolygon(cell.box()));
}

bool TeDisjoint(const TeCell& cell, const TePoint& point)
{
	return TeDisjoint(point.location(), cell.box());
}


// INTERSECTS

template<> bool TeIntersects(const TeCoord2D& c, const TeBox& b)
{
	// c to the right of b
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.x(), b.x2())) 
		return false;

	// c to the left of b
	if(TeGeometryAlgorithmsPrecision::IsGreater(b.x1(), c.x())) 
		return false;

	// c is above b
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.y(), b.y2())) 
		return false;

	// c is below b 
	if(TeGeometryAlgorithmsPrecision::IsGreater(b.y1(), c.y())) 
		return false;

	return true;
}

template<> bool TeIntersects(const TePoint& p, const TeBox& b)
{
	return TeIntersects(p.location(), b);
}

template<> bool TeIntersects(const TeBox& bx1, const TeBox& bx2)
{
	// B1 to the right of B2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.x1(), bx2.x2())) 
		return false;

	// B1 to the left of B2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.x1(), bx1.x2())) 
		return false;

	// B2 is above B1
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.y1(), bx1.y2())) 
		return false;

	// B2 is below B1 
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.y1(), bx2.y2())) 
		return false;

	return true;
}


// TOUCHES
bool TeTouches(const TeCoord2D& c, const TeLine2D& l)
{
	return TeRelation(c, l) == TeBOUNDARY;
}

bool TeTouches(const TeCoord2D& c, const TePolygon& pol)
{
	return TeRelation(c, pol) == TeBOUNDARY;
}

bool TeTouches(const TePoint& p, const TeLine2D& l)
{
	return TeTouches(p.location(), l);
}

bool TeTouches(const TePoint& p, const TePolygon& pol)
{
	return TeTouches(p.location(), pol);
}

bool TeTouches(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeDisjoint(redLine.box(), blueLine.box()))
		return false;

	return TeRelation(redLine, blueLine, TeTOUCHES) == TeTOUCHES;	
}

bool TeTouches(const TeLine2D& l, const TePolygon& pol)
{
	if(TeDisjoint(l.box(), pol.box()))
		return false;

	return TeRelation(l, pol) == TeTOUCHES;
}

bool TeTouches(const TePolygon& redPol, const TePolygon& bluePol)
{
	if(TeDisjoint(redPol.box(), bluePol.box()))
		return false;

	return TeRelation(redPol, bluePol) == TeTOUCHES;
}

bool TeTouches(const TeBox& bx1, const TeBox& bx2)
{	
	// bx1 may touches its right wall in bx2 left wall
	// or bx1 may touches its left wall in bx2 right wall
	if(TeGeometryAlgorithmsPrecision::IsEqual(bx1.x2(), bx2.x1()) || TeGeometryAlgorithmsPrecision::IsEqual(bx1.x1(), bx2.x2()))
	{
		// bx1 is below bx2
		if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.y1(), bx1.y2()))
			return false;

		// bx1 is above bx2
		if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.y1(), bx2.y2()))
			return false;

		// touches
		return true;	
	}

	// bx1 may touches its bottom wall in bx2 top wall
	// or bx1 may touches its top wall in bx2 bottom wall
	if(TeGeometryAlgorithmsPrecision::IsEqual(bx1.y1(), bx2.y2()) || TeGeometryAlgorithmsPrecision::IsEqual(bx1.y2(), bx2.y1()))
	{
		// bx1 is left of bx2
		if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.x1(), bx1.x2()))
			return false;

		// bx1 is right of bx2
		if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.x1(), bx2.x2()))
			return false;

		// touches
		return true;
	}

	// doesn't touches
	return false;
}

bool TeTouches(const TeCell& c1, const TeCell& c2)
{
	return TeTouches(c1.box(), c2.box());
}

bool TeTouches(const TeLine2D& line, const TeCell& cell)
{
	return TeTouches(line, TeMakePolygon(cell.box()));
}

bool TeTouches(const TeCell& c1, const TePolygon& poly)
{
	return  TeTouches (poly, TeMakePolygon(c1.box()));
}

bool TeTouches(const TePoint& point,const TeCell& c1)
{
	return TeTouches(point.location(), TeMakePolygon(c1.box()));
}


// CROSSES
bool TeCrosses(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeDisjoint(redLine.box(), blueLine.box()))
		return false;
	else
		return TeRelation(redLine, blueLine, TeCROSSES) == TeCROSSES;
}

bool TeCrosses(const TeLine2D& l, const TePolygon& pol)
{
	if(TeDisjointOrTouches(l.box(), pol.box()))
		return false;
	else
		return TeRelation(l, pol) == TeCROSSES;
}

bool TeCrosses(const TeLine2D& l, const TeCell& cell)
{
	return TeCrosses(l, TeMakePolygon(cell.box()));
}


// WITHIN
bool TeWithin(const TeCoord2D& c1, const TeCoord2D& c2)
{
	return TeEquals(c1, c2);
}

bool TeWithin(const TeCoord2D& c, const TeBox& b)
{
	// c to the right of b left wall AND c to the left of b right wall
	// AND c below b top wall AND c above b bottom wall => then c is on b interior.
	return (TeGeometryAlgorithmsPrecision::IsGreater(c.x(), b.x1()) && TeGeometryAlgorithmsPrecision::IsGreater(b.x2(), c.x()) && TeGeometryAlgorithmsPrecision::IsGreater(b.y2(), c.y()) && TeGeometryAlgorithmsPrecision::IsGreater(c.y(), b.y1()));
}

bool TeWithin(const TeCoord2D& c, const TeLine2D& l)
{
	return TeRelation(c, l) == TeINSIDE;
}

bool TeWithin(const TeCoord2D& c, const TePolygon& pol)
{
	return TeRelation(c, pol) == TeINSIDE;
}

bool TeWithin(const TePoint& p1, const TePoint& p2)
{
	return TeWithin(p1.location(), p2.location());
}

bool TeWithin(const TePoint& p, const TeLine2D& l)
{
	return TeWithin(p.location(), l);
}

bool TeWithin(const TePoint& p, const TePolygon& pol)
{
	return TeWithin(p.location(), pol);
}

bool TeWithin(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeWithinOrCoveredByOrEquals(redLine.box(), blueLine.box()))
		return TeRelation(redLine, blueLine, TeWITHIN) == TeWITHIN;	
	else
		return false;
}

bool TeWithin(const TeLine2D& l, const TePolygon& pol)
{
	if(TeWithinOrCoveredByOrEquals(l.box(), pol.box()))	
		return TeRelation(l, pol) == TeWITHIN;


	return false;
}

bool TeWithin(const TePolygon& redPol, const TePolygon& bluePol)
{
	if(TeWithinOrCoveredByOrEquals(redPol.box(), bluePol.box()))	
		return TeRelation(redPol, bluePol) == TeWITHIN;
	else
		return false;
}

bool TeWithin(const TeBox& bx1, const TeBox& bx2)
{
	// bx1 left wall is left of or on bx2 left wall
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx2.x1(), bx1.x1()))
		return false;

	// bx1 right wall is right of or on bx2 right wall
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx1.x2(), bx2.x2()))
		return false;

	// bx1 is below bx2 or on.
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx2.y1(), bx1.y1()))
		return false;

	// bx1 is above bx2 or on
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx1.y2(), bx2.y2()))
		return false;

	return true;	
}

bool TeWithin(const TeCell& cell1, const TeCell& cell2)
{
	return TeWithin(cell1.box(), cell2.box());
}

bool TeWithin(const TeLine2D& line, const TeCell& cell)
{
	return TeWithin(line, TeMakePolygon(cell.box()));
}


bool TeWithin(const TeCell& cell, const TePolygon& poly)
{
	return TeWithin(TeMakePolygon(cell.box()), poly);
}

bool TeWithin(const TePoint& point, const TeCell& cell)
{
	return TeWithin(point.location(), cell.box());
}


// OVERLAPS
bool TeOverlaps(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeDisjoint(redLine.box(), blueLine.box()))
		return false;
	else
		return TeRelation(redLine, blueLine, TeOVERLAPS) == TeOVERLAPS;
}

bool TeOverlaps(const TePolygon& redPol, const TePolygon& bluePol)
{
	//if(TeDisjoint(redPol.box(), bluePol.box()))
	if(TeDisjointOrTouches(redPol.box(), bluePol.box()))
		return false;
	else
		return TeRelation(redPol, bluePol) == TeOVERLAPS;
}

bool TeOverlaps(const TeCell& cell1, const TeCell& cell2)
{
	return TeOverlaps(TeMakePolygon(cell1.box()), TeMakePolygon(cell2.box()));
}

bool TeOverlaps(const TeCell& cell, const TePolygon& poly)
{
	return TeOverlaps(TeMakePolygon(cell.box()), poly);
}


// COVERED BY
bool TeCoveredBy(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(TeWithinOrCoveredByOrEquals(redLine.box(), blueLine.box()))
		return TeRelation(redLine, blueLine, TeCOVEREDBY) == TeCOVEREDBY;
	else
		return false;
}

bool TeCoveredBy(const TeLine2D& l, const TePolygon& pol)
{
	if(TeWithinOrCoveredByOrEquals(l.box(), pol.box()))
		return TeRelation(l, pol) == TeCOVEREDBY;
	else
		return false;
}

bool TeCoveredBy(const TePolygon& redPol, const TePolygon& bluePol)
{
	if(TeWithinOrCoveredByOrEquals(redPol.box(), bluePol.box()))
		return TeRelation(redPol, bluePol) == TeCOVEREDBY;
	else
		return false;
}

bool TeCoveredBy(const TeCell& cell1, const TeCell& cell2)
{
	return TeCoveredBy(TeMakePolygon(cell1.box()), TeMakePolygon(cell2.box()));
}

bool TeCoveredBy(const TePolygon& poly, const TeCell& cell)
{
	return TeCoveredBy(poly, TeMakePolygon(cell.box()));
}

bool TeCoveredBy(const TeLine2D& line, const TeCell& cell)
{
	return TeCoveredBy(line, TeMakePolygon(cell.box()));
}

//---------- Box Tests ----------//

bool TeDisjointOrTouches(const TeBox& bx1, const TeBox& bx2)
{
	// B1 to the right of or on B2
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx1.x1(), bx2.x2())) 
		return true;

	// B1 to the left of or on B2
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx2.x1(), bx1.x2())) 
		return true;

	// B2 is above or on B1
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx2.y1(), bx1.y2())) 
		return true;

	// B2 is below or on B1 
	if(TeGeometryAlgorithmsPrecision::IsGreaterEqual(bx1.y1(), bx2.y2())) 
		return true;

	return false;
}

template<>
bool TeWithinOrCoveredByOrEquals(const TeBox& bx1, const TeBox& bx2)
{
	// bx1 left wall is left of bx2 left wall
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.x1(), bx1.x1()))
		return false;

	// bx1 right wall is right of bx2 right wall
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.x2(), bx2.x2()))
		return false;

	// bx1 is below bx2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx2.y1(), bx1.y1()))
		return false;

	// bx1 is above bx2
	if(TeGeometryAlgorithmsPrecision::IsGreater(bx1.y2(), bx2.y2()))
		return false;

	return true;	
}

template<>
bool TeWithinOrCoveredByOrEquals(const TeLine2D& line1, const TeLine2D& line2)
{
	if(TeWithinOrCoveredByOrEquals(line1.box(), line2.box()))
	{
		short r;
		short rel = TeRelation(line1, line1, r);

		if((rel&TeWITHIN) || (rel&TeCOVEREDBY) || (rel&TeEQUALS))
		   return true;

		return false;
	}
	return false;
}

template<>
bool TeWithinOrCoveredByOrEquals(const TeLine2D& line1, const TePolygon& pol)
{
	if(TeWithinOrCoveredByOrEquals(line1.box(), pol.box()))
	{
		short rel = TeRelation(line1, pol);

		if((rel&TeWITHIN) || (rel&TeCOVEREDBY))
		   return true;

		return false;
	}
	return false;
}

template<>
bool TeWithinOrCoveredByOrEquals(const TePolygon& pol1, const TePolygon& pol2)
{
	if(TeWithinOrCoveredByOrEquals(pol1.box(), pol2.box()))
	{
		short rel = TeRelation(pol1, pol2);

		if((rel&TeWITHIN) || (rel&TeCOVEREDBY) || (rel&TeEQUALS))
		   return true;

		return false;
	}
	return false;
}


bool TeWithinOrTouches(const TeCoord2D& c, const TeCoord2D& c1, const TeCoord2D& c2)
{
	// c is to the right of c1 and c2
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.x(), c1.x()) && TeGeometryAlgorithmsPrecision::IsGreater(c.x(), c2.x()))
		return false;

	// c is above c1 and c2
	if(TeGeometryAlgorithmsPrecision::IsGreater(c.y(), c1.y()) && TeGeometryAlgorithmsPrecision::IsGreater(c.y(), c2.y()))
		return false;

	// c is to the left of c1 and c2
	if(TeGeometryAlgorithmsPrecision::IsSmaller(c.x(), c1.x()) && TeGeometryAlgorithmsPrecision::IsSmaller(c.x(), c2.x()))
		return false;

	// c is below c1 and c2
	if(TeGeometryAlgorithmsPrecision::IsSmaller(c.y(), c1.y()) && TeGeometryAlgorithmsPrecision::IsSmaller(c.y(), c2.y()))
		return false;

	return true;	
}


//---------- Intersection Functions ----------//

bool TeIntersection(const TeBox& bx1, const TeBox& bx2, TeBox& bout)
{
	if(TeDisjoint(bx1, bx2))
		return false;

	double x1 = MAX(bx1.x1(),bx2.x1());
	double x2 = MIN(bx1.x2(),bx2.x2());
	double y1 = MAX(bx1.y1(),bx2.y1());
	double y2 = MIN(bx1.y2(),bx2.y2());

	bout = TeBox(x1, y1, x2, y2);

	return true;
}

TeCoordPairVect	TeGetIntersections(const TePolygon &poly, const double& y)
{
	TeCoordPairVect		Segments;
	vector<double>		crossList, segListInY;
	unsigned int		nholes = poly.size();
			
	//for each ring of the polygon
	for (unsigned int count=0; count<nholes; count++ ) 		
	{
		TeLinearRing coords = poly[count];
		//for each segment of the ring
		for  (unsigned int j=0;  j < (coords.size() - 1); j++ ) 		
		{
			// Get one segment
			TeCoord2D coord0 = coords[j];
			TeCoord2D coord1 = coords[j+1];

			bool yflag0 = ( coord0.y() > y );  
			bool yflag1 = ( coord1.y() > y ); 

			//treating a special case: when the segment touches or is ON the y axe  
			//if there is a special case, we must test if the middle point
			//of each segment is inside or outside of the polygon 
			if((coord0.y()!=y) && (coord1.y()==y)) 
			{
				bool pointsInY = true;
				bool lowerY1 = coord0.y()<y;
                unsigned int i = j+2;
				
				TeCoord2D lastPointInY;
				TeCoord2D firstPointOutY = coord1;
				
				while(pointsInY) 
				{
					lastPointInY = firstPointOutY; 

					// Get the next point 
					if(i>=coords.size())
						i=1;
                    firstPointOutY = coords[i];
					++i;

					//Verify if it is on the y axe
					pointsInY = firstPointOutY.y()==y;
				}

				bool lowerY2 = firstPointOutY.y()<y;
				//the segment touches the y axe only in one point and cross the axe Y
				if((lastPointInY==coord1) && (lowerY1!=lowerY2)) 
					crossList.push_back(coord1.x());	
				else 
				{
					segListInY.push_back (coord1.x());
					segListInY.push_back (lastPointInY.x());
				}
			}
			else if(coord0.y()==y)
				continue;
			// line crosses ring horizontally 
			else if ( yflag0 != yflag1 )
			{ 
				double slope =  ( coord1.x() - coord0.x() ) / ( coord1.y() - coord0.y());
				double xcross = ( y -   coord0.y() )* slope + coord0.x();
				crossList.push_back (xcross);	
			}
		}
	}
	
	if(crossList.empty())
		crossList = segListInY;
	else if(!segListInY.empty())
	{
		//insert segListInY in the cross list 
		for(unsigned i=0; i<segListInY.size(); ++i) 
			crossList.push_back (segListInY[i]);

		sort (crossList.begin(), crossList.end());

		vector<double> aux;
		//Verify if the segment middle point intersects the polygon 
		for(unsigned i=1; i<crossList.size(); ++i)
		{
			//calculates the middle point
			double x0 = crossList[i-1];
			double x1 = crossList[i];
			double x = x0 + (x1-x0)/2;
			
			TeCoord2D pt (x, y);
			if (!TeDisjoint(pt, poly)) 
			{
				aux.push_back (x0);
				aux.push_back (x1);
			}
		}
		crossList.clear();
		crossList = aux;
    }
	
	// Sort the x-intersections
	sort (crossList.begin(), crossList.end());
	
	// Make the result segments
	vector<double>::iterator it = crossList.begin();
	while ( it != crossList.end())
	{
		TeCoordPair cp;

		cp.pt1 = TeCoord2D ( (*it), y);
		++it;
		if ( it == crossList.end() ) 
			break;

		cp.pt2 = TeCoord2D ( (*it), y);
		++it;

		Segments.push_back ( cp );
	}
	return Segments;
}

//---------- Union Operators ----------//

TeBox TeUnion(const TeBox& bx1, const TeBox& bx2)
{
	double x1 = MIN(bx1.x1(), bx2.x1());
	double y1 = MIN(bx1.y1(), bx2.y1());
		
	double x2 = MAX(bx1.x2(), bx2.x2());
	double y2 = MAX(bx1.y2(), bx2.y2());		

	return TeBox(x1, y1, x2, y2);
}


//---------- Localization Functions ----------//

bool TePointInPoly(const TeCoord2D& c, const TeLinearRing& r)
{
	if(r.size() < 4)
		return false;

	register double	ty, tx;

	register const unsigned int nVertices = r.size();

	register bool inside_flag = false;

	register int j, yflag0, yflag1;

	TeLinearRing::iterator vtx0, vtx1;

	tx = c.x();
    ty = c.y();
    
	vtx0 = r.end() - 2;

    yflag0 = (vtx0->y() >= ty);

    vtx1 = r.begin();

    for(j = nVertices; --j; )
	{
		yflag1 = (vtx1->y() >= ty);
		
		if(yflag0 != yflag1)
		{
			if(((vtx1->y() - ty) * (vtx0->x() - vtx1->x()) >=
		        (vtx1->x() - tx) * (vtx0->y() - vtx1->y())) == yflag1)
			{
				inside_flag = !inside_flag ;
			}
		}

		yflag0 = yflag1 ;
		vtx0 = vtx1 ;
		vtx1++;
    }

	return inside_flag;
}

bool TeIsOnSegment(const TeCoord2D& c, const TeCoord2D& c1, const TeCoord2D& c2)
{
	if(TeWithinOrTouches(c, c1, c2))
	{
		bool aux = false;

		return TeINTERSECTOR2::TeCCW(c1, c2, c, aux) == TeNOTURN;
	}

	return false;	
}

bool TeIsOnLine(const TeCoord2D& c, const TeLine2D& l)
{
	if(l.size() < 2)
		return false;

	if(TeDisjoint(c, l.box()))
		return false;

	TeLine2D::iterator it = l.begin();

	unsigned int nstep = l.size() - 1;

	for(unsigned int i = 0; i < nstep; ++i)
	{
		if(TeIsOnSegment(c, *(it), *(it + 1)))
			return true;
		
		++it;
	}
	
	return false;
}

bool TeLocateLineSegment (TeCoord2D& pin, TeLine2D& line, int& segment, double /*tol*/)
{
	if (line.size() < 2) 
		return false;

	TeCoord2D pout;
		
	segment = 0;
	double min_dist = TeMinimumDistance(line[0], line[1], pin, pout);
	
	for (unsigned int i = 2; i < line.size(); i++)
	{
		double dist;
		if ((dist = TeMinimumDistance (line[i-1],line[i], pin, pout)) < min_dist)
		{		
				min_dist = dist;
				segment = i - 1;
		}
	}

	return true;
}


//---------- Area Functions ----------//

template<> double TeGeometryArea(const TePolygon& p)
{
	if(p.size() > 0)
	{
		TePolygon::iterator it = p.begin();
	
		double area = fabs(Te2Area(*it));

		++it;
		
		// subtract inner rings area.
		while(it != p.end())
		{
			area -= fabs(Te2Area(*it));
			++it;
		}

		return (area / 2.0);
	}
	
	return 0.0;
}

template<> double TeGeometryArea(const TePolygonSet& ps)
{
	TePolygonSet::iterator it = ps.begin();
	
	double area = 0.0;
	
	while(it != ps.end())
	{
		area += TeGeometryArea(*it);
		++it;
	}

	return (area);
}

template<> double TeGeometryArea(const TeBox& b)
{
	return ((b.x2() - b.x1()) * (b.y2() - b.y1()));
}

template<> double TeGeometryArea(const TeMultiGeometry& mGeom)
{
	if(mGeom.hasPolygons())
	{
		TePolygonSet pSet;
		mGeom.getGeometry(pSet);
		return TeGeometryArea(pSet);
	}
	else if(mGeom.hasCells())
	{
		TeCellSet cSet;
		mGeom.getGeometry(cSet);
		return TeGeometryArea(cSet);
	}
	return 0.;
}

//---------- ConvexHull ----------//

//! If we have the  two end point equals, so we remove it.
void removeDuplicatedCoords(vector<TeCoord2D>& coordSet)
{
	if(coordSet[0] == coordSet[coordSet.size() - 1])
		coordSet.erase(coordSet.end() - 1);

	return;
}

// Return a linear ring that is the convex hull of a given list of coords
TePolygon ConvexHull(vector<TeCoord2D>& coordSet)
{
	// sorting the coords
	std::sort(coordSet.begin(), coordSet.end(), xOrder<TeCoord2D>());

	register unsigned int i = 0;
	register unsigned int n = coordSet.size();

	TeLine2D upperHull;
	TeLine2D lowerHull;

	lowerHull.add(coordSet[0]);
	lowerHull.add(coordSet[1]);

	unsigned int count = 2;

	bool aux = false;
	
	for(i = 2; i < n; ++i)
	{
		lowerHull.add(coordSet[i]);

		++count;

		while(count > 2 && TeINTERSECTOR2::TeCCW(lowerHull[count - 3], lowerHull[count - 2], lowerHull[count - 1], aux) <= TeNOTURN)
		{
			lowerHull.erase(count - 2);
			--count;
		}
	}

	upperHull.add(coordSet[n - 1]);
	upperHull.add(coordSet[n - 2]);

	count = 2;

	for(i = n - 2;  i > 0;  --i)
	{
		upperHull.add(coordSet[i - 1]);
		++count;

		while(count > 2 && TeINTERSECTOR2::TeCCW(upperHull[count - 3], upperHull[count - 2], upperHull[count - 1], aux) <= TeNOTURN)
		{
			upperHull.erase(count - 2);
			--count;
		}
	}

	
	upperHull.erase(0);
	upperHull.erase(upperHull.size() - 1);
	
	lowerHull.copyElements(upperHull);
	lowerHull.add(lowerHull[0]);

	TeLinearRing aux_ring(lowerHull);
	TePolygon p;
	p.add(aux_ring);
	return p;
}

template<class T> TePolygon TeConvexHull(const T& coordSet)
{
	// creates an auxiliary line with the points of the ring
	vector<TeCoord2D> aux;

	typename T::iterator it = coordSet.begin();
	
	while(it != coordSet.end())
	{
		aux.push_back(*it);
		++it;
	}

	// removes duplicated coords from structs like ring
	removeDuplicatedCoords(aux);

	return ConvexHull(aux);
}

template<> TePolygon TeConvexHull(const TePolygon& p)
{
	vector<TeCoord2D> coords;
	back_insert_iterator<vector<TeCoord2D> > it(coords);
	// Copy the first ring of each polygon without the last point (that is equals to the first).
	
	copy(p[0].begin(), p[0].end() - 1, it);

	return ConvexHull(coords);
}

template<> TePolygon TeConvexHull(const TePolygonSet& ps)
{
	vector<TeCoord2D> coords;
	back_insert_iterator<vector<TeCoord2D> > it(coords);
	// Copy the first ring of each polygon without the last point (that is equals to the first).
	
	TePolygonSet::iterator it_ps = ps.begin();
	while(it_ps != ps.end())
	{
		TeLinearRing r = (*it_ps)[0];
		copy(r.begin(), r.end() - 1, it);
		++it_ps;
	}

	return ConvexHull(coords);
}

template<> TePolygon TeConvexHull(const TePointSet& ps)
{
	vector<TeCoord2D> coords;
	// Copy the first ring of each polygon without the last point (that is equals to the first).
	
	TePointSet::iterator itr = ps.begin();
	
	while(itr != ps.end())
	{
		coords.push_back(itr->location());

		++itr;
	}

	return ConvexHull(coords);
}

//---------- Utilities Functions ----------//



//---------- Validation Functions ----------//

bool TeIsConvex(const TeLinearRing& ring)
{
	bool aux = false;

	short orientation = TeINTERSECTOR2::TeCCW(*(ring.end() - 2), ring[0], ring[1], aux);

	const unsigned int nStep = ring.size() - 1;
	
	for(unsigned int i = 1; i <  nStep; ++i)
	{
		short this_orientation = TeINTERSECTOR2::TeCCW(ring[i-1], ring[i], ring[i+1], aux);
		
		if(this_orientation == TeNOTURN)
			continue;

		if((orientation != TeNOTURN) && (orientation != this_orientation))
			return false;

		orientation = this_orientation;		
	}

	return true;
}

short TeOrientation(const TeLinearRing& r)
{
	double area = Te2Area(r);

	if(area >  0.0)
		return TeCOUNTERCLOCKWISE; 

	if(area < 0.0)
		return TeCLOCKWISE;  

	return TeNOTURN;
}

void TeGetMiddlePoint(const TeCoord2D &first, const TeCoord2D &last, TeCoord2D &middle)
{
	double	lenght,parts,curlenght,incx,incy,
			deltax,deltay,dx,dy;
	short	i,nparts;
	
	lenght = TeDistance(first,last);
	if(lenght == 0.)
	{
		middle = first;
		return;
	}

	nparts = 2;	
	parts = lenght/2.;
// verify segment orientation
	if(first.x() < last.x())
		incx = 1.;
	else
		incx = -1.;

	if(first.y() < last.y())
		incy = 1.;
	else
		incy = -1.;

	curlenght = 0.;
	deltax = fabs(first.x()-last.x());
	deltay = fabs(first.y()-last.y());
	for(i=0;i<(nparts-1);i++)
	{
		curlenght = curlenght + parts;
		// vertical segment
		if(first.x() == last.x())
		{
			middle = TeCoord2D(first.x(),first.y()+(curlenght*incy));
			continue;
		}
				
		// horizontal segment
		if(first.y() == last.y())
		{
			middle = TeCoord2D(first.x()+(curlenght*incx),first.y());
			continue;
		}
		
		// inclined segment
		
		// calculating X coordinate
		dx = curlenght*deltax/lenght;
		
		// calculating Y coordinate
		dy = curlenght*deltay/lenght;
		middle = TeCoord2D(first.x()+(dx*incx),first.y()+(dy*incy));
	}
}

//---------- Metric Functions ----------//

double TeDistance(const TeCoord2D& c1, const TeCoord2D& c2)
{
	return sqrt(((c2.x() - c1.x()) * (c2.x() - c1.x())) + ((c2.y()-c1.y()) * (c2.y() - c1.y())));
}

double TeLength(const TeLine2D& l)
{
	double len = 0.0;

	unsigned int nStep = l.size() - 1;

	for(unsigned int i = 0 ; i < nStep; ++i)
		len += TeDistance(l[i], l[i+1]);
	
	return len;
}

double TePerpendicularDistance(const TeCoord2D& first, const TeCoord2D& last, const TeCoord2D& pin, TeCoord2D &pinter)
{
	double	d12,xmin,ymin;

	double xi = first.x();
	double xf = last.x();
	double yi = first.y();
	double yf = last.y();
	double x = pin.x();
	double y = pin.y();
		
	double dx = xf - xi;
	double dy = yf - yi;
	double a2 = (y - yi) * dx - (x - xi)*dy;
	
    if(dx==0. && dy==0.)
	{
		d12= sqrt(((x - xi) * (x - xi)) + ((y - yi) * (y - yi)));
		d12 *= d12;
	}
	else
		d12= a2 * a2 / (dx * dx + dy * dy);

	if (dx == 0.)
	{
		xmin = xi;
		ymin = y;
	}
	else if (dy == 0.)
	{
		xmin = x;
		ymin = yi;
	}
	else
	{
		double alfa = dy / dx;
		xmin = (x + alfa * (y - yi) + alfa * alfa * xi) / (1. + alfa * alfa);
		ymin = (x - xmin) / alfa + y;	
	}
	
	pinter = TeCoord2D(xmin, ymin);		
	return (sqrt(d12));
}

double TeMinimumDistance (const TeCoord2D& first, const TeCoord2D& last, const TeCoord2D& pin, TeCoord2D& pout, double /*tol*/)
{
	TeCoord2D	pinter;
	TeBox 		sbox(MIN(first.x(),last.x()),
					 MIN(first.y(),last.y()),
					 MAX(first.x(),last.x()),
					 MAX(first.y(),last.y()));

	double d = TePerpendicularDistance (first, last, pin, pinter);
	double dmin = TeMAXFLOAT;

	// Perpendicular minimum distance point was found.
	if (TeIntersects (pinter, sbox))
	{
		dmin = d;
		pout = pinter;
	}
	else  
	{
		// Perpendicular minimum distance point could not be found. 
		// The segment vertices distances will analyzed
		double d1 = TeDistance (first, pin);
		double d2 = TeDistance (last, pin);
		if (d1 <= d2) 
		{
			dmin = d1;
			pout = first;
		}
		else if (d2 < dmin) 
		{
			dmin = d2;
			pout = last;
		}
	}

	return dmin;
}


//---------- Relation Functions ----------//

short TeRelation(const TeCoord2D& c, const TeLine2D& l)
{
	if(TeEquals(c, l[0]) || TeEquals(c, l[l.size() - 1]))
	{
		// Ring doesn't have bundaries, only interiors.
		if(l.isRing())
			return TeINSIDE;

		return TeBOUNDARY;
	}

	if(TeIsOnLine(c, l))
		return TeINSIDE;

	return TeOUTSIDE;
}

short TeRelation(const TeCoord2D& c, const TeLinearRing& r)
{
	if(TeDisjoint(c, r.box()))
		return TeOUTSIDE;

	if(TeIsOnLine(c, r))
		return TeBOUNDARY;

	if(TePointInPoly(c, r))
		return TeINSIDE;
	else
		return TeOUTSIDE;
}

short TeRelation(const TeCoord2D& c, const TePolygon& pol)
{
	short rel = TeRelation(c, pol[0]);
	
	if(rel != TeINSIDE)
		return rel;
	else	// If point is inside exterior ring
	{
		unsigned int size = pol.size();

		for(unsigned int i = 1; i < size; ++i)
		{
			rel = TeRelation(c, pol[i]);
		
			switch(rel)
			{
				// If point is inside a hole so it is on polygon's outside.
				case TeINSIDE    :	return TeOUTSIDE;

				// If point is on boundary so it is on polygon's boundary.
				case TeBOUNDARY  :  return TeBOUNDARY;
			}
		}
	}

	// If the point is inside exterior ring and is not on one of the holes so it is in polygon's interior.
	return TeINSIDE;
}

short TeRelation(const TePoint& p, const TePolygon& pol)
{ 
	return (TeRelation(p.location(), pol)); 
}

short TeRelation(const TeCoord2D& c, const TePolygonSet& pSet)
{
	if(TeDisjoint(c, pSet.box()))
		return TeOUTSIDE;

	unsigned int size = pSet.size();

	for(unsigned int i = 0; i < size; ++i)
	{
		short rel = TeRelation(c, pSet[i]);
		
		if(rel & TeINSIDE)
			return TeINSIDE;
		
		if(rel & TeBOUNDARY)
			return TeBOUNDARY;
	}

	return TeOUTSIDE;
}

short TeRelation(const TeLine2D& lRed, const TeLine2D& lBlue, const short& relation)
{
	TeINTERSECTOR2::TeVectorBoundaryIP report;

	if(TeINTERSECTOR2::TeSafeIntersections(lRed, lBlue, report))
	{
		short rel = Relation(lRed, lBlue, report, relation);		

		// Stop to check, because touches can't occur anymore
		if((relation == TeTOUCHES || relation == TeCROSSES) && rel != relation)
			return TeUNDEFINEDREL;

		if(rel != TeUNDEFINEDREL)
			return rel;

		TeINTERSECTOR2::TeVectorBoundaryIP::iterator it = report.begin();
		TeINTERSECTOR2::TeVectorBoundaryIP::iterator it_end = report.end();
		
		for(unsigned int i = 0; i < report.size(); ++i)
		{
			swap(report[i].redPartNum_, report[i].bluePartNum_);
			swap(report[i].redSegNum_, report[i].blueSegNum_);
		}

		rel = Relation(lBlue, lRed, report, relation);

		if(rel != TeUNDEFINEDREL)
			return ConverseRelation(rel);
		else
			return TeOVERLAPS;
	}
	else
		return TeDISJOINT;
}

short TeRelation(const TeLine2D& line, const TePolygon& pol)
{
	short rel = TopologicRelation(line, pol[0]);

	if(rel & TeTOUCHES || rel & TeCROSSES || rel & TeDISJOINT)
		return rel;

	// If relation is WITHIN or COVERED BY we need to test inner rings.
	register unsigned int i = 1;
	register unsigned int nRings = pol.size();

	for(; i < nRings; ++i)
	{
		short rel_aux = TopologicRelation(line, pol[i]);

		switch(rel_aux)
		{
			case TeCROSSES   : return TeCROSSES;

			case TeCOVEREDBY : return TeTOUCHES;

			case TeTOUCHES   : rel = TeCOVEREDBY;
				               break;	// Needs to check other holes.

			case TeWITHIN    : return TeDISJOINT;

			case TeDISJOINT  : break;	// Keeps rel and check other inner rings
		}
	}

	return rel;
}

short TeRelation(const TePolygon& pRed, const TePolygon& pBlue)
{

	short rel = TopologicRelation(pRed[0], pBlue[0]);

	if(rel & TeTOUCHES || rel & TeOVERLAPS || rel & TeDISJOINT)
		return rel;

	
	short rel_external_ring = TeUNDEFINEDREL;

	bool converse = false;

	vector<TeLinearRing> innerRingsToTest;

	switch(rel)
	{
		case TeWITHIN		: 
		case TeCOVEREDBY	:
	                          // if pBlue has inner rings.
			                  if(pBlue.size() > 1)
							  {
								  rel_external_ring = LookAtInnerRings(pRed[0], pBlue, innerRingsToTest, rel);	
							  }
							  else	// else if it hasn't
							  {
								  if(pRed.size() > 1)	// but pRed has
									  return TeOVERLAPS;	// they overlaps
								  else
									  return rel;
							  }
							  break;

		case TeCONTAINS		:
		case TeCOVERS		: 
							  // if pRed has inner rings.
			                  if(pRed.size() > 1)
							  {
								  rel_external_ring = LookAtInnerRings(pBlue[0], pRed, innerRingsToTest, rel);							      
							  }
							  else	// else if it hasn't
							  {
								  if(pBlue.size() > 1)	// but pBlue has
									  return TeOVERLAPS;	// they overlaps
								  else
									  return rel;
							  }

							  converse = true;
							  break;

		case TeEQUALS		:
			                  if(pRed.size() != pBlue.size())
								  return TeOVERLAPS;

							  if(pRed.size() == 1 && pBlue.size() == 1)
								  return rel;

							  for(unsigned int k = 1; k < pBlue.size(); k++)
								  innerRingsToTest.push_back(pBlue[k]);

							  rel_external_ring = TeUNDEFINEDREL;
	}
	
	if(rel_external_ring != TeUNDEFINEDREL)	
		return rel_external_ring;	

	// If we are here, so we need to test 
	if(converse)	// COVERS or CONTAINS
	{
		if(pBlue.size() == 1 && innerRingsToTest.size() == 0)
			return rel;

		// The result may be: overlap or equals.
		short rel_aux = TestInnerRings(pBlue, innerRingsToTest);

		if(rel_aux & TeOVERLAPS)
			return TeOVERLAPS;

		if(rel_aux & TeCOVERS)
			return TeCOVERS;

		if(rel_aux & TeCONTAINS)
			return TeCONTAINS;
	}
	else	// EQUALS, COVERED BY or WITHIN
	{
		if(pRed.size()  == 1 && innerRingsToTest.size() == 0)
			return rel;

		short rel_aux = TestInnerRings(pRed, innerRingsToTest);

		if(rel_aux & TeOVERLAPS)
			return TeOVERLAPS;

		if((rel_aux == TeEQUALS) && (rel == TeEQUALS))
			return TeEQUALS;

		if((rel_aux & TeCOVERS) || (rel_aux & TeEQUALS))
			return TeCOVEREDBY;	

		if(rel_aux & TeCONTAINS)
		{
			if((rel == TeEQUALS) || (rel == TeCOVEREDBY))
				return TeCOVEREDBY;

			return TeWITHIN;
		}
	}

	return TeUNDEFINEDREL;
}


//---------- Generate Geometry Functions ----------//

TePolygon TeMakePolygon(const TeBox& b)
{
	TeLine2D l;
	
	l.add(b.lowerLeft());
	l.add(TeCoord2D(b.x2(), b.y1()));
	l.add(b.upperRight());
	l.add(TeCoord2D(b.x1(), b.y2()));
	l.add(b.lowerLeft());
	l.setBox(b);

	TePolygon p;
	p.add(TeLinearRing(l));
	p.setBox(b);

	return p;
}

TeLinearRing TeSimpleClosedPath(const TePointSet& pSet)
{
	TeLine2D l;

	for(register unsigned int i = 0; i < pSet.size(); ++i)
		l.add(pSet[i].location());

	ThetaOrder tr(pSet[0].location());

	std::sort(l.begin(), l.end(), tr);

	l.add(l[0]);

	return TeLinearRing(l);
}


//---------- Nearest ----------//

bool TeNearest ( TeCoord2D& pt, TeNodeSet& ns , int& k, const double& tol)
{
	double d,dmin = TeMAXFLOAT;
	bool flag = false;

	k = -1;
	for (unsigned int i=0; i < ns.size() ; i++)
	{
		d = TeDistance (pt, ns[i].location());
		if ( d <= tol && d < dmin)
		{
			dmin = d;
			k = i;
			flag = true;
		}
	}
	return flag;
}

bool TeNearest ( TeCoord2D& pt, TePointSet& ps , int& k, const double& tol)
{
	double d,dmin = TeMAXFLOAT;
	bool flag = false;

	k = -1;
	for (unsigned int i=0; i < ps.size() ; i++)
	{
		d = TeDistance (pt, ps[i].location());
		if ( d <= tol && d < dmin)
		{
			dmin = d;
			k = i;
			flag = true;
		}
	}
	return flag;
}

bool TeNearest ( TeCoord2D& pt, TeTextSet& ts , int& k, const double& tol)
{
	double d,dmin = TeMAXFLOAT;
	bool flag = false;

	k = -1;
	for (unsigned int i=0; i < ts.size() ; i++)
	{
		d = TeDistance (pt, ts[i].location());
		if ( d <= tol && d < dmin)
		{
			dmin = d;
			k = i;
			flag = true;
		}
	}
	return flag;
}

bool TeNearest(TeCoord2D& pt, TeLineSet& ls , int& k, TeCoord2D& pi, const double& tol)
{
	double d,dmin = TeMAXFLOAT;
	bool flag = false;

	k = -1;
	for (unsigned int i=0 ; i<ls.size() ; i++)
	{
		TeLine2D line = ls[i];
		TeBox lb = line.box();
		TeBox box(lb.x1()-tol,lb.y1()-tol,lb.x2()+tol,lb.y2()+tol);
		if (TeIntersects(pt, box))
		{
			for (unsigned int j=0 ; j<line.size()-1 ; j++)
			{
				TeCoord2D	pinter;
				TeBox		sbox(MIN(line[j].x(),line[j+1].x()),
								MIN(line[j].y(),line[j+1].y()),
								MAX(line[j].x(),line[j+1].x()),
								MAX(line[j].y(),line[j+1].y()));
				d = TePerpendicularDistance (line[j],line[j+1],pt, pinter);
				if((d <= tol) && (d < dmin) && TeIntersects(pinter, sbox))
				{
					dmin = d;
					k = i;
					pi = pinter;
					flag = true;
				}
			}
		}
	}
	return flag;
}


bool TeNearest (TeCoord2D& pt,TeLineSet& ls, int& lindex, TeCoord2D& pout, double& dmin, const double& tol)
{
	bool flag = false;
	TeCoord2D	pinter;

	dmin = TeMAXFLOAT;
	lindex = -1;

	for (unsigned int i = 0 ; i < ls.size(); i++)
	{
		TeLine2D line = ls[i];

		for (unsigned int j = 0 ; j < line.size() - 1 ; j++)
		{
			double d = TeMinimumDistance (line[j], line[j + 1], pt, pinter, tol);
			if (d < dmin)
			{
				lindex = i;
				pout = pinter;
				dmin = d;
				flag = true;
			}
		}
	}
	return flag;
}

bool TeNearest(TeCoord2D& pt, TePolygonSet& ps , int& k, const double& /*tol*/)
{
	bool flag = false;

	k = -1;
	
	TePoint ptaux(pt);

	for (unsigned int i=0; i < ps.size() ; i++)
	{
		if (TeIntersects(ptaux,ps[i]))
		{
			k = i;
			flag = true;
		}
	}
	return flag;
}

bool TeNearestByPoints ( TeCoord2D& pt, TeLineSet& ls , int& k, double& dist, const double& tol)
{
	double d,dmin = TeMAXFLOAT;
	bool flag = false;

	k = -1;
	for (unsigned int i=0 ; i<ls.size() ; i++)
	{
		TeLine2D line = ls[i];
		TeBox lb = line.box();
		TeBox box(lb.x1()-tol,lb.y1()-tol,lb.x2()+tol,lb.y2()+tol);
		if (TeIntersects (pt,box))
		{
			for (unsigned int j=0 ; j<line.size()-1 ; j++)
			{
				d = TeDistance (line[j],pt);
				if ( d <= tol && d < dmin )
				{
					dmin = d;
					dist = dmin;
					k = i;
					flag = true;
				}
			}
		}
	}
	return flag;
}


//---------- TIN ----------//

bool TeFindTriangleCenter(const TeCoord2D& vert0, const TeCoord2D& vert1, const TeCoord2D& vert2, TeCoord2D& pc )
{
	if ( vert0 == vert1 || vert0 == vert2 )
		return false; // pontos sao iguais-> retorna

	//	calcula o coeficiente angular perpendicular a reta 1
 	bool perpvert1 = false,	// perpendicular vertical ao segmento 1 
		 perpvert2 = false;	// perpendicular vertical ao segmento 2 
	double	m1 = 10., m2 =10.;	// normais aos segmentos 1 e 2
	double prcsion = 1.0e-10;	// Precision to be used for equal comparison
	double oldPrcsion = TePrecision::instance().precision();
	TePrecision::instance().setPrecision(prcsion);

	if ( TeCoord2D( 0., vert0.y() ) == TeCoord2D( 0., vert1.y() ) )
		perpvert1 = true;
	else 
		m1 = -(vert1.x() - vert0.x()) / (vert1.y() - vert0.y());

//	calcula o coeficiente angular da perpendicular a reta 2
	if ( TeCoord2D( 0., vert1.y() ) == TeCoord2D( 0., vert2.y() ) )
		perpvert2 = true;
	else
		m2 = -(vert2.x() - vert1.x()) / (vert2.y() - vert1.y());

	TePrecision::instance().setPrecision(oldPrcsion);
//	Caso as retas sejam coincidentes, uma circunferencia 
//	 nao eh definida
	if (fabs( m1 - m2 ) > prcsion)
	{

//	passou pelos testes: os pontos definem uma circunferencia
//	calculo do ponto medio do segmento ponto0-ponto1 (segmento 1)
		TeCoord2D ptm1 = vert0;
		ptm1 += vert1;
		ptm1.scale ( 0.5, 0.5);

//	calculo do ponto medio do segmento ponto1-ponto2 (segmento 2)
		TeCoord2D ptm2 = vert1;
		ptm2 += vert2;
		ptm2.scale ( 0.5, 0.5);

//	calculo das coordenadas do centro: ponto de interseccao das mediatrizes
//	 dos segmentos ponto0-ponto1 e ponto1-ponto2
  		if (perpvert1 == true)
		{
   			pc.x( ptm1.x() );
  			pc.y( ptm2.y() + m2 * ( pc.x() - ptm2.x() ) );
 		}
		else if (perpvert2 == true)
		{
   			pc.x( ptm2.x() );
			pc.y( ptm1.y() + m1 * ( pc.x() - ptm1.x() ) );
		}
		else
		{
			pc.x( (m1*ptm1.x() - m2*ptm2.x() - ptm1.y() + ptm2.y())/(m1-m2) );
			pc.y( ptm1.y() + m1 * ( pc.x() - ptm1.x() ) );
		}
		return true;
	}
	return false;
}

bool TeLineSimplify(TeLine2D& ptlist, double snap, double maxdist)
{
//	If snap is zero, don't worry
	if (snap == 0.0)
		return true;
//	If line is too short do nothing
	int npts = ptlist.size();
	if ( npts <= 3 )
		return true;
	int npte = npts;

	double snap2 = maxdist*maxdist;
	TeLine2D vxy;
	vxy.copyElements(ptlist);

//	Check for islands before defining number of points to be used
	int npt;
	if ( ptlist.last() == vxy.first() )
		npt = npte - 1;
	else
		npt = npte;
	ptlist.clear();

//	initialize variables
	int i     = 0;
	int numa  = 0;
	int numpf = npt - 1;

//	define anchor
	TeCoord2D axy = vxy[numa];

//	define floating point
	TeCoord2D pfxy = vxy[numpf];

	while (numa < (npt-1))
	{
		bool retv (false);
		double aa1 = 0.;
		double a = 0.;
		double b = 0.;
//		Compute coeficients of straight line y=ax+b
		if (pfxy.x() == axy.x())
			retv = true;
		else
		{
			a = (pfxy.y() - axy.y())/(pfxy.x() - axy.x());
			b = pfxy.y() - a * pfxy.x();
			aa1	= sqrt(a * a + 1.);
		}

		double d    = 0;
		double dmax = 0;
		int numdmax = numpf;

		int k;
		for (k = numa + 1; k <= numpf; k++)
		{
//			Distance between point and line
			if (retv)
				d = fabs(axy.x() - vxy[k].x());
			else
				d = fabs(vxy[k].y() - a*vxy[k].x() - b)/aa1;

			if (d > dmax)
			{
				dmax	= d;
				numdmax	= k;
			}
		}

		if (dmax <= snap)
		{
//			Store selected point
			if (i > (npt-1))
				return false;
			vxy[i++] = axy;
			
			double axbx = pfxy.x()-axy.x();
			double ayby = pfxy.y()-axy.y();
			double dist2 = axbx*axbx + ayby*ayby;
			if (dist2 > snap2)
			{
				for (k = numpf; k > numa+1; k--)
				{
					axbx = vxy[k].x()-axy.x();
					ayby = vxy[k].y()-axy.y();
					dist2 = axbx*axbx + ayby*ayby;
					if (dist2 < snap2)
						break;
				}
//				Shift anchor
				numa = k;
				axy = vxy[k];
			}
			else
			{
//				Shift anchor
				numa = numpf;
				axy = vxy[numpf];
			}
			numpf = npt - 1;
		}
		else
		{
//			Shift floating point
			numpf = numdmax;
		}

		pfxy = vxy[numpf];
	}

//	Store results
	vxy[i] = vxy[numa];
	npts = i+1;

	if ( vxy[i] == vxy[i-1] )
		npts = i;

	for (i = 0; i < npts; i++)
		ptlist.add( vxy[i] );

//	Case islands
	if (vxy[0] == vxy[npte-1])
		ptlist.add( vxy[0] );

	return true;
}

bool TeSegmentsIntersectPoint(const TeCoord2D& fr0, const TeCoord2D& to0, const TeCoord2D& fr1, const TeCoord2D& to1, TeCoord2D& pi)
{
//	Adapted from TWO-DIMENSIONAL CLIPPING: A VECTOR-BASED APPROACH
//	Hans J.W. Spoelder, Fons H. Ullings in:
//	Graphics Gems I, pp.701, 

	double	a, b, c,
		px, py, lx, ly, lpx, lpy,
		s;

	px  = to0.x() - fr0.x();
	py  = to0.y() - fr0.y();
	lx  = to1.x() - fr1.x();
	ly  = to1.y() - fr1.y();
	lpx = fr0.x() - fr1.x();
	lpy = fr0.y() - fr1.y();

	a = py * lx - px * ly;
	b = lpx * ly - lpy * lx;
	c = lpx * py - lpy * px;

	if (a == 0) // Linhas paralelas
		return false;
	else
	{
		if (a > 0)
		{
			if ((b < 0) || (b > a) || (c < 0) || (c > a))
				return false;
		}
		else
		{
			if ((b > 0) || (b < a) || (c > 0) || (c < a))
				return false;
		}
		s = b/a;
		pi.x(fr0.x() + (px*s));
		pi.y(fr0.y() + (py*s));
	}
	return true;
}

//---------- Curve ----------//

/*! \fn void TeSwap(TePoint &p1, TePoint &p2)
    \brief Swaps to points.
	\param p1 The first point.
	\param p2 The second point.
*/
void TeSwap(TePoint& p1, TePoint& p2)
{
	TePoint temp;

	temp = p1;
	p1 = p2;
	p2 = temp;

	return;
}

bool TeGetCenter(TePoint p1, TePoint p2, TePoint p3, TePoint& center)
{
	double x, y;
	double ma=0., mb= 0.;
	bool result = true;

	//we don't want infinite slopes, or 0 slope for line 1, since we'll divide by "ma" below
	if ((p1.location().x()==p2.location().x()) || (p1.location().y()==p2.location().y())) 
		TeSwap(p2,p3);
	  
	if (p2.location().x()==p3.location().x()) 
		TeSwap(p1,p2);

	if (p1.location().x()!=p2.location().x())
		ma=(p2.location().y()-p1.location().y())/(p2.location().x()-p1.location().x());
	else 
		result=false;
	  
	if (p2.location().x()!=p3.location().x()) 
		mb=(p3.location().y()-p2.location().y())/(p3.location().x()-p2.location().x());
	else 
	  result=false;
	  
	if ((ma==0) && (mb==0)) 
		result=false;
	  
	if (result) 
	{
		x=(ma*mb*(p1.location().y()-p3.location().y())+mb*(p1.location().x()+p2.location().x())-ma*(p2.location().x()+p3.location().x()))/(2*(mb-ma));
		y=-(x-(p1.location().x()+p2.location().x())/2)/ma + (p1.location().y()+p2.location().y())/2;
		
		double w= x;  //TeRound(x);
		double z= y;  //TeRound(y);	
		TeCoord2D coord(w,z);
		center.add(coord);
	}
  return result;
}

double TeGetRadius(TePoint& p1, TePoint& p2, TePoint& p3)
{
	double s,a,b,c, result;
	a=sqrt(pow(p1.location().x()-p2.location().x(),2)+pow(p1.location().y()-p2.location().y(),2));  //sqr: square of the param
	b=sqrt(pow(p2.location().x()-p3.location().x(),2)+pow(p2.location().y()-p3.location().y(),2));
	c=sqrt(pow(p3.location().x()-p1.location().x(),2)+pow(p3.location().y()-p1.location().y(),2));
	s=(a+b+c)/2;
	result=(a*b*c)/(4*sqrt(s*(s-a)*(s-b)*(s-c)));
	return result;
}

bool TeGenerateArc(TePoint& p1, TePoint& p2, TePoint& p3, TeLine2D& arcOut, const short& NPoints)
{	
	TePoint center;
	double radius;

	if(!TeGetCenter(p1, p2, p3, center))
		return false;

	radius = TeGetRadius(p1, p2, p3);

	//calculate the distance between the points p1 and p3
	double length = TeDistance(p1.location(),p3.location());

	double val = length/(2*radius);
	if(val>1)
		return false;

	//calculate the angle (in radians) between the segments (p1 - p3) in the circle center 
	//asin : arco seno
	//http://mathforum.org/dr.math/faq/faq.circle.segment.html#7
	double thetaR = 2 * asin(val);	
	
	//calculate the variation of the angle in radians for each point
	double deltaR = thetaR/(NPoints+1); 

	//verify if is counterclockwise or clockwise
	TeLine2D line;
	line.add(p1.location());
	line.add(p2.location());
	line.add(p3.location());
	line.add(p1.location());
	TeLinearRing ring(line);
	
	short orient = TeOrientation(ring);

	//calculate the coseno e seno of the angle (beta) between the circle center (horizontal segment) and the first point  
	//http://mathforum.org/library/drmath/view/55327.html
	double cosBeta = (p1.location().x()-center.location().x())/radius; 
	double sinBeta = (p1.location().y()-center.location().y())/radius;

	//rela�es trigonom�ricas
	//sin(x+y) = sin(x)cos(y) + cos(x)sin(y),
    //cos(x+y) = cos(x)cos(y) - sin(x)sin(y),
	//sin(x-y) = sin(x)cos(y) - cos(x)sin(y),
	//cos(x-y) = cos(x)cos(y) + sin(x)sin(y),

	arcOut.add(p1.location());
	double angle = deltaR;
	for (int i=0; i<(NPoints+1); i++)
	{
		double x = 0.,y = 0.;
		if(orient==TeCOUNTERCLOCKWISE)
		{
			//c = h + r*cos(B+A),
			//d = k + r*sin(B+A),
			x = center.location().x() + radius * ((cosBeta*cos(angle))-(sinBeta*sin(angle))); //cos(beta+angle) 
			y = center.location().y() + radius * ((sinBeta*cos(angle))+(cosBeta*sin(angle))); //sin(beta+angle)
		}
		else if (orient==TeCLOCKWISE)
		{
			//c = h + r*cos(B-A),
			//d = k + r*sin(B-A),
			x = center.location().x() + radius * ((cosBeta*cos(angle))+(sinBeta*sin(angle))); //cos(beta-angle)
			y = center.location().y() + radius * ((sinBeta*cos(angle))-(cosBeta*sin(angle))); //sin(beta-angle)
		}

		TeCoord2D coord(x,y);
		arcOut.add(coord);
		angle += deltaR;
	}
	arcOut.add(p3.location());
	return true;
}

bool TeGenerateCircle(TePoint& center, const double& radius, TeLine2D& circle, const short& NPoints)
{
	//angle (in radians) of the circumference (2*Pi)
	double thetaR = 2 * 3.14159; 	
	
	//calculate the variation of the angle in radians for each point
	double deltaR = thetaR/(NPoints+1); 

	//calculate a point P1 of the  circumference
	TePoint p1(center.location().x()+radius, center.location().y());
	
	//calculate the coseno e seno of the angle (beta) between the center and the point P1  
	//http://mathforum.org/library/drmath/view/55327.html
	double cosBeta = 1;
	double sinBeta = 0;

	//rela�es trigonom�ricas
	//sin(x+y) = sin(x)cos(y) + cos(x)sin(y),
    //cos(x+y) = cos(x)cos(y) - sin(x)sin(y),
	//sin(x-y) = sin(x)cos(y) - cos(x)sin(y),
	//cos(x-y) = cos(x)cos(y) + sin(x)sin(y),

	circle.add(p1.location());
	double angle = deltaR;
	for (int i=0; i<(NPoints+1); i++)
	{
		double x,y;
		
		//TeCOUNTERCLOCKWISE
		//c = h + r*cos(B+A),
		//d = k + r*sin(B+A),
		
		x = center.location().x() + radius * ((cosBeta*cos(angle))-(sinBeta*sin(angle))); //cos(beta+angle) 
		y = center.location().y() + radius * ((sinBeta*cos(angle))+(cosBeta*sin(angle))); //sin(beta+angle)
		
		TeCoord2D coord(x,y);
		circle.add(coord);
		angle += deltaR;
	}
	circle.add(p1.location());
	return true;
}

double TeGetPrecision(TeProjection* proj)
{
	if(!proj)
		return 0.000000001;

	if(proj->units() == "Meters")
	{
		return 0.001;
	}
	
	return 0.000000001;	// Lat/Long e NoProjection
}


