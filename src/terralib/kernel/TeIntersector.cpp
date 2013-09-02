/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include "TeIntersector.h"
#include "TeRTree.h"
#include "TeGeometryAlgorithms.h"
#include <map>

using namespace std;

short TeINTERSECTOR2::TeCCW(const TeCoord2D& c1, const TeCoord2D& c2, const TeCoord2D& c3, bool& between)
{
	double dx1 = c2.x() - c1.x();
	double dx2 = c3.x() - c1.x();
	double dy1 = c2.y() - c1.y();
	double dy2 = c3.y() - c1.y();

	double dx1y2 = dx1 * dy2;
	double dy1x2 = dy1 * dx2;

	double coef = fabs(dx1y2 - dy1x2);

	if(coef == 0.0)
	{
		// check if c3 is between c1 and c2
		TeBox b;
		b.x1_ = MIN(c1.x(), c2.x());
		b.y1_ = MIN(c1.y(), c2.y());
		b.x2_ = MAX(c1.x(), c2.x());
		b.y2_ = MAX(c1.y(), c2.y());

		if(::TeIntersects(c3, b))
			between = true;

		return TeNOTURN;
	}
	else
	{
		TeCoord2D pinter;
		double dist = TePerpendicularDistance(c1, c2, c3, pinter); 

		if(dist <= TePrecision::instance().precision())
		{
			// check if c3 is between c1 and c2
			TeBox b;
			b.x1_ = MIN(c1.x(), c2.x());
			b.y1_ = MIN(c1.y(), c2.y());
			b.x2_ = MAX(c1.x(), c2.x());
			b.y2_ = MAX(c1.y(), c2.y());

			if(::TeIntersects(c3, b))
				between = true;

			return TeNOTURN;
		}
		else			
		{
			if(dx1y2 > dy1x2)
			{
				return TeCOUNTERCLOCKWISE;
			}
			else
			{
				return TeCLOCKWISE;
			}
		}
	}
}

bool TeINTERSECTOR2::TeIntersection(const TeCoord2D& a, const TeCoord2D& b, const TeCoord2D& c, const TeCoord2D& d, TeBoundaryIP& ips, TeSegmentIntersectionType& intersectionType)
{
	if(TeEquals(a, b))
		return false;

	if(TeEquals(c, d))
		return false;

	double xmin1 = MIN(a.x(), b.x());
	double ymin1 = MIN(a.y(), b.y());
	double xmax1 = MAX(a.x(), b.x());
	double ymax1 = MAX(a.y(), b.y());
	TeBox b1(xmin1, ymin1, xmax1, ymax1);

	double xmin2 = MIN(c.x(), d.x());
	double ymin2 = MIN(c.y(), d.y());
	double xmax2 = MAX(c.x(), d.x());
	double ymax2 = MAX(c.y(), d.y());
	TeBox b2(xmin2, ymin2, xmax2, ymax2);

	if(::TeIntersects(b1, b2))
	{
		if((TeEquals(a, c) || TeEquals(a, d)) && (TeEquals(b, c) || TeEquals(b, d)))
		{
			intersectionType = TeImproperIntersection;

			ips.coords_.push_back(c);
			ips.coords_.push_back(d);

			return true;
		}

		bool between1 = false;
		short sign1 = TeINTERSECTOR2::TeCCW(a, b, c, between1);

		bool between2 = false;
		short sign2 = TeINTERSECTOR2::TeCCW(a, b, d, between2);

		bool between3 = false;
		short sign3 = TeINTERSECTOR2::TeCCW(c, d, a, between3);

		bool between4 = false;
		short sign4 = TeINTERSECTOR2::TeCCW(c, d, b, between4);

		if(((sign1 * sign2) <= 0) && ((sign3 * sign4) <= 0))	// if there is an intersection
		{
			intersectionType = TeProperIntersection;

			if(between1)
			{			
				intersectionType = TeImproperIntersection;
				ips.coords_.push_back(c);
			}
			
			if(between2)
			{
				intersectionType = TeImproperIntersection;
				ips.coords_.push_back(d);
			}

			if(between3 && !TeEquals(a, c) && !TeEquals(a, d))
			{
				intersectionType = TeImproperIntersection;

				if(ips.coords_.size() == 2)
					return true;

				if(ips.coords_.size() == 1)
				{
					if(TeEquals(ips.coords_[0], a))
						return true;
				}

				ips.coords_.push_back(a);
			}
			
			if(between4 && !TeEquals(b, c) && !TeEquals(b, d))
			{
				intersectionType = TeImproperIntersection;

				if(ips.coords_.size() == 2)
					return true;

				if(ips.coords_.size() == 1)
				{
					if(TeEquals(ips.coords_[0], b))
						return true;
				}

				ips.coords_.push_back(b);
			}

			if(intersectionType == TeImproperIntersection)
				return true;

			double denominator = (d.y() - c.y()) * (b.x() - a.x()) - (d.x() - c.x()) * (b.y() - a.y());
			
			if(denominator == 0.0)	// parallel can not occur here any more! I expect this is true!
				return false;		

			// parameters
			double Ua = ((d.x() - c.x()) * (a.y() - c.y()) - (d.y() - c.y()) * (a.x() - c.x())) / denominator; 
			//double Ub = ((b.x() - a.x()) * (a.y() - c.y()) - (b.y() - a.y()) * (a.x() - c.x())) / denominator; 

			TeCoord2D caux(a.x() + Ua * (b.x() - a.x()), a.y() + Ua * (b.y() - a.y()));

			if(TeEquals(caux, a))
			{
				ips.coords_.push_back(a);
				return true;
			}

			if(TeEquals(caux, b))
			{
				ips.coords_.push_back(b);
				return true;
			}

			if(TeEquals(caux, c))
			{
				ips.coords_.push_back(c);
				return true;
			}

			if(TeEquals(caux, d))
			{
				ips.coords_.push_back(d);
				return true;
			}

			ips.coords_.push_back(caux);
			
			return true;
		}
	}

	intersectionType = TeImproperIntersection;

	return false;
}

bool TeINTERSECTOR2::TeIntersects(const TeCoord2D& a, const TeCoord2D& b, const TeCoord2D& c, const TeCoord2D& d, TeSegmentIntersectionType& intersectionType)
{	
	if(TeEquals(a, b))
		return false;

	if(TeEquals(c, d))
		return false;

	double xmin1 = MIN(a.x(), b.x());
	double ymin1 = MIN(a.y(), b.y());
	double xmax1 = MAX(a.x(), b.x());
	double ymax1 = MAX(a.y(), b.y());
	TeBox b1(xmin1, ymin1, xmax1, ymax1);

	double xmin2 = MIN(c.x(), d.x());
	double ymin2 = MIN(c.y(), d.y());
	double xmax2 = MAX(c.x(), d.x());
	double ymax2 = MAX(c.y(), d.y());
	TeBox b2(xmin2, ymin2, xmax2, ymax2);

	if(::TeIntersects(b1, b2))
	{
		if((TeEquals(a, c) || TeEquals(a, d)) && (TeEquals(b, c) || TeEquals(b, d)))
		{
			intersectionType = TeImproperIntersection;

			return true;
		}

		bool between1 = false;
		short sign1 = TeINTERSECTOR2::TeCCW(a, b, c, between1);

		bool between2 = false;
		short sign2 = TeINTERSECTOR2::TeCCW(a, b, d, between2);

		bool between3 = false;
		short sign3 = TeINTERSECTOR2::TeCCW(c, d, a, between3);

		bool between4 = false;
		short sign4 = TeINTERSECTOR2::TeCCW(c, d, b, between4);

		if((sign1 * sign2) <= 0 && (sign3 * sign4 <= 0))	// if there is an intersection
		{
			intersectionType = TeProperIntersection;

			if(between1 || between2 || between3 || between4)
			{			
				intersectionType = TeImproperIntersection;
			}			

			return true;
		}
	}
		
	return false;
}

bool TeINTERSECTOR2::TeIntersects(const TeLine2D& redLine, const TeLine2D& blueLine)
{
	if(redLine.size() < 2 || blueLine.size() < 2) 
		return false; 

	if(TeDisjoint(redLine.box(), blueLine.box()))
		return false;

	unsigned int i = 0;
	unsigned int j = 0;

	TeBox interBox;
	
	// Creates a intersection box from lines boxes.
	::TeIntersection(redLine.box(), blueLine.box(), interBox);

	unsigned int nstep_redLine  = redLine.size() - 1;
	unsigned int nstep_blueLine = blueLine.size() - 1;

	TeSegmentIntersectionType t = TeImproperIntersection;

	for(i = 0; i < nstep_redLine; ++i)
	{
		// Creates the segment box.
		TeBox red_box = makeBox(redLine[i].x(), redLine[i].y(), redLine[i+1].x(), redLine[i+1].y());

		// See if red segment box intersects with the intersection box.
		if(TeDisjoint(interBox, red_box))
			continue;	// If it doesn't intersect, go to another segment => skip comparasion beteween other "m" segments.

		for(j = 0; j < nstep_blueLine; ++j)
		{
			// Check intersection.
			if(TeINTERSECTOR2::TeIntersects(redLine[i], redLine[i+1], blueLine[j], blueLine[j+1], t))
				return true;
		}
	}	

	return false;
}

bool TeINTERSECTOR2::TeSafeIntersections(const TeLine2D& redLine, const TeLine2D& blueLine, TeVectorBoundaryIP& report, const unsigned int& redObjId, const unsigned int& blueObjId)
{
	if(redLine.size() < 2 || blueLine.size() < 2) 
		return false; 

	if(TeDisjoint(redLine.box(), blueLine.box()))
		return false;

	unsigned int i = 0;
	unsigned int j = 0;

	TeBox interBox;
	
	bool hasIntersections = false;

	// Creates a intersection box from lines boxes.
	::TeIntersection(redLine.box(), blueLine.box(), interBox);

	unsigned int nstep_redLine  = redLine.size() - 1;
	unsigned int nstep_blueLine = blueLine.size() - 1;

	TeSegmentIntersectionType t = TeImproperIntersection;

	for(i = 0; i < nstep_redLine; ++i)
	{
		// Creates the segment box.
		TeBox red_box = makeBox(redLine[i].x_, redLine[i].y_, redLine[i+1].x_, redLine[i+1].y_);

		// See if red segment box intersects with the intersection box.
		if(TeDisjoint(interBox, red_box))
			continue;	// If it doesn't intersect, go to another segment => skip comparasion beteween other "m" segments.

		for(j = 0; j < nstep_blueLine; ++j)
		{
			// Check intersection.
			TeBoundaryIP ip;

			if(TeIntersection(redLine[i], redLine[i+1], blueLine[j], blueLine[j+1], ip, t))
			{
				hasIntersections = true;

				ip.redPartNum_ = redObjId;
				ip.redSegNum_ = i;
				ip.bluePartNum_ = blueObjId;
				ip.blueSegNum_ = j;

				if(ip.coords_.size() == 2)	//overlap
				{
					// Verificar se os pontos estão em ordem crescente
					if(ip.coords_[0].x_ < ip.coords_[1].x_)
					{
						report.push_back(ip);
					}
					else if(ip.coords_[0].x_ > ip.coords_[1].x_)
					{
						swap(ip.coords_[0], ip.coords_[1]);
						report.push_back(ip);
					}
					else if(ip.coords_[0].y_ < ip.coords_[1].y_)
					{
						report.push_back(ip);						
					}
					else
					{
						swap(ip.coords_[0], ip.coords_[1]);
						
						report.push_back(ip);						
					}

				}
				else
				{
					report.push_back(ip);
				}
			}
		}
	}	

	return hasIntersections;
}

bool TeINTERSECTOR2::TeSafeIntersections(const TePolygonSet& redPols, const TePolygonSet& bluePols, TeVectorBoundaryIP& report)
{
	unsigned int redPart = 0;
	unsigned int redPolsSize = redPols.size();

	unsigned int i, k;
	register unsigned int j, l;

	// Loops through red polygons
	for(i = 0; i < redPolsSize; ++i)
	{
		TePolygon redPol = redPols[i];
		unsigned int redPolSize = redPol.size();

		// Loops through red polygons rings
		for(j = 0; j < redPolSize; ++j)
		{
			TeLinearRing redRing = redPol[j];			

			// Loops through blue polygons
			unsigned int bluePart = 0;
			unsigned int bluePolsSize =  bluePols.size();

			for(k = 0; k < bluePolsSize; ++k)
			{
				// Loops through blue polygons rings
				TePolygon bluePol = bluePols[k];

				unsigned int bluePolSize = bluePol.size();

				for(l = 0; l < bluePolSize; ++l)
				{
					TeLinearRing blueRing = bluePol[l];

					TeSafeIntersections(redRing, blueRing, report, redPart, bluePart);

					++bluePart;
				}
			}

			++redPart;
		}
	}

	return !report.empty();
}

// Put polygon set segments into the index
void TeINTERSECTOR2::TeIndexPolygonSet(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree)
{
	unsigned int nPols = polygons.size();	
	unsigned int i, j, k;

	for(i = 0; i < nPols; ++i)
	{
		unsigned int nRings = polygons[i].size();

		for(j = 0; j < nRings; ++j)
		{
			unsigned int nSegs = polygons[i][j].size() - 1;

			for(k = 0; k < nSegs; ++k)
			{
				TeBox b;

				b.x1_ = MIN(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y1_ = MIN(polygons[i][j][k].y(), polygons[i][j][k + 1].y());
				b.x2_ = MAX(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y2_ = MAX(polygons[i][j][k].y(), polygons[i][j][k + 1].y());

				TeINTERSECTOR2::TeSegIdInPolygonSet sid;

				sid.polId_  = i;
				sid.lineId_   = j;
				sid.segId_ = k;

				tree.insert(b, sid);
			}
		}
	}

	return;
}

// Estrutura auxiliar para indexar cada um dos pontos de intersecao em um multimap
struct TeIntersectionIndexKey
{
	unsigned int segNum_;
	unsigned int partNum_;
	unsigned int polNum_;

	TeIntersectionIndexKey()
		: segNum_(0), partNum_(0), polNum_(0)
	{
	}

	TeIntersectionIndexKey(const unsigned int& segNum, const unsigned int& partNum, const unsigned int& polNum)
		: segNum_(segNum), partNum_(partNum), polNum_(polNum)
	{
	}

	bool operator<(const TeIntersectionIndexKey& rhs) const
	{
		if(polNum_ < rhs.polNum_)
			return true;

		if(polNum_ > rhs.polNum_)
			return false;

		if(partNum_ < rhs.partNum_)
			return true;

		if(partNum_ > rhs.partNum_)
			return false;

		if(segNum_ < rhs.segNum_)
			return true;

		if(segNum_ > rhs.segNum_)
			return false;

		return false;
	}
};


typedef multimap<TeIntersectionIndexKey, unsigned int> TeIntersectionIndex;

// Dado um ponto de intersecao, faz a normalizacao do mesmo: procura um ponto correlato na lista de intersecao e tenta usar as mesmas coordenadas
void TeNormalizeIntersection(TeINTERSECTOR2::TeBoundaryIP& ip, TeINTERSECTOR2::TeVectorBoundaryIP& report, TeIntersectionIndex& interIndex)
{
	TeIntersectionIndexKey key(ip.blueSegNum_, ip.bluePartNum_, ip.bluePolNum_);

	pair<TeIntersectionIndex::iterator, TeIntersectionIndex::iterator> its = interIndex.equal_range(key);

//	bool eraseOwn = false;

	while(its.first != its.second)
	{
// se achamos um indice, vamos verificar se ele possui um ponto de intersecao que seja igual ao que temos em maos, segundo a tolerancia
		unsigned int position = its.first->second;

		if(TeEquals(report[position].coords_[0], ip.coords_[0]))
		{
			ip.coords_[0] = report[position].coords_[0];
		}

		if(ip.coords_.size() == 2 && report[position].coords_.size() == 1)
		{
			if(TeEquals(report[position].coords_[0], ip.coords_[1]))
				ip.coords_[1] = report[position].coords_[0];			
		}

		if(ip.coords_.size() == 1 && report[position].coords_.size() == 2)
		{
			if(TeEquals(report[position].coords_[1], ip.coords_[0]))
				ip.coords_[0] = report[position].coords_[1];			

		}

		if(ip.coords_.size() == 2 && report[position].coords_.size() == 2)

		{
			if(TeEquals(report[position].coords_[1], ip.coords_[1]))
				ip.coords_[1] = report[position].coords_[1];

			if(TeEquals(report[position].coords_[0], ip.coords_[1]))
				ip.coords_[1] = report[position].coords_[0];


			if(TeEquals(report[position].coords_[1], ip.coords_[0]))
				ip.coords_[0] = report[position].coords_[1];
		}

		++its.first;
	}
}

// insere a informacao da intersecao no indice
void TeIndexIntersection(TeINTERSECTOR2::TeBoundaryIP& ip, const unsigned int& redPos, const unsigned int& bluePos, TeIntersectionIndex& interIndex)
{
	TeIntersectionIndexKey redkey(ip.redSegNum_, ip.redPartNum_, ip.redPolNum_);
	TeIntersectionIndexKey bluekey(ip.blueSegNum_, ip.bluePartNum_, ip.bluePolNum_);

	interIndex.insert(pair<TeIntersectionIndexKey, unsigned int>(redkey, redPos));
	interIndex.insert(pair<TeIntersectionIndexKey, unsigned int>(bluekey, bluePos));
}

// insere o ponto de intersecao na lista
void TeInsertIntersection(TeINTERSECTOR2::TeBoundaryIP& ip, TeINTERSECTOR2::TeVectorBoundaryIP& report)
{
	if(ip.coords_.size() == 2)	//overlap
	{
// put intersections in increase order of 'x'
		if(ip.coords_[0].x_ < ip.coords_[1].x_)
		{
			report.push_back(ip);
		}
		else if(ip.coords_[0].x_ > ip.coords_[1].x_)
		{
			swap(ip.coords_[0], ip.coords_[1]);
			report.push_back(ip);			
		}
		else if(ip.coords_[0].y_ < ip.coords_[1].y_)
		{
			report.push_back(ip);						
		}
		else
		{
			swap(ip.coords_[0], ip.coords_[1]);							
			report.push_back(ip);
		}
	}
	else
	{
		report.push_back(ip);
	}
}

// Find intersection into two distinct sets: red and blue
bool TeINTERSECTOR2::TeIntersection(const TePolygonSet& redPolygons, TeINTERSECTOR2::TeSegmentRTree& redTree, const TePolygonSet& bluePolygons, TeINTERSECTOR2::TeVectorBoundaryIP& report)
{
// if index is empty, so we index first
	if(redTree.isEmpty())
		TeINTERSECTOR2::TeIndexPolygonSet(redPolygons, redTree);

	TeIntersectionIndex interIndex;

	unsigned int nPols = bluePolygons.size();	

	TeSegmentIntersectionType t = TeImproperIntersection;

	unsigned int i, j, k, l;
	for(i = 0; i < nPols; ++i)
	{
		unsigned int nRings = bluePolygons[i].size();

		for(j = 0; j < nRings; ++j)
		{
			unsigned int nSegs = bluePolygons[i][j].size() - 1;

// for each blue segment, find the one's that can intersects it into the red index tree
			for(k = 0; k < nSegs; ++k)
			{
				TeBox b;

				b.x1_ = MIN(bluePolygons[i][j][k].x(), bluePolygons[i][j][k + 1].x());
				b.y1_ = MIN(bluePolygons[i][j][k].y(), bluePolygons[i][j][k + 1].y());
				b.x2_ = MAX(bluePolygons[i][j][k].x(), bluePolygons[i][j][k + 1].x());
				b.y2_ = MAX(bluePolygons[i][j][k].y(), bluePolygons[i][j][k + 1].y());

				vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

				redTree.search(b, segs);

				unsigned int nSegsInter = segs.size();

// depois tentamos achar os pontos de intersecao
				for(l = 0; l < nSegsInter; ++l)
				{
					TeINTERSECTOR2::TeBoundaryIP ip1;

					if(TeINTERSECTOR2::TeIntersection(bluePolygons[i][j][k], bluePolygons[i][j][k + 1], redPolygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_], redPolygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_ + 1], ip1, t))
					{
						ip1.redPolNum_ = segs[l].polId_;
						ip1.redPartNum_ = segs[l].lineId_;
						ip1.redSegNum_ = segs[l].segId_;

						ip1.bluePolNum_ = i;
						ip1.bluePartNum_ = j;
						ip1.blueSegNum_ = k;

						// normaliza a intersecao: se achar um outro pontos na lista de intersecao semelhante, usaremos o semelhante!
						TeNormalizeIntersection(ip1, report, interIndex);


						TeINTERSECTOR2::TeBoundaryIP ip2;	// objeto auxiliar
						ip2.redPolNum_ = i;
						ip2.redPartNum_ = j;
						ip2.redSegNum_ = k;

						ip2.bluePolNum_ = segs[l].polId_;
						ip2.bluePartNum_ = segs[l].lineId_;
						ip2.blueSegNum_ = segs[l].segId_;

						ip2.coords_ = ip1.coords_;

						TeNormalizeIntersection(ip2, report, interIndex);

						ip1.coords_ = ip2.coords_;

						TeInsertIntersection(ip1, report);						

						TeIndexIntersection(ip1, report.size() - 1, report.size() - 1, interIndex);
					}
				}
			}			
		}
	}

	return !report.empty();
}

bool TeINTERSECTOR2::TeIsSimple(const TePolygonSet& polygons, vector<TeINTERSECTOR2::TePairSegIdInPolygonSet>& selfIntersectionList)
{
	bool hasSelfIntersection = false;

	TeSegmentIntersectionType t = TeImproperIntersection;

// create index tree
	TeINTERSECTOR2::TeSegmentRTree tree(polygons.box());

// loop through segments
	unsigned int nPols = polygons.size();	

	unsigned int i, j, k, l;

	for(i = 0; i < nPols; ++i)
	{
		unsigned int nRings = polygons[i].size();

		for(j = 0; j < nRings; ++j)
		{
			unsigned int nSegs = polygons[i][j].size() - 1;

			for(k = 0; k < nSegs; ++k)
			{
				TeBox b;

				b.x1_ = MIN(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y1_ = MIN(polygons[i][j][k].y(), polygons[i][j][k + 1].y());
				b.x2_ = MAX(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y2_ = MAX(polygons[i][j][k].y(), polygons[i][j][k + 1].y());

				TeINTERSECTOR2::TeSegIdInPolygonSet sid;

				sid.polId_  = i;
				sid.lineId_   = j;
				sid.segId_ = k;

// try to find filter segments that boxe intersects this segment
				vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;
	
				tree.search(b, segs);

				unsigned int nSegsInter = segs.size();

// if found, loops through segments to see if there are intersections
				for(l = 0; l < nSegsInter; ++l)
				{
					TeINTERSECTOR2::TeBoundaryIP ip;

					if(TeIntersection(polygons[i][j][k], polygons[i][j][k + 1], polygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_], polygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_ + 1], ip, t))
					{
// if there is intersection, we have to cases:
						if(ip.coords_.size() == 2)
						{
// first, the segment overlaps another: so this configures a selfintersection
							hasSelfIntersection = true;

							selfIntersectionList.push_back(TePairSegIdInPolygonSet(sid, segs[l]));
						}
						else
						{
// second, the segment intersects in one point: so this may be a selft-intersection or may be a intersection between the posterior segment (already inserted) in this second, no worry it is right
							if((sid.polId_ != segs[l].polId_) || (sid.lineId_ != segs[l].lineId_))
							{
								hasSelfIntersection = true;

								selfIntersectionList.push_back(TePairSegIdInPolygonSet(sid, segs[l]));
							}
							else
							{
// we must look if they are consecutives (remeber last an firt will have intersections) 
								int auxSegId = (int)sid.segId_;
								int otherSegId = (int)(segs[l].segId_);

// if it intersects the previous or the next, it's ok!
								if((auxSegId == (otherSegId + 1)) || (auxSegId == (otherSegId -1)))
								{
									continue;
								}
								else
								{
// we need to see if it is not between last and first
									if(auxSegId == (int)(polygons[sid.polId_][sid.lineId_].size() - 2))
									{
// ok, it is a consecutive intersection between last and first
										if(otherSegId == 0)
											continue;
									}
									else if(auxSegId == 0)
									{
// ok, it is a consecutive intersection between first and last
										if(otherSegId == (int)(polygons[segs[l].polId_][segs[l].lineId_].size() - 2))
											continue;
									}

									hasSelfIntersection = true;

									selfIntersectionList.push_back(TePairSegIdInPolygonSet(sid, segs[l]));
								}
							}
						}

					}
				}

// inserts the segment into the tree
				tree.insert(b, sid);
			}
		}
	}

	tree.clear();

	return !hasSelfIntersection;
}

// Find intersection into two distinct sets: red lines and blue polygons
bool TeINTERSECTOR2::TeIntersection(const TeLineSet& redLines, const TePolygonSet& bluePolygons, TeSegmentRTree& blueTree, TeINTERSECTOR2::TeVectorBoundaryIP& report)
{
// if index is empty, so we index the segments from polygonset first
	if(blueTree.isEmpty())
		TeINTERSECTOR2::TeIndexPolygonSet(bluePolygons, blueTree);

// for each lines we must compute intersections
	unsigned int nLines = redLines.size();	

	TeSegmentIntersectionType t = TeImproperIntersection;

	unsigned int i, j, l;

	for(i = 0; i < nLines; ++i)
	{
		unsigned int nSegs = redLines[i].size() - 1;

		for(j = 0; j < nSegs; ++j)
		{
// for each segment we compute intersections
			TeBox b;

			b.x1_ = MIN(redLines[i][j].x(), redLines[i][j + 1].x());
			b.y1_ = MIN(redLines[i][j].y(), redLines[i][j + 1].y());
			b.x2_ = MAX(redLines[i][j].x(), redLines[i][j + 1].x());
			b.y2_ = MAX(redLines[i][j].y(), redLines[i][j + 1].y());

			vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

			blueTree.search(b, segs);

			unsigned int nSegsInter = segs.size();

// depois tentamos achar os pontos de intersecao
			for(l = 0; l < nSegsInter; ++l)
			{
				TeINTERSECTOR2::TeBoundaryIP ip1;

				if(TeINTERSECTOR2::TeIntersection(redLines[i][j], redLines[i][j + 1], bluePolygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_], bluePolygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_ + 1], ip1, t))
				{
					ip1.redPolNum_ = 0;
					ip1.redPartNum_ = i;
					ip1.redSegNum_ = j;

					ip1.bluePolNum_ = segs[l].polId_;
					ip1.bluePartNum_ = segs[l].lineId_;
					ip1.blueSegNum_ = segs[l].segId_;

					if(ip1.coords_.size() == 2)	//overlap
					{
						// Verificar se os pontos estao em ordem crescente
						if(ip1.coords_[0].x_ < ip1.coords_[1].x_)
						{
							report.push_back(ip1);
						}
						else if(ip1.coords_[0].x_ > ip1.coords_[1].x_)
						{
							swap(ip1.coords_[0], ip1.coords_[1]);
							report.push_back(ip1);
						}
						else if(ip1.coords_[0].y_ < ip1.coords_[1].y_)
						{
							report.push_back(ip1);						
						}
						else
						{
							swap(ip1.coords_[0], ip1.coords_[1]);							
							report.push_back(ip1);						
						}
					}
					else
					{
						report.push_back(ip1);
					}
				}
			}
		}
	}

	return !report.empty();
}

// Find intersection between segments into polygonset list
bool TeINTERSECTOR2::TeIntersection(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree, TeINTERSECTOR2::TeVectorBoundaryIP& report)
{
// clear tree before start process
	tree.clear();

	TeIntersectionIndex interIndex;

	unsigned int nPols = polygons.size();	

	if(nPols == 0)
		return false;

	unsigned int nLines = polygons[0].size();	

	unsigned int i, j, k, l;

// the first polygon's segments are all indexed into the tree
	for(i = 0; i < nLines; ++i)
	{
		unsigned int nSegs = polygons[0][i].size() - 1;

		for(j = 0; j < nSegs; ++j)
		{
			TeBox b;

			b.x1_ = MIN(polygons[0][i][j].x(), polygons[0][i][j + 1].x());
			b.y1_ = MIN(polygons[0][i][j].y(), polygons[0][i][j + 1].y());
			b.x2_ = MAX(polygons[0][i][j].x(), polygons[0][i][j + 1].x());
			b.y2_ = MAX(polygons[0][i][j].y(), polygons[0][i][j + 1].y());

			TeINTERSECTOR2::TeSegIdInPolygonSet sid;

			sid.polId_  = 0;
			sid.lineId_   = i;
			sid.segId_ = j;

			tree.insert(b, sid);
		}
	}
	
	TeSegmentIntersectionType t = TeImproperIntersection;

// for each polygon, we first test intersection of all segments  and then put them into the index
	for(i = 1; i < nPols; ++i)
	{
		unsigned int nLines = polygons[i].size();	

// for each line of the polygon, verifies intersection against the segments into the index
		for(j = 0; j < nLines; ++j)
		{
			unsigned int nSegs = polygons[i][j].size() - 1;

			for(k = 0; k < nSegs; ++k)
			{
// for each segment we find the one's that intersects its bounding box
				TeBox b;

				b.x1_ = MIN(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y1_ = MIN(polygons[i][j][k].y(), polygons[i][j][k + 1].y());
				b.x2_ = MAX(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y2_ = MAX(polygons[i][j][k].y(), polygons[i][j][k + 1].y());

				vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

				tree.search(b, segs);

				unsigned int nSegsInter = segs.size();

// if there are candidates, try to find intersections
				for(l = 0; l < nSegsInter; ++l)
				{
					TeINTERSECTOR2::TeBoundaryIP ip1;

					TeINTERSECTOR2::TeBoundaryIP ip2;

					if(TeINTERSECTOR2::TeIntersection(polygons[i][j][k], polygons[i][j][k + 1], polygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_], polygons[segs[l].polId_][segs[l].lineId_][segs[l].segId_ + 1], ip1, t))
					{
// if the segment intersects a candidate, stores theirs intersection points
						ip1.redPolNum_  = i;
						ip1.redPartNum_ = j;
						ip1.redSegNum_  = k;
						ip1.bluePolNum_ = segs[l].polId_;
						ip1.bluePartNum_ = segs[l].lineId_;
						ip1.blueSegNum_ = segs[l].segId_;

						// normaliza a intersecao: se achar um outro pontos na lista de intersecao semelhante, usaremos o semelhante!
						TeNormalizeIntersection(ip1, report, interIndex);						

						ip2.redPolNum_ = segs[l].polId_;
						ip2.redPartNum_ = segs[l].lineId_;
						ip2.redSegNum_ = segs[l].segId_;

						ip2.bluePolNum_ = i;
						ip2.bluePartNum_ = j;
						ip2.blueSegNum_ = k;

						ip2.coords_ = ip1.coords_;

						TeNormalizeIntersection(ip2, report, interIndex);

						ip1.coords_ = ip2.coords_;

// insere os pontos de intersecao na lista de intersecao e no multi-map
						TeInsertIntersection(ip1, report);						
						TeInsertIntersection(ip2, report);

						TeIndexIntersection(ip1, report.size() - 2, report.size() - 1, interIndex);	
					}
				}
			}
		}
	
// after the search for intersection, put segfments into index
		for(j = 0; j < nLines; ++j)
		{
			unsigned int nSegs = polygons[i][j].size() - 1;

			for(k = 0; k < nSegs; ++k)
			{
				TeBox b;

				b.x1_ = MIN(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y1_ = MIN(polygons[i][j][k].y(), polygons[i][j][k + 1].y());
				b.x2_ = MAX(polygons[i][j][k].x(), polygons[i][j][k + 1].x());
				b.y2_ = MAX(polygons[i][j][k].y(), polygons[i][j][k + 1].y());

				TeSegIdInPolygonSet sid;

				sid.polId_ = i;
				sid.lineId_ = j;
				sid.segId_ = k;

				tree.insert(b, sid);
			}
		}
	}

	return !report.empty();
}

