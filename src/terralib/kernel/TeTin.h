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
/*! \file TeTin.h
    \brief This file provides support for a TIN structure
*/
#ifndef  __TERRALIB_INTERNAL_TIN_H
#define  __TERRALIB_INTERNAL_TIN_H

#include "TeGeometry.h"
#include <list>
#include <set>

/** @} */ 
 
/** @defgroup MathConst Mathematical constants and macro definitions for use in Tin
  @{
 */
const double	TeTinBIGFLOAT = TeMAXFLOAT / 10.;		//!< Near Maximum float value - TeMAXFLOAT / 10.

//!  Tin Vertex types 
enum TeTinVertexType 
{ NORMALVERTEX = 0, ONCONTOURVERTEX = 1, ONBREAKLINEVERTEX = 0x10 };

//!  A basic class for handling vertex of TIN
/*!
	\sa TeGeometry TeMeasure
*/
class TL_DLL TeTinVertex : public TeGeomSingle<TeCoord2D>, public TeMeasure
{
public:

	//! Empty constructor
	TeTinVertex() : TeMeasure ( TeMAXFLOAT ), oneEdge_ (-1) {}

	//! Constructor with parameters
	TeTinVertex( TeCoord2D xy, double value, TeTinVertexType type,
		long edge ) :
		TeGeomSingle<TeCoord2D> (xy), TeMeasure ( value ),
		 type_ (type), oneEdge_ ( edge )
		{ if ( ! xy.tooBig() ) TeGeometry::setBox(TeBox(xy,xy)); }

// -- Methods
//
	//! Returns TRUE if a TeTinVertex is equal to other
	bool operator== (const TeTinVertex& other) const
	{
		if((value_ == other.value_) && (type_ == other.type_) )
		{
			TeCoord2D p1 = location();
			TeCoord2D p2 = other.location();
			return p1==p2;
		}
		return false;
	}

	long oneEdge () 
	{ return oneEdge_; }

	void setOneEdge (long oneEdge) 
	{ oneEdge_ = oneEdge; }

	TeTinVertexType type () 
	{ return type_; }

	void setType (TeTinVertexType type) 
	{ type_ = type; }

	void value (double value) 
	{ value_ = value; }

	double value () 
	{ return value_; }

	TeGeomRep elemType() { return TeSAMPLES; }

private:

	TeTinVertexType type_;
	long oneEdge_;
};

//!  A basic class for handling a std::set of TeTinVertex
class TL_DLL TeTinVertexSet : public TeGeomComposite <TeTinVertex>
{
public:

// -- Constructors

	TeTinVertexSet() {}

// -- Methods

	bool loadFromFile (std::string &fileName);

	TeGeomRep elemType() { return TeSAMPLES; }
};

//! Tin Edges type 
enum TeTinEdgeType 
{ NORMALEDGE, ONCONTOUREDGE, ONBREAKLINEEDGE };

//!  A basic class for handling edges of a  TIN
/*!
	\sa TeGeometry
*/
class TL_DLL TeTinEdge : public TeGeometry
{
public:

// -- Constructors

	TeTinEdge() {}
	TeTinEdge(long from, long to, long left, long right,
		TeTinEdgeType type) :
	from_ (from), to_ (to), left_ (left), right_ (right), type_ (type)
	{}

// -- Methods

	//! Returns TRUE if a TeTinEdge is equal to other
	bool operator== (const TeTinEdge& other) const
	{
		if( (to_ == other.to_) && (from_ == other.from_) &&
			(left_ == other.left_) && (right_ == other.right_) &&
			(type_ == other.type_) )
			return true;

		return false;
	}

	long from () 
	{ return from_; }

	long to () 
	{ return to_; }

	void setVertices (long from, long to)
	{ 
		from_ = from;
		to_ = to;
	}

	void swapVertices ()
	{ 
		long aux = from_;
		from_ = to_;
		to_ = aux;
	}

	long left () 
	{ return left_; }

	long right ()
	{ return right_; }

	void setTriangles (long left, long right)
	{ 
		left_ = left;
		right_ = right;
	}

	void swapTriangles ()
	{ 
		long aux = left_;
		left_ = right_;
		right_ = aux;
	}
	
	void exchangeTriangle ( long t1, long t2 )
	{
		if (left_ == t1 )
			left_ = t2;
		else
			right_ = t2;
	}

	void exchangeVertex ( long v1, long v2 )
	{
		if (from_ == v1 )
			from_ = v2;
		else
			to_ = v2;
	}

	TeTinEdgeType type () 
	{ return type_; }

	void setType (TeTinEdgeType type) 
	{ type_ = type; }

	TeGeomRep elemType() { return TeSAMPLES; }

private:

	long	from_, to_;
	long	left_, right_;
	TeTinEdgeType type_;
};

//!  A basic class for handling a std::set of TeTinEdge
class TL_DLL TeTinEdgeSet : public TeGeomComposite<TeTinEdge>
{
public:

// -- Constructors

	TeTinEdgeSet()	{}

// -- Methods

	bool loadFromFile (std::string &fileName);

	TeGeomRep elemType() { return TeSAMPLES; }
};

//! TeTinTriangle: A class for handling a TIN triangle
class TL_DLL TeTinTriangle : public TeGeometry
{
public:

// -- Constructors

	TeTinTriangle()
	{
		triEdges_[0] = -1;
		triEdges_[1] = -1;
		triEdges_[2] = -1;
	}

	TeTinTriangle(long first, long second, long third) 
	{
		triEdges_[0] = first;
		triEdges_[1] = second;
		triEdges_[2] = third;
	}

// -- Methods

	//! Returns TRUE if a TeTinTriangle is equal to other
	bool operator== (const TeTinTriangle& other) const
	{
		if( (triEdges_[0] == other.triEdges_[0]) &&
			(triEdges_[1] == other.triEdges_[1]) &&
			(triEdges_[2] == other.triEdges_[2]) )
			return true;

		return false;
	}

	void setEdges (long first, long second, long third)
	{
		triEdges_[0] = first;
		triEdges_[1] = second;
		triEdges_[2] = third;
	}

	//! Return triangle edges ids
	/*!
      \param first  first edge
      \param second second edge
      \param third  third edge
	 */
	void edges (long& first, long& second, long& third)
	{
		first  = triEdges_[0];
		second = triEdges_[1];
		third  = triEdges_[2];
	}

	long edgeAt (short num)
	{
		if ((num > -1) && (num < 3))
			return triEdges_[num];
		else
			return -1;
	}

	TeGeomRep elemType() { return TeSAMPLES; }

private:

	long	triEdges_[3];
};

//! TeTinTriangle: A class for handling a std::set of TIN triangles
class TL_DLL TeTinTriangleSet : public TeGeomComposite<TeTinTriangle>
{
public:

// -- Constructors

	TeTinTriangleSet()	{}

// -- Methods

	bool loadFromFile (std::string &fileName);

	TeGeomRep elemType() { return TeSAMPLES; }
};

//! A class to handle a TIN
class TL_DLL TeTin
{
public:

// -- Constructors

	TeTin() {}
	TeTin(TeBox & /* box */) {}

// -- Methods
	void clear()
	{
		vertexSet_.clear();
		triangleSet_.clear();
		edgeSet_.clear();
	}

	TeBox box()
	{ return vertexSet_.box(); }

	bool createInitialTriangles(TeBox &box);
	bool insertLineSamples ( TeContourLine& line );
	bool loadFromFile(std::string &fileName);
	bool getLines(TeLineSet &lineSet);
	bool isEmpty ()
	{ return ( vertexSet_.empty() && triangleSet_.empty() &&
	           edgeSet_.empty() ); } 

	TeTinEdge& edge (long i)
	{ return edgeSet_[i]; }

	TeTinVertex& vertex (long i)
	{ return vertexSet_[i];	}

	TeTinTriangle& triangle (long i)
	{ return triangleSet_[i];	}

	typedef TeTinVertexSet::iterator TeTinVertexIterator;

	TeTinVertexIterator vertexBegin()
	{ return vertexSet_.begin(); }

	TeTinVertexIterator vertexEnd()
	{ return vertexSet_.end(); }

	typedef  TeTinEdgeSet::iterator TeTinEdgeIterator;

	TeTinEdgeIterator edgeBegin()
	{ return edgeSet_.begin(); }

	TeTinEdgeIterator edgeEnd()
	{ return edgeSet_.end(); }

	typedef  TeTinTriangleSet::iterator TeTinTriangleIterator;

	TeTinTriangleIterator triangleBegin()
	{ return triangleSet_.begin(); }

	TeTinTriangleIterator triangleEnd()
	{ return triangleSet_.end(); }

	bool triangleContainsPoint(TeTinTriangle& t, TeCoord2D& pt);
	bool triangleContainsPoint(long t, TeCoord2D& pt)
	{ return triangleContainsPoint(triangleSet_[t], pt); }

	long edgeWithVertexDiffFromEdge(long t, long v, long e);
	long oneTriangleWithVertex(long vertex);
	long oneEdgeWithVertex(long vertex);

	long edgeOppVertex(TeTinTriangle& triangle, long vertex);
	long edgeOppVertex(long triangle, long vertex)
	{ return edgeOppVertex(triangleSet_[triangle], vertex); }

	long vertexOppEdge(long triangle, long edge);
	bool vertexEdges(long vertex, std::list<long> &edges);
	bool vertexOppEdges(long vertex, std::list<long> &edges);
	bool vertexOppEdges(long v, std::list<long> &triset, std::list<long> &edges);

	bool vertexOppVertices(long vertex, std::list<long> &vertices);
	bool vertexSecondNeighVertices(long v, list<long> &vertices);
	bool vertexTriangles(long vertex, std::list<long> &triangles);
	bool triangle3Neighbors(long t, long& neighbor0, long& neighbor1, long& neighbor2);
	long triangleAt (TeCoord2D& pt);

	bool triangleVertices(TeTinTriangle& t, long& vertex0, long& vertex1, long& vertex2);
	bool triangleVertices(long t, long& vertex0, long& vertex1, long& vertex2)
	{ return this->triangleVertices(triangleSet_[t], vertex0, vertex1, vertex2); }

	long triangleVertexAt(long t, long vertex);

	bool trianglePoints(TeTinTriangle& t, TeCoord2D& pt0, TeCoord2D& pt1, TeCoord2D& pt2);
	bool trianglePoints(long t, TeCoord2D& pt0, TeCoord2D& pt1, TeCoord2D& pt2)
	{ return trianglePoints( triangleSet_[t], pt0, pt1, pt2); }
	
	bool twoNewTriangles(long t, long v, std::set<long>& triangles);
	long duplicateTriangle(long t, long n, long v, std::set<long>& triangles);
	bool dupNeighTriangle (long t, long e, long v, std::set<long>& triangles);

	void testDelaunayForVertex(long v, std::set<long>& triangles);
	bool testDelaunayAt(long t, long v, std::set<long>& triangles);

	bool swapEdges( long t, long tv, long ai );

	bool insertPoint(TeSample& xyz)
	{ return insertPoint ( xyz.location().x(),xyz.location().y(),xyz.value() ); }

	bool insertPoint(double x, double y, double value);

	bool edgesInterBySegment( TeCoord2D& pf, TeCoord2D& pn, std::list<long> &aiset, std::list<long> &viset );
	bool insertSegment( TeCoord2D& pf, TeCoord2D& pn );


	void convexize ();
	bool isEdgeTriangle (long t);

	TeGeomRep elemType() { return TeSAMPLES; }
private:

	TeTinVertexSet		vertexSet_;
	TeTinTriangleSet	triangleSet_;
	TeTinEdgeSet		edgeSet_;
};

#endif
