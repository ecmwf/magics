/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/*! \file TeGeometry.h
    \brief This file contains structures and definitions about	geometries support in TerraLib
*/

#ifndef __TERRALIB_INTERNAL_GEOMETRY_H
#define __TERRALIB_INTERNAL_GEOMETRY_H

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeDefines.h"
#include "TeBox.h"
#include "TeCoord2D.h"
#include "TeComposite.h"
#include "TeMeasure.h"
#include "TeUtils.h"
#include "TeDataTypes.h"

#include <string>
#include <iostream>


using namespace std;

//!  A basic class for handling geometries in TerraLib
/*! 
	 All geometric classes of TerraLib are derived from
	 TeGeometry. This class keep track of a geometryId and of a bounding box for the geometry
	
	 \note The Geometry classes in TerraLib use the Composite and the Visitor patterns
	 \sa TeBox TeGeomComposite, TeGeomSingle 
*/
class TL_DLL TeGeometry
{
public:

	//! Empty constructor
	TeGeometry(): box_ ( TeMAXFLOAT, TeMAXFLOAT, -TeMAXFLOAT, -TeMAXFLOAT ),
		geomId_  ( 0 ), objectId_ ("") {}

	//! Copy Constructor
	TeGeometry ( const TeGeometry& other ) 
	{
		box_    = other.box_;
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;
	}
	
	//! Destructor
	virtual ~TeGeometry() {}

	//! Sets the bounding box for the object
	void setBox ( const TeBox & box )
	{ box_  = box; }
	
	//! Returns the constant bounding box
	const TeBox& box () const 
	{ return box_; }

	//! Returns the bounding box
	TeBox& box ()
	{ return box_; }

	//! Returns the geometry Id
	int geomId() const
	{ return geomId_; }

	//! Sets the geometryId.
	void geomId( int id )
	{ geomId_ = id; }

	//! Returns the object unique identification
	virtual string objectId() const
	{ return objectId_; }

	//! Sets the objectId 
	virtual void objectId ( const string& id )
	{ objectId_ = id; }

	//! Return the geometry size 
	virtual unsigned int size() const
	{ return 0; }

	//! Outputs the geometical identification to an output stream
	ostream& operator<<(ostream& os)
	{
		os << Te2String(geomId_);
		return os;
	}

	//! Returns TRUE if a geometry is a closed ring  
	virtual bool isRing() const
	{ return false; }

	//! Returns the basic geometry type in a set of geometries structure
	virtual TeGeomRep elemType() 
	{ return TeGEOMETRYNONE; }
	
protected:

	TeBox	box_;		//!<  The bounding box of the geometry
	int	geomId_;		//!<  The unique geometry identification
	string	objectId_;	//!<  The unique object identification associated to a geometry
};


//! A class that represents a non existent geometry
/*!
	This class is used to deal in a similar way with objects with or without geometry 
*/
class TL_DLL TeGeometryNone:  public TeGeometry
{
public: 
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeGEOMETRYNONE; }

	//! Removes geometry elements
	void clear () { return; }
};

//! A class to handle vector geometries
class TL_DLL TeVector : public TeGeometry
{
};

//!   TeGeomSingle: A class for handling geometries in TerraLib which consist of one location
/*!  
  \sa TePoint, TeSample 
*/
template <class T>
class TeGeomSingle : public TeVector
{
public:

    //! Exports the type of the element of a TeGeomSingle
	typedef  T value_type;

	//! Empty constructor
	TeGeomSingle<T>() {}

	//! Constructor from the single element of the container
	TeGeomSingle<T> (const T& elem ): elem_ ( elem  ) 
	{	updateBox ( box_, elem ); }

	//! Copy Constructor
	TeGeomSingle ( const TeGeomSingle& other ) : TeVector()
	{
		box_    = other.box_;
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;
		elem_ = other.elem_;
	}

	//! Equal Operator
	TeGeomSingle& operator = ( const TeGeomSingle& other )
	{
		box_    = other.box_;
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;
		elem_ = other.elem_;
		return *this;
	}

	//! Destructor
	virtual ~TeGeomSingle<T>() {}

	//! Changes the unique element
	void add ( T& elem )
	{
		elem_ = elem;
		box_ = TeBox(TeMAXFLOAT, TeMAXFLOAT,-TeMAXFLOAT,-TeMAXFLOAT); // invalidates its box
		updateBox ( box_, elem );
	}

	//! Returns the unique element
	T&	location ()
	{	return elem_; }

	//! Returns the unique element
	const T& location () const
	{	return elem_; }

	//! Returns the unique element
	T& elem ()
	{	return elem_; }

	//! Returns the unique element
	const T& elem () const
	{	return elem_; }

	//! Returns the unique element
	T& operator [] ( int /* i */) 
	{	return elem_; }

	//! Returns TRUE if a TeGeomSingle is equal to other
	bool operator== (const TeGeomSingle& other) const
	{	return elem_ == other.elem(); }

	//! Returns the size of a TeGeomSingle: always 1
	int size() { return 1; }

protected:
	T	elem_;

};



//!  TeGeomComposite: A template class for handling a hierarchy of geometries in TerraLib
/*!

	  Used for instantiating the different geometries. Provide a vector to store the 2D 
	  coordinates of a ring. Multiple copies of a geometry are allowed to share the same 
	  coordinates by means of a "handle/body"  idiom.

	\sa  TeLine2D TePolygon TeLineSet TePolygonSet
*/

template <class T>  
class TeGeomComposite: public TeVector
{
public:

	//! Constructor
	TeGeomComposite()
	{
		pImpl_ = new TeComposite<T>;
		pImpl_->attach();
	}

	//! Destructor
	virtual ~TeGeomComposite()
	{	pImpl_->detach();	}


	//! Copy Constructor
	TeGeomComposite ( const TeGeomComposite& other ) : TeVector()
	{
		pImpl_ = other.pImpl_;
		pImpl_->attach();
		box_    = other.box_;
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;
	}

	//! Operator =
	TeGeomComposite& operator= ( const TeGeomComposite& other )
	{
		if ( this != &other )
		{	
			other.pImpl_->attach();
			pImpl_->detach();
			pImpl_  = other.pImpl_;
			box_    = other.box_;
			geomId_ = other.geomId_;
			objectId_ = other.objectId_;
		}
		return *this;
	}

	//! Returns the identification of the object associated to this geometry
	virtual string objectId() const
	{ return objectId_; }

	//! Sets the identification of the object associated to this geometry
	virtual void objectId (const string& id )
	{ 
		objectId_ = id; 
		typename TeComposite<T>::iterator it = pImpl_->begin();
		while (it != pImpl_->end())
		{
			it->objectId(id);
			++it;
		}
	}

	//! Copy two composites, duplicating elements (breaking handle/body idiom)
	void copyElements ( const TeGeomComposite& other )
	{
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;

		for (unsigned int i = 0; i < other.pImpl_->size(); i++)
			add (other.pImpl_->operator[](i));
	}

	//! Returns TRUE if two composites have exactly the same elements
	bool operator== (const TeGeomComposite& other) const
	{	
		if ( this->size() != other.size() )
			return false;

		for (unsigned int i = 0; i < other.pImpl_->size(); i++)
			if ( (pImpl_->operator[](i))==(other.pImpl_->operator[]( i )) )
				return false;
		return true; 
	}

	//! Adds a new component
	void add ( const T& elem )
	{	
		pImpl_->add ( elem );
		updateBox ( box_, elem );
	}

	//! Removes the i-th component
	bool erase ( int i )
	{ 	
		bool status = pImpl_->erase (i);

		if (status)	// recalculates the box
		{
			box_ = TeBox(TeMAXFLOAT, TeMAXFLOAT,-TeMAXFLOAT,-TeMAXFLOAT);
			for (unsigned int j = 0; j < pImpl_->size(); j++)
				updateBox(box_,pImpl_->operator[](j));
		}
		return status;
	}

	//! Removes an element
	bool erase ( T& object )
	{	
		bool status = pImpl_->erase ( object ); 
		if (status)	// recalculates the box
		{
			box_ = TeBox(TeMAXFLOAT, TeMAXFLOAT,-TeMAXFLOAT,-TeMAXFLOAT);
			for (unsigned int j = 0; j < pImpl_->size(); j++)
				updateBox(box_,pImpl_->operator[](j));
		}
		return status;
	}

	//! Removes the element pointed by an interator
	typename TeComposite<T>::iterator erase(typename TeComposite<T>::iterator it)
	{	
		typename TeComposite<T>::iterator res = pImpl_->erase(it); 
		box_ = TeBox(TeMAXFLOAT, TeMAXFLOAT,-TeMAXFLOAT,-TeMAXFLOAT);
		for (unsigned int j = 0; j < pImpl_->size(); j++)
			updateBox(box_,pImpl_->operator[](j));
		return res;
	}

	//! Removes all elements
	void clear ()
	{	
		pImpl_->clear ();		// remove all elements
		box_ = TeBox(TeMAXFLOAT, TeMAXFLOAT,-TeMAXFLOAT,-TeMAXFLOAT); // invalidates its box
	}
	
	//! Returns the size of the composite
	unsigned int size() const
	{	return ( (unsigned int) pImpl_->size() ); }

	//! Reserves space for a given number of elements (reserve is available for vectors)  
	void reserve(int nelem)
	{ pImpl_->reserve(nelem); }

	//! Returns the i-th element
	T& operator [] ( int i ) const
	{	return pImpl_->operator[] ( i ); }

	//! Returns the first element
	T& first() const
	{	return pImpl_->operator[] ( 0 ); }
	
	//! Returns the i-th element
	T& last() const
	{	return pImpl_->operator[] ( pImpl_->size()-1 ); }
	
	//! Returns TRUE if composite is empty
	bool empty () const
	{	return pImpl_->empty (); }

	//! An Iterator that enables forward traversal of a TeGeomComposite
	typedef typename TeComposite<T>::iterator iterator;

	//! The type of the value obtained by dereferencing a TeGeomComposite iterator
//	typedef typename T value_type;
	typedef  T value_type;

	//! The iterator to the first position in the TeGeomComposite
	typename TeComposite<T>::iterator begin()
	{ return pImpl_->begin(); }

	//! The iterator to the first position in the TeGeomComposite
	typename TeComposite<T>::iterator const begin() const
	{ return pImpl_->begin(); }

	//! The iterator to the last plus one position in the TeGeomComposite
	typename TeComposite<T>::iterator end()
	{ return pImpl_->end(); }

	//! The iterator to the last plus one position in the TeGeomComposite
	typename TeComposite<T>::iterator const end() const
	{ return pImpl_->end(); }

	//! An Iterator that enables backward traversal of a TeGeomComposite
	typedef typename TeComposite<T>::reverse_iterator reverse_iterator;

	//! The iterator to the first position in the TeGeomComposite in reverse order
	typename TeComposite<T>::reverse_iterator rbegin()
	{ return pImpl_->rbegin(); }

	//! The iterator to the last plus one position in the TeGeomComposite in reverse order
	typename TeComposite<T>::reverse_iterator rend()
	{ return pImpl_->rend(); }

protected:

	//! Pointer to the implementation of a composite<T>
	TeComposite<T> * pImpl_; 
};

//!  TeLine2D: Supports a simple 2D line,  composed of 2D xy points
/*!
	\sa TeGeometry TeLinearRing
*/
class TL_DLL TeLine2D : public TeGeomComposite<TeCoord2D>
{
public:
	//! Check if a line2D is a closed ring
	bool isRing() const;	

	//! Returns the identification of the object associated to this geometry
	string objectId() const
	{ return objectId_; }
	
	//! Sets the identification of the object associated to this geometry
	void objectId (const string& id )
	{  objectId_ = id; }

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeLINES; }
};

//!  TeLineSet: Supports a composite of lines
/*!
  \sa TeGeomComposite
*/
class TL_DLL TeLineSet: public TeGeomComposite<TeLine2D>
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeLINES; }

	//! Executes a real copy of two sets (duplicate the elements)
	void copyElements (const TeLineSet& other ); 
}; 

//!  TeLinearRing: Provides support for a 2D linear ring
/*!
	 A linear ring is a 2D line (without self-intersections) whose 
	 first point is the same as the last point.  A linear ring cannot be created 
	 directly, but is instantiated from a 2D line.

   \sa TePolygon 
*/
class TL_DLL TeLinearRing : public TeLine2D  {
public:

	//! Empty constructor
	TeLinearRing() : TeLine2D() {}

	//! Contructor from a line
	TeLinearRing ( TeLine2D& line );
};


//!  TePolygon: A class for handling 2D polygons. 
/*!
	In TerraLib, a 2D polygon consists of an outer ring and a list
	of inner rings 
*/
class TL_DLL TePolygon: public TeGeomComposite<TeLinearRing>  
{
public:
	
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TePOLYGONS; }

	//! Executes a real copy of two sets (duplicate the elements)
	void copyElements ( const TePolygon& other );
};

//!  TePolygonSet: A class for handling sets of 2D polygons. 
class TL_DLL TePolygonSet: public TeGeomComposite<TePolygon> 
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TePOLYGONS; }

	//! Executes a real copy of two sets (duplicate the elements)
	void copyElements ( const TePolygonSet& other );
};

	
//!  TePoint: A class for handling 2D Points. 
class TL_DLL TePoint : public TeGeomSingle<TeCoord2D>
{
public:
	//! Default constructor
	TePoint(const double& x = 0., const double& y = 0. ):
		TeGeomSingle<TeCoord2D> ( )
	{
			elem_ = TeCoord2D(x,y);
			setBox(TeBox(x,y,x,y)); // the box of a point is the point itself
	}

	//! Copy constructor
	TePoint(TeCoord2D& c):
		TeGeomSingle<TeCoord2D> ( )
	{
			elem_ = c;
			setBox(TeBox(c.x(),c.y(),c.x(),c.y())); // the box of a point is the point itself
	}

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TePOINTS; }

	//! Returns the identification of the object associated to this geometry
	string objectId() const
	{ return objectId_; }
	
	//! Sets the identification of the object associated to this geometry
	void objectId (const string& id )
	{  objectId_ = id; }

};

//!  TePointSet:  A class for handling sets of 2D polygons. 
class TL_DLL TePointSet: public  TeGeomComposite<TePoint> 
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TePOINTS; }
};

//!  TeText : A class for handling text.
class TL_DLL TeText: public TeGeomSingle<TeCoord2D>
{
public:
	//! Default contructor
	/*!
      \param txt the string of character that form the text (default is an empty string)
	*/
	TeText(const string& txt="" ):
		TeGeomSingle<TeCoord2D> ( ),
		angle_(0),
		height_(0),
		textValue_(txt),
		alignmentVert_(0),
		alignmentHoriz_(0)
	{
		elem_ = TeCoord2D(0,0);
		setBox(TeBox(0.0,0.0,0.0,0.0));
	}

	//! Constructor
	/*!
		\param location basic position of the text
		\param txt the string of character that form the text (default is an empty string)
	*/

	TeText( TeCoord2D& location, const string& txt="" ):
		TeGeomSingle<TeCoord2D> ( location ),
		angle_(0),
		height_(0),
		textValue_(txt),
		alignmentVert_(0),
		alignmentHoriz_(0)
	{
		setBox(TeBox(location,location));
	}

	//! Copy constructor
	TeText(const TeText& other ) : TeGeomSingle<TeCoord2D>()
	{
		angle_ = other.angle_;
		height_ = other.height_;
		textValue_ = other.textValue_;
		alignmentVert_ = other.alignmentVert_;
		alignmentHoriz_ = other. alignmentHoriz_;
		setBox(other.box());
		elem_ = other.elem_;
		geomId_ = other.geomId_;
		objectId_ = other.objectId_;
	}

	//! Operator =
	TeText& operator= ( const TeText& other )
	{
		if ( this != &other )
		{	
			angle_ = other.angle_;
			height_ = other.height_;
			textValue_ = other.textValue_;
			alignmentVert_ = other.alignmentVert_;
			alignmentHoriz_ = other. alignmentHoriz_;
			setBox(other.box());
			elem_ = other.elem_;
			geomId_ = other.geomId_;
			objectId_ = other.objectId_;
		}
		return *this;
	}

	//! Returns TRUE if a text is equal to other
	bool operator== (const TeText& tx) const 
	{
		return (angle_ == tx.angle_ &&
			height_ == tx.height_ &&
			textValue_ == tx.textValue_ &&
			alignmentVert_ == tx.alignmentVert_ &&
			alignmentHoriz_ == tx.alignmentHoriz_ &&
			elem_ == tx.elem_ &&
			geomId_ == tx.geomId_ &&
			objectId_ == tx.objectId_);
	}

	//! Sets anew value for the location of the text
	void setLocation(TeCoord2D& l)
	{	elem_ = l; setBox(TeBox(l,l));	}

	//! Returns the string value of a text
	string textValue () const
	{ return textValue_; }
	
	//! Sets the string value of a text
	void setTextValue (const string &text) 
	{ textValue_ = text; }

	//! Returns the text inclination
	double angle () const
	{ return angle_; }

	//! Sets the text inclination
	void setAngle (double angle) 
	{ angle_ = angle; }

	//! Returns the text height
	double height () const
	{ return height_ ; }

	//! Sets the text height
	void setHeight (double height) 
	{ height_ = height; }

	//! Returns the text vertical alignment
	double alignmentVert () const
	{ return alignmentVert_ ; }

	//! Sets the text vertical alignment
	void setAlignmentVert (double alig) 
	{ alignmentVert_ = alig; }

	//! Returns the text horizontal alignment
	double alignmentHoriz () const
	{ return alignmentHoriz_ ; }

	//! Sets the text horizontal alignment
	void setAlignmentHoriz (double alig) 
	{ alignmentHoriz_ = alig; }

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeTEXT; }

private:
	double angle_;	
	double height_;
	string textValue_;
	double alignmentVert_;
	double alignmentHoriz_;
};


//!  TeTextSet : A class for handling sets of TeText.
class TL_DLL TeTextSet : public TeGeomComposite<TeText> 
{
public:
	//! Returns the basic geometry in a set of geometries structure
	TeGeomRep elemType() { return TeTEXT; }

};


//! TeNode:  A class for handling 2D Nodes.
class TL_DLL TeNode: public TeGeomSingle<TeCoord2D>
{
public:
	//! Returns TRUE if nodes are equal
	bool operator== (const TeNode& node) const
	{
		TeCoord2D p1 = elem_;
		TeCoord2D p2 = node.elem_;
		return p1==p2;
	}

	//! Outputs the geometrical identification of a node
	ostream& operator<<(ostream& os)
	{
		os << Te2String(geomId_);
		return os;
	}

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeNODES; }

};

//! Outputs the geometrical identification of a node
TL_DLL ostream& operator<<(ostream& os, TeNode& N);

//!  TeNodeSet :  A class for handling sets of 2D Nodes. 
class TL_DLL TeNodeSet : public TeGeomComposite<TeNode> 

{
public:
	//! Returns the basic geometry in a set of geometries structure
	TeGeomRep elemType() { return TeNODES; }

};

//!  TeArc : Provides support for a 2D arc.
class TL_DLL TeArc : public TeVector 
{
public:

	//! Empty contructors
	TeArc(): ifrom_ (-1), ito_ (-1){}

	//! Construtor
	/*!
		\param from the starting node of an arc
		\param to the ending node of an arc
	*/
	TeArc(TeNode& from, TeNode& to)
	{
		from_ = from;
		to_ = to;
		updateBox ( box_, from );
		updateBox ( box_, to );
	}

	//! Construtor
	/*!
		\param from the geometrical identification of the  starting node of an arc
		\param to the geometrical identification of the ending node of an arc
	*/
	TeArc(int from, int to): ifrom_ (from), ito_ (to) {}

// -- Methods

	//! Returns the starting node 
	TeNode& fromNode () 
	{ return from_; }

	//! Returns the ending node 
	TeNode& toNode () 
	{ return to_; }

	//! Returns the geometrical identification of the starting node 
	int fromId () const
	{ return ifrom_; }

	//! Sets the geometrical identification of the starting node 
	void fromId (int i) 
	{ ifrom_ = i; }

	//! Returns the geometrical identification of the ending node 
	int toId () const
	{ return ito_; }

	//! Sets the geometrical identification of the ending node 
	void toId (int i)
	{ ito_ = i; }
	
	//! Sets the starting and ending node 
	void setNodes (TeNode& from, TeNode& to)
	{ 
		from_ = from;
		to_ = to;
		updateBox ( box_, from );
		updateBox ( box_, to );
	}

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeARCS; }

	//! Returns TRUE if a TeArc is equal to other
	bool operator== (const TeArc& other) const
	{
		if((from_ == other.from_) &&
	       (to_ == other.to_) &&
		   (ifrom_ == other.ifrom_) &&
           (ito_ != other.ito_))
			return true;

		return false;
	}

private:

	TeNode	from_, to_;
	int		ifrom_, ito_;
};

//! Outputs the description of an arc
TL_DLL ostream& operator<<(ostream& os, const TeArc& N);


//!  TeArcSet: Provides support for a set of 2D arc.
class TL_DLL TeArcSet: public TeGeomComposite <TeArc> 
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeARCS; }
};

//!  TeSample: A class for handling 2D Points with an associated measure.
class TL_DLL TeSample: public TeGeomSingle<TeCoord2D>, public TeMeasure
{
public:
	//! Constructor
	/*!
		\param location the position of the sample
		\param measure the value associated to the sample
	*/
	TeSample ( TeCoord2D& location, double measure = 0.  ):
		TeGeomSingle<TeCoord2D> ( location ), TeMeasure ( measure ) {}

	TeGeomRep elemType() { return TeSAMPLES; }
};

//!  TeSampleSet: A class for handling sets of 2D samples
class TL_DLL TeSampleSet: public TeGeomComposite<TeSample>
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeSAMPLES; }
};

//!  TeContourLine: A class for handling 2D countour lines
class TL_DLL TeContourLine: public TeLine2D, public TeMeasure
{
public:
	//! Constructor
	/*!
		\param line the contour line
		\param measure the value associated to the contour line
	*/
	TeContourLine ( TeLine2D& line, double measure = 0. )
		: TeLine2D ( line ), TeMeasure ( measure )
	{}
};


//!  TeContourLineSet: A class for handling sets of 2D countour lines
class TL_DLL TeContourLineSet: public  TeGeomComposite <TeContourLine> 
{
public:
	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeSAMPLES; }
};


//!    A class for handling cells. 
class TL_DLL TeCell : public TeVector 
{
	int column_;	//!< the column number of this cell	
	int line_;		//!< the line number of this cell

public:
	//! Empty constructor
	TeCell():
	  column_(-1),
	  line_(-1) {}

	TeCell(TeBox& box, int col, int lin):
	  column_(col),
	  line_(lin) { setBox(box); }

	//! Returns the column identification of the cell
	int	column () const
	{ return column_; }

	//! Sets the column identification of the cell
	void column (int column) 
	{ column_ =  column; }

	//! Returns the line identification of the cell
	int	line () const
	{ return line_; }

	//! Sets the line identification of the cell
	void line (int line) 
	{ line_ =  line; }

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeCELLS; }

	//! Returns TRUE if a TeCell is equal to other
	bool operator== (const TeCell& other) const
	{
		if((column_ == other.column_) &&
	       (line_ == other.line_))
			return true;

		return false;
	}
};

//!  A class for handling sets of cells.
class TL_DLL TeCellSet: public  TeGeomComposite<TeCell> 
{
	double	resX_;	//!< the X resolution of a set of cells
	double	resY_;	//!< the Y resolution of a set of cells

public:

	//! Returns the X resolution of a cell set
	double resX () const
	{ return resX_; }

	//! Returns the Y resolution of a cell set
	double resY () const
	{ return resY_; }

	//! Sets the X resolution of a cell set
	void resX (double reX) 
	{ resX_ = reX; }

	//! Sets the Y resolution of a cell set

	void resY (double reY) 
	{ resY_ = reY; }

	//! Returns the basic geometry in a set of geometries structure 
	TeGeomRep elemType() { return TeCELLS; }

};


/*! \fn TePointSet makePointSet( const TeLinearRing& lr )
   \brief builds a TePointSet geometry from a TeLinearRing
 */
TL_DLL TePointSet makePointSet( const TeLinearRing& lr );


/*! \fn TePointSet makePointSet( const TePolygon& p )
   \brief builds a TePointSet geometry from a TePolygon
 */
TL_DLL TePointSet makePointSet( const TePolygon& p );

#endif


