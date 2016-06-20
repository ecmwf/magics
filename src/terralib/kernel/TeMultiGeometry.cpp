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

#include "TeMultiGeometry.h"


TeMultiGeometry::TeMultiGeometry(const TeMultiGeometry& other)
{
	polygons_.copyElements(other.polygons_);
	lines_.copyElements(other.lines_);
	points_.copyElements(other.points_);
	cells_.copyElements(other.cells_);
	texts_.copyElements(other.texts_); 
}

TeMultiGeometry& 	
TeMultiGeometry::operator= (const TeMultiGeometry& other)
{
	if ( this != &other )
	{
		polygons_.copyElements(other.polygons_);
		lines_.copyElements(other.lines_);
		points_.copyElements(other.points_);
		cells_.copyElements(other.cells_);
		texts_.copyElements(other.texts_); 
	}
	return *this;
}
	
	
bool 
TeMultiGeometry::getGeometry(TePolygonSet& result) const
{
	if(!hasPolygons())
		return false;
	result.copyElements(polygons_);
	return true;
}


bool 
TeMultiGeometry::getGeometry(TeLineSet& result) const
{
	if(!hasLines())
		return false;
	result.copyElements(lines_);
	return true;
}
	
bool 
TeMultiGeometry::getGeometry(TePointSet& result) const
{
	if(!hasPoints())
		return false;
	result.copyElements(points_);
	return true;
}
	
bool 
TeMultiGeometry::getGeometry(TeCellSet& result) const
{
	if(!hasCells())
		return false;
	result.copyElements(cells_);
	return true;
}

bool 
TeMultiGeometry::getGeometry(TeTextSet& result) const
{
	if(!hasTexts())
		return false;
	result.copyElements(texts_);
	return true;
}
	
bool 
TeMultiGeometry::getGeometry(vector<TeGeometry*>& result)  
{
	bool status = false;
	if(hasPolygons())
	{
		for(int i=0; i< (int)polygons_.size(); ++i)
		{
			TePolygon* pol = new TePolygon();
			(*pol) = polygons_[i];
			result.push_back (pol);
			status = true;
		}
	}
	if(hasLines())
	{
		for(int i=0; i< (int)lines_.size(); ++i)
		{
			TeLine2D* lin = new TeLine2D();
			(*lin) = lines_[i];
			result.push_back (lin);
			status = true;
		}
	}
	if(hasPoints())
	{
		for(int i=0; i< (int)points_.size(); ++i)
		{
			TePoint* pon = new TePoint();
			(*pon) = points_[i];
			result.push_back (pon);
			status = true;
		}
	}
	if(hasCells())
	{
		for(int i=0; i< (int)cells_.size(); ++i)
		{
			TeCell* cell = new TeCell();
			(*cell) = cells_[i];
			result.push_back (cell);
			status = true;
		}
	}
	return status;
}

void 
TeMultiGeometry::setGeometry(const TePolygonSet& result) 
{ 
	polygons_ = result;
}
	
void 
TeMultiGeometry::setGeometry(const TeLineSet& result)     
{ 
	lines_ = result; 
}
	
void 
TeMultiGeometry::setGeometry(const TePointSet& result)	
{ 
	points_ = result; 
}
	
void 
TeMultiGeometry::setGeometry(const TeCellSet& result)		
{ 
	cells_ = result; 
}
	
void 
TeMultiGeometry::setGeometry(const TeTextSet& result)		
{ 
	texts_ = result; 
}

void 
TeMultiGeometry::setGeometry(vector<TeGeometry*>& result)
{
	this->clear();
	vector<TeGeometry*>::iterator it = result.begin();
	
	while(it!=result.end())
	{
		TeGeometry* geom = (*it);
		
		if(dynamic_cast<TePolygon*> (geom))
			polygons_.add (*((TePolygon*)geom));
		else if (dynamic_cast<TePolygonSet*> (geom))
			polygons_ = *((TePolygonSet*) geom);
		else if(dynamic_cast<TeLine2D*> (geom))
			lines_.add (*((TeLine2D*)geom));
		else if(dynamic_cast<TeLineSet*> (geom))
			lines_ = *((TeLineSet*)geom);
		else if(dynamic_cast<TePoint*> (geom))
			points_.add (*((TePoint*)geom));
		else if(dynamic_cast<TePointSet*> (geom))
			points_ = *((TePointSet*)geom);
		else if(dynamic_cast<TeCell*> (geom))
			cells_.add (*((TeCell*)geom));
		else if(dynamic_cast<TeCellSet*> (geom))
			cells_ = *((TeCellSet*)geom);
		else if(dynamic_cast<TeText*> (geom))
			texts_.add (*((TeText*)geom));
		++it;
	}
}
	
void 
TeMultiGeometry::addGeometry(const TePolygon& poly)  
{ 
	polygons_.add(poly); 
	polygons_.objectId(poly.objectId()); 
}
	
void 
TeMultiGeometry::addGeometry(const TeLine2D& line)     
{ 
	lines_.add(line); 
	lines_.objectId(line.objectId()); 
}
	
void 
TeMultiGeometry::addGeometry(const TePoint& point)	
{ 
	points_.add(point); 
	points_.objectId(point.objectId()); 
} 
	
void 
TeMultiGeometry::addGeometry(const TeCell& cell)		
{ 
	cells_.add(cell); 
	cells_.objectId(cell.objectId()); 
}

void 
TeMultiGeometry::addGeometry(const TeText& text)		
{ 
	texts_.add(text); 
	texts_.objectId(text.objectId()); 
}

void 
TeMultiGeometry::clear()
{
	polygons_.clear ();
	lines_.clear ();
	points_.clear ();
	cells_.clear ();
	texts_.clear();
}

TeBox 
TeMultiGeometry::getBox()
{
	TeBox box;
	if (hasPolygons())
		updateBox(box,polygons_.box());
	if (hasLines())
		updateBox(box,lines_.box());
	if (hasPoints())
		updateBox(box,points_.box());
	if (hasCells())
		updateBox(box,cells_.box());
	if (hasTexts())
		updateBox(box,texts_.box());
	return box;
}

