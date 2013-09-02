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


#include <TeSTInstance.h>


TeSTInstance::TeSTInstance (const string& object_id, TeProperty& prop) :
	TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>()
{
	object_id_ = object_id;
	properties_.push_back(prop.value_);
}

void 
TeSTInstance::clear() 
{
	TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>::clear();
	geometries_.clear(); 
	time_ = TeTimeInterval();
}


bool 
TeSTInstance::isTimeValid()
{
	return (this->time_.isValid());
}
	
bool 
TeSTInstance::hasPolygons()	
{ 
	return (geometries_.hasPolygons()); 
} 
	
bool 
TeSTInstance::hasLines()		
{ 
	return (geometries_.hasLines()); 
} 
	
bool 
TeSTInstance::hasPoints()		
{ 
	return (geometries_.hasPoints()); 
} 
	
bool 
TeSTInstance::hasCells()		
{ 
	return (geometries_.hasCells()); 
} 

bool 
TeSTInstance::hasTexts()		
{ 
	return (geometries_.hasTexts()); 
} 
	
bool 
TeSTInstance::getGeometry(TePolygonSet& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}
	
bool 
TeSTInstance::getGeometry(TeLineSet& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}

bool 
TeSTInstance::getGeometry(TePointSet& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}

bool 
TeSTInstance::getGeometry(TeCellSet& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}

bool 
TeSTInstance::getGeometry(TeTextSet& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}

bool 
TeSTInstance::getGeometry(vector<TeGeometry*>& result)
{
	if(geometries_.getGeometry(result))
		return true;
	return false;
}

bool 
TeSTInstance::getGeometry(TeMultiGeometry& result)
{
	if(!geometries_.empty())
	{
		result = geometries_;
		return true; 
	}

	geometries_.getGeometry (result.polygons_);
	geometries_.getGeometry (result.lines_);
	geometries_.getGeometry (result.points_);
	geometries_.getGeometry (result.cells_);
	
	return (!result.empty());
}

TePolygonSet&  
TeSTInstance::getPolygons()
{	
	return geometries_.getPolygons(); 
}
	
TeLineSet&  
TeSTInstance::getLines()
{	
	return geometries_.getLines(); 
}

TePointSet&  
TeSTInstance::getPoints()
{	
	return geometries_.getPoints(); 
}

TeCellSet&  
TeSTInstance::getCells()
{	
	return geometries_.getCells(); 
}

TeTextSet&  
TeSTInstance::getTexts()
{	
	return geometries_.getTexts(); 
}

void 
TeSTInstance::setGeometry(const TePolygonSet& result)
{ 
	geometries_.setGeometry(result); 
}
	
void 
TeSTInstance::setGeometry(const TeLineSet& result)
{ 
	geometries_.setGeometry(result); 
}
	
void 
TeSTInstance::setGeometry(const TePointSet& result)
{ 
	geometries_.setGeometry(result); 
}
	
void 
TeSTInstance::setGeometry(const TeCellSet& result)
{ 
	geometries_.setGeometry(result); 
}
	
void 
TeSTInstance::setGeometry(const TeTextSet& result)
{ 
	geometries_.setGeometry(result); 
}

void 
TeSTInstance::setGeometry(const TeMultiGeometry& result)
{ 
	geometries_ = result; 
}
 
bool 
TeSTInstance::addGeometry(const TePolygon& poly)  
{ 
	if(poly.objectId() == objectId())
	{
		geometries_.addGeometry (poly);
		return true;
	}
	return false;
}
	
bool 
TeSTInstance::addGeometry(const TeLine2D& line)     
{ 
	if(line.objectId() == objectId())
	{
		geometries_.addGeometry (line);
		return true;
	}
	return false; 
}
	
bool 
TeSTInstance::addGeometry(const TePoint& point)	
{ 
	if(point.objectId() == objectId())
	{
		geometries_.addGeometry (point);
		return true;
	}
	return false;  
} 
	
bool
TeSTInstance::addGeometry(const TeCell& cell)		
{ 
	if(cell.objectId() == objectId())
	{
		geometries_.addGeometry (cell);
		return true;
	}
	return false;  
}

bool
TeSTInstance::addGeometry(const TeText& text)		
{ 
	if(text.objectId() == objectId())
	{
		geometries_.addGeometry (text);
		return true;
	}
	return false;  
}

void 
TeSTInstance::centroid(TeCoord2D& centroid, TeGeomRep geomRep)
{
	if(geomRep==TePOLYGONS)
	{ 
		TePolygonSet pols;
		if(getGeometry(pols))
		{
			centroid = TeFindCentroid(pols);
			return;
		}
	}
	if(geomRep==TeLINES)
	{
		TeLineSet lins; 
		if(getGeometry(lins))
		{
			centroid = TeFindCentroid(lins);
			return;
		}
	}
	if(geomRep==TePOINTS)
	{
		TePointSet points; 
		if(getGeometry(points))
		{
			centroid = TeFindCentroid(points);
			return;
		}
	}
	if(geomRep==TeCELLS)
	{
		TeCellSet cells; 
		if(getGeometry(cells))
		{
			centroid = TeFindCentroid(cells);
			return;
		}
	}
	if(geomRep==TeGEOMETRYNONE)
	{
		if(hasPolygons())
			this->centroid(centroid, TePOLYGONS);
		else if(hasLines())
			this->centroid(centroid, TeLINES);
		else if (hasPoints())
			this->centroid(centroid, TePOINTS);
		else if (hasCells())
			this->centroid(centroid, TeCELLS);
	}

	return;
}

void 
TeSTInstance::area(double& a, TeGeomRep geomRep)
{
	a = 0.;
	if(geomRep==TePOLYGONS)
	{ 
		TePolygonSet pols;
		if(getGeometry(pols))
		{
			a = TeGeometryArea(pols);
			return;
		}
	}
	if(geomRep==TeCELLS)
	{
		TeCellSet cells; 
		if(getGeometry(cells))
		{
			a = TeGeometryArea(cells);
			return;
		}
	}
	if(geomRep==TeGEOMETRYNONE)
	{
		if(hasPolygons())
			this->area(a, TePOLYGONS);
		else if (hasCells())
			this->area(a, TeCELLS);
	}

	return;
}


