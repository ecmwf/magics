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

#include "TeSpatialOperations.h"
#include "TeGeometryAlgorithms.h"
#include "TeRasterRemap.h"
#include "TeStatistics.h"
#include "TeOverlay.h"
#include "TeBufferRegion.h"
#include "TeProgress.h"
#include "TeVectorRemap.h"

#include <algorithm>


TeProjection*
getProjectionFromGeomTable(const string& geomTable, TeDatabase* db)
{
	TeLayerMap layerMap = db->layerMap();
	TeLayerMap::iterator it = layerMap.begin();
	while(it!=layerMap.end())
	{
		TeLayer* layer = it->second;
		TeRepresPointerVector::iterator itRep = layer->vectRepres().begin();
		while(itRep!=layer->vectRepres().end())
		{
			if(TeConvertToUpperCase((*itRep)->tableName_)==TeConvertToUpperCase(geomTable))
				return layer->projection();
			++itRep;
		}
		++it;
	}
	return 0; 
}


// Return if exist or not a totpological relation between two geometries
// Use the TerraLib topological functions
bool TeTopologicalRelation(const TePolygon* geom1, const TePolygon* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2);
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2);
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom1, *geom2);
		break;

		case TeOVERLAPS:
			result = TeOverlaps(*geom1, *geom2);
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2);
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(*geom1, *geom2);
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2);
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom1, *geom2);
		break;
		
		case TeEQUALS:
			result = TeEquals(*geom1, *geom2);
		break;

		case (TeWITHIN | TeCOVEREDBY) :
			result = TeWithinOrCoveredByOrEquals(*geom1, *geom2);
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeOverlaps(*geom1, *geom2)) || (TeWithinOrCoveredByOrEquals(*geom1, *geom2)));
		break;
	}
	return result;
}


bool TeTopologicalRelation(const TePolygon* geom1, const TeLine2D* geom2, int relation)
{
	bool result = false;
	switch (relation)
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom2, *geom1); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom2, *geom1); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom2, *geom1);  
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2);  
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(*geom2, *geom1);  
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom2, *geom1); 
		break;
		
		case TeCROSSES:
			result = TeCrosses(*geom2, *geom1); 
		break;

		case (TeWITHIN | TeCOVEREDBY):
			result = TeWithinOrCoveredByOrEquals(*geom2, *geom1);
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeCrosses(*geom2, *geom1)) || (TeWithinOrCoveredByOrEquals(*geom2, *geom1)));
		break;
	}
	return result;
}


bool TeTopologicalRelation(const TePolygon* geom1, const TePoint* geom2, int relation)
{
	bool result = false;
	switch (relation)
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom2, *geom1); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom2, *geom1); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom2, *geom1);  
		break;

		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom2, *geom1); 
		break;

		case (TeWITHIN | TeCOVEREDBY):
		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = TeWithinOrCoveredByOrEquals((*geom2).location(), *geom1);
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TeLine2D* geom1, const TeLine2D* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom1, *geom2); 
		break;

		case TeOVERLAPS:
			result = TeOverlaps(*geom1, *geom2); 
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2); 
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(*geom1, *geom2); 
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom1, *geom2); 
		break;
		
		case TeEQUALS:
			result = TeEquals(*geom1, *geom2); 
		break;

		case TeCROSSES:
			result = TeCrosses(*geom1, *geom2); 
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeCrosses(*geom1, *geom2)) || (TeWithinOrCoveredByOrEquals(*geom1, *geom2)));
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TeLine2D* geom1, const TePoint* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom2, *geom1); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom2, *geom1); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom2, *geom1); 
		break;

		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom2, *geom1); 
		break;
		
		case (TeWITHIN | TeCOVEREDBY):
		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = TeWithinOrCoveredByOrEquals((*geom2).location(), *geom1);
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TePoint* geom1, const TePoint* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2); 
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom1, *geom2); 
		break;
		
		case TeEQUALS:
		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = TeEquals(*geom1, *geom2); 
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TeCell* geom1, const TeCell* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2);
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2);
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom1, *geom2);
		break;

		case TeOVERLAPS:
			result = TeOverlaps(*geom1, *geom2);
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2);
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(*geom1, *geom2);
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2);
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom1, *geom2);
		break;
		
		case TeEQUALS:
			result = TeEquals(*geom1, *geom2);
		break;

		case (TeWITHIN | TeCOVEREDBY):
			result = TeWithinOrCoveredByOrEquals((*geom1).box(), (*geom2).box());
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeOverlaps(*geom1, *geom2)) || (TeWithinOrCoveredByOrEquals((*geom1).box(), (*geom2).box())));
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TeCell* geom1, const TePolygon* geom2, int relation)
{
	bool result = false;
	switch (relation)  
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2);
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2);
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom1, *geom2);
		break;

		case TeOVERLAPS:
			result = TeOverlaps(*geom1, *geom2);
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2);
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(TeMakePolygon((*geom1).box()), *geom2);
		break;
		
		case TeCONTAINS:
			result = TeWithin(*geom2, TeMakePolygon((*geom1).box()));
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom1, *geom2);
		break;
		
		case TeEQUALS:
			result = TeEquals(*geom1, *geom2);
		break;

		case (TeWITHIN | TeCOVEREDBY):
			result = TeWithinOrCoveredByOrEquals(TeMakePolygon((*geom1).box()), *geom2);
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeOverlaps(*geom1, *geom2)) || (TeWithinOrCoveredByOrEquals(TeMakePolygon((*geom1).box()) , *geom2)));
		break;
	}
	return result;
}

bool TeTopologicalRelation(const TeCell* geom1, const TeLine2D* geom2, int relation)
{
	bool result = false;
	switch (relation)
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom2, *geom1);  
		break;

		case TeCOVERS:
			result = TeCovers(*geom1, *geom2);  
		break;

		case TeCOVEREDBY:
			result = TeCoveredBy(*geom2, *geom1);  
		break;
		
		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom2, *geom1); 
		break;
		
		case TeCROSSES:
			result = TeCrosses(*geom2, *geom1); 
		break;

		case (TeWITHIN | TeCOVEREDBY):
			result = TeWithinOrCoveredByOrEquals(*geom2, TeMakePolygon((*geom1).box()));
		break;

		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = ( (TeCrosses(*geom2, *geom1)) || (TeWithinOrCoveredByOrEquals(*geom2, TeMakePolygon((*geom1).box()))));
		break;
	}
	return result;

}

bool TeTopologicalRelation(const TeCell* geom1, const TePoint* geom2, int relation)
{
	bool result = false;
	switch (relation)
	{		
		case TeDISJOINT:
			result = TeDisjoint(*geom1, *geom2); 
		break;

		case TeINTERSECTS:
			result = TeIntersects(*geom1, *geom2); 
		break;
		
		case TeTOUCHES:
			result = TeTouches(*geom2, *geom1);  
		break;

		case TeCONTAINS:
			result = TeContains(*geom1, *geom2); 
		break;
		
		case TeWITHIN:
			result = TeWithin(*geom2, *geom1); 
		break;

		case (TeWITHIN & TeCOVEREDBY):
		case (TeWITHIN | TeCOVEREDBY | TeOVERLAPS | TeCROSSES):
			result = TeWithinOrCoveredByOrEquals((*geom2).location(), TeMakePolygon((*geom1).box()));
		break;
	}
	return result;

}


// Return a string with the identifications of the selected geometries 
string 
getStringIds(TeKeys& IdsIn)
{
	string Ids = "";
	int cont = 0;
	
	TeKeys::iterator itIdIn = IdsIn.begin();
	while (itIdIn != IdsIn.end())
	{
		if(cont)
			Ids += ","; 
		Ids += "'" + (*itIdIn) + "'";
		itIdIn++;
		cont++;
	}

	return Ids;
}

// fill the geometries vector from a portal
string
mountSelect(string geomTable, string colTable, TeGeomRep rep, TeBox& box, int relate, TeDatabasePortal *portal)
{
	
	string sql = "SELECT * FROM "+ geomTable;
	
	if(!colTable.empty())
		sql +=", "+ colTable +" WHERE c_object_id = object_id ";
		
	//If not disjoint
	if(box.isValid() && (relate!=TeDISJOINT))
	{
		if(!colTable.empty())
			sql += " AND ";
		else
			sql += " WHERE ";

		sql += portal->getDatabase()->getSQLBoxWhere (box, rep);
	}
	return sql;
}

//fill the result vector of the geometries identificators  
bool
fillSelectedGeomVector(TeGeomRep actRep, TeSelectedGeom& vetGeom, TeDatabasePortal *portal, TeProjection* projAct=0, TeProjection* projVis=0)
{
	bool flag = true;
	bool remap = false;

	if((projAct!=0) && (projVis!=0) && (projAct!=projVis))
		remap = true;
	
	try
	{
		do
		{
			if(actRep == TePOLYGONS)
			{
				TePolygon   poly, *pol;
				flag = portal->fetchGeometry(poly); 
				pol = new TePolygon;
				if(remap)
				{
					TePolygon pout;
					TeVectorRemap (poly, projAct, pout, projVis);
					*pol = pout;
				}
				else
					*pol = poly;
				vetGeom.push_back(pol);
			}
			else if(actRep == TeLINES)
			{
				TeLine2D	line, *lin;
				flag = portal->fetchGeometry(line); 
				lin = new TeLine2D();
				if(remap)
				{
					TeLine2D gout;
					TeVectorRemap (line, projAct, gout, projVis);
					*lin = gout;
				}
				else
					*lin = line;
				vetGeom.push_back(lin);
			}
			else if(actRep == TePOINTS)
			{
				TePoint		point, *pnt;
				flag = portal->fetchGeometry(point); 
				pnt = new TePoint();
				if(remap)
				{
					TePoint gout;
					TeVectorRemap (point, projAct, gout, projVis);
					*pnt = gout;
				}
				else
					*pnt = point;
				vetGeom.push_back(pnt);
			}
			else if(actRep == TeCELLS)
			{
				TeCell		cell, *cll;
				flag = portal->fetchGeometry(cell); 
				cll = new TeCell();
				if(remap)
				{
					TeCell gout;
					TeVectorRemap (cell, projAct, gout, projVis);
					*cll = gout;
				}
				else
					*cll = cell;
				vetGeom.push_back(cll);
			}
		} while (flag);
	}
	catch(...)
	{
		TeSelectedGeom::iterator it;
		for(it = vetGeom.begin(); it != vetGeom.end(); it++)
			delete (*it);
		return false;
	}

	return true;
}

//geom: selected geometries
//obj: portal geometries
//basic idea: obj "relate" geom
bool
fillResultIdsVector(TeGeometry *geom, TeDatabasePortal *portal, TeGeomRep portalRep, int relate, TeKeys& resultIds)
{
	bool flag = false;
	bool result = false;
	TeGeomRep geomRep = geom->elemType();
	bool isSet = false;
	int setSize = 1;

	if ( (dynamic_cast<TePolygonSet*> (geom)) ||
		 (dynamic_cast<TeLineSet*> (geom)) ||
		 (dynamic_cast<TePointSet*> (geom)) ||
		 (dynamic_cast<TeCellSet*> (geom)) )
	{
		isSet = true;
		setSize = geom->size();
	}

	try
	{
		do
		{
			TeGeometry* obj = 0;
			if(portalRep == TePOLYGONS)
			{
				TePolygon   poly, *pol;
				flag = portal->fetchGeometry (poly);
				pol = new TePolygon();
				*pol = poly;
				obj = pol;
			}
			else if(portalRep == TeLINES)
			{
				TeLine2D	line, *lin;
				flag = portal->fetchGeometry (line);
				lin = new TeLine2D();
				*lin = line;
				obj = lin;
			}
			else if(portalRep == TePOINTS)
			{
				TePoint		point, *pnt;
				flag = portal->fetchGeometry (point);
				pnt = new TePoint();
				*pnt = point;
				obj = pnt;
			}
			else if(portalRep == TeCELLS)
			{
				TeCell		cell, *cll;
				flag = portal->fetchGeometry (cell);
				cll = new TeCell();
				*cll = cell;
				obj = cll;
			}

			result = false;

			//for each geometry 	
			for(int i=0; i<setSize; ++i)
			{
				TeGeometry* geomTemp = 0;
				if(isSet)
				{
					if(geomRep==TePOLYGONS)
						geomTemp = &(((TePolygonSet*)geom)->operator[](i));
					else if (geomRep==TeLINES)
						geomTemp = &(((TeLineSet*)geom)->operator[](i));
					else if (geomRep==TePOINTS)
						geomTemp = &(((TePointSet*)geom)->operator[](i));
					else if (geomRep==TeCELLS)
						geomTemp = &(((TeCellSet*)geom)->operator[](i));
				}
				else
					geomTemp = geom;
				
				switch (geomRep | portalRep)   //geom: selected and obj: portal
				{
						
					case 1: //polygon with polygon
						result = TeTopologicalRelation((TePolygon*)obj, (TePolygon*)geomTemp, relate);
					break;

					case 2://line with line 
						result = TeTopologicalRelation((TeLine2D*)obj, (TeLine2D*)geomTemp, relate);
					break;
			
					case 3://polygon with line
						if(geomRep == TePOLYGONS)
							result = TeTopologicalRelation((TePolygon*)geomTemp, (TeLine2D*)obj, relate);
						else
							result = TeTopologicalRelation((TePolygon*)obj, (TeLine2D*)geomTemp, relate);
					break;
				
					case 4://point with point
						result = TeTopologicalRelation((TePoint*)obj, (TePoint*)geomTemp, relate);
					break;
				
					case 5://polygon with point
						if(geomRep == TePOLYGONS)
							result = TeTopologicalRelation((TePolygon*)geomTemp, (TePoint*)obj, relate);
						else
							result = TeTopologicalRelation((TePolygon*)obj, (TePoint*)geomTemp, relate);
					break;

					case 6://line with point 
						if(geomRep == TeLINES)
							result = TeTopologicalRelation((TeLine2D*)geomTemp, (TePoint*)obj, relate);
						else
							result = TeTopologicalRelation((TeLine2D*)obj, (TePoint*)geomTemp, relate);
					break;
					
					case 256://cell with cell 
						result = TeTopologicalRelation((TeCell*)obj, (TeCell*)geomTemp, relate);
					break;

					case 257://cell with polygon 
						if(geomRep == TePOLYGONS)
							result = TeTopologicalRelation((TeCell*)obj, (TePolygon*)geomTemp, relate);
						else
							result = TeTopologicalRelation((TeCell*)geomTemp, (TePolygon*)obj, relate);
					break;
					
					case 258://cell with line 
						if(geomRep == TeLINES)
							result = TeTopologicalRelation((TeCell*)obj,  (TeLine2D*)geomTemp, relate);
						else
							result = TeTopologicalRelation((TeCell*)geomTemp, (TeLine2D*)obj, relate);
					break;

					case 260://cell with point
						if(geomRep == TeCELLS)
							result = TeTopologicalRelation((TeCell*)geomTemp, (TePoint*)obj, relate);
						else
							result = TeTopologicalRelation((TeCell*)obj,  (TePoint*)geomTemp, relate);
					break;
				}
						
				if(result)
				{
					string objId = obj->objectId();
					resultIds.push_back(objId);
				}
			}//for

			delete obj;
		} while (flag); //fetchrow
	}
	catch(...)
	{
		return false;
	}
	return true;
}


bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   TeDatabasePortal *portal, int relation, const string& actColTable)
{
	TeKeys  resultIds;

	if(!TeTopologicalRelation(actGeomTable, actRep, actIdsIn, resultIds, portal->getDatabase(), relation, actColTable))
		return false;

	string Ids = getStringIds(resultIds);
	
	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";
	
	portal->freeResult();

	if(!portal->query(sql) || !portal->fetchRow())
		return false;
	
	return true;
}

bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   const string& visGeomTable,  TeGeomRep visRep, TeDatabasePortal *portal, 
						   int relation, const string& visColTable)
{
	TeKeys  resultIds;

	if(!TeTopologicalRelation(actGeomTable, actRep, actIdsIn, visGeomTable, visRep, resultIds, portal->getDatabase(), relation, visColTable))
		return false;
	
	string Ids = getStringIds(resultIds);

	portal->freeResult ();

	string sql = "SELECT * FROM " + visGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";
	if(visRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";

	if(!portal->query(sql) || !portal->fetchRow())
		return false;
		
	return true;
}

bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
						   TeDatabasePortal *portal, int relation, const string& actCollTable)
{
	
	TeKeys	resultIds;
	
	if(!TeTopologicalRelation(actGeomTable, actRep, geom, resultIds, portal->getDatabase(), relation, actCollTable))
		return false;
	
	string Ids = getStringIds(resultIds);

	portal->freeResult();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";

	if(!portal->query(sql) || !portal->fetchRow())
		return false;
	
	return true;
}
	
bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   TeKeys& actIdsOut, TeDatabase* db, int relation, const string& actCollTable)
{
	TeSelectedGeom	vetGeom;
			
	string Ids = getStringIds(actIdsIn);

	TeDatabasePortal* portal = db->getPortal();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	
	//order by clause
	string sqlOrderBy = " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sqlOrderBy += ", parent_id ASC, num_holes DESC ";

	sql += sqlOrderBy;
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	if(!fillSelectedGeomVector(actRep,vetGeom,portal))
	{
		delete portal;
		return false; 
	}

	//iterators
	TeSelectedGeom::iterator it;
	actIdsOut.clear();
	double prec = TePrecision::instance().precision();
	
	for (it = vetGeom.begin(); it != vetGeom.end(); it++)
	{
		portal->freeResult();
		TeBox box = (**it).box();

		TeBox temp (box.x1()-prec, box.y1()-prec, box.x2()+prec, box.y2()+prec); 
		
		sql = mountSelect(actGeomTable, actCollTable, actRep, temp, relation, portal);
		if(relation != TeEQUALS)
			sql += " AND object_id NOT IN (" + Ids + ")";
		sql += sqlOrderBy;

		if(!portal->query(sql)) 
		{
			delete portal;
			return false;
		}

		if(!portal->fetchRow())
			continue;

		fillResultIdsVector((*it), portal, actRep, relation, actIdsOut);
	
	} 
		
	sort(actIdsOut.begin(), actIdsOut.end());
	unique(actIdsOut.begin(), actIdsOut.end());

	delete portal;
	
	for (it = vetGeom.begin(); it != vetGeom.end(); it++)
		delete (*it);

	return true;
}

bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   const string& visGeomTable, TeGeomRep visRep, TeKeys& visIdsOut, 
						   TeDatabase* db, int relation, const string& visCollTable, TeDatabase* dbVis)
{
	TeSelectedGeom	vetGeom;
			
	string Ids = getStringIds(actIdsIn);
	if(dbVis == 0)
		dbVis = db;

	//verify projection
	TeProjection* projAct = getProjectionFromGeomTable(actGeomTable, db);
	TeProjection* projVis = getProjectionFromGeomTable(visGeomTable, dbVis);
	
	TeDatabasePortal* portal = db->getPortal();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql)|| !portal->fetchRow ())
	{
		delete portal;
		return false;
	}
	
	if(!fillSelectedGeomVector(actRep,vetGeom,portal,projAct, projVis))
	{
		delete portal;
		return false; 
	}

	if(db != dbVis)
	{
		delete portal;
		portal = dbVis->getPortal();
	}

	//iterators
	TeSelectedGeom::iterator it;
	visIdsOut.clear();
	double prec = TePrecision::instance().precision();

	for (it = vetGeom.begin(); it != vetGeom.end(); it++)
	{
		portal->freeResult();
		TeBox box = (**it).box();
		
		TeBox temp (box.x1()-prec, box.y1()-prec, box.x2()+prec, box.y2()+prec); 
		
		sql = mountSelect(visGeomTable, visCollTable, visRep, temp, relation, portal);
		sql += " ORDER BY object_id ASC ";
		if(visRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
			sql += ", parent_id ASC, num_holes DESC ";

		if(!portal->query(sql)) 
		{
			delete portal;
			return false;
		}

		if(!portal->fetchRow())
			continue;
	
		fillResultIdsVector((*it), portal, visRep, relation, visIdsOut);

	}
						
	delete portal;
	
	sort(visIdsOut.begin(), visIdsOut.end());
	unique(visIdsOut.begin(), visIdsOut.end());

	for (it = vetGeom.begin(); it != vetGeom.end(); it++)
		delete (*it);

	return true;
}

bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
						   TeKeys& actIdsOut, TeDatabase* db, int relation, const string& actCollTable)
{
	TeDatabasePortal* portal = db->getPortal();
	TeBox box = geom->box();

	double prec = TePrecision::instance().precision();
	TeBox temp (box.x1()-prec, box.y1()-prec, box.x2()+prec, box.y2()+prec); 

	string sql = mountSelect(actGeomTable, actCollTable, actRep, temp, relation, portal);
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";

	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	actIdsOut.clear();
	
	fillResultIdsVector(geom, portal, actRep, relation, actIdsOut);

	delete portal;
	
	sort(actIdsOut.begin(), actIdsOut.end());
	unique(actIdsOut.begin(), actIdsOut.end());

	return true;
}

bool TeGetWithinDistance(const string& actGeomTable, TeGeomRep actRep, const TeCoord2D& point, TeKeysToDist& IdsDistOut,
						 TeDatabase* db, const double& max_dist, const string& actCollTable)
{
	if(!db)
		return false;
	
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	TeBox box (point.x()-max_dist, point.y()-max_dist,point.x()+max_dist, point.y()+max_dist); 

	string sql = mountSelect(actGeomTable, actCollTable, actRep, box, TeWITHIN, portal);
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";

	if(!portal->query(sql)) 
	{
		delete portal;
		return false;
	}

	if(!portal->fetchRow())
	{
		delete portal;
		return true;
	}

	bool flag = true;
	do
	{
		TeCoord2D centroid2;
		string objId;
		if(actRep == TePOLYGONS)
		{
			TePolygon   poly;
			flag = portal->fetchGeometry (poly);
			objId = poly.objectId ();
			centroid2 = TeFindCentroid(poly);
		}
		else if(actRep == TeLINES)
		{
			TeLine2D	line;
			flag = portal->fetchGeometry (line);
			objId = line.objectId ();
			centroid2 = TeFindCentroid(line);
		}
		else if(actRep == TePOINTS)
		{
			TePoint		point;
			flag = portal->fetchGeometry (point);
			objId = point.objectId ();
			centroid2 = TeFindCentroid(point);
		}
		else if(actRep == TeCELLS)
		{
			TeCell		cell;
			flag = portal->fetchGeometry (cell);
			objId = cell.objectId ();
			centroid2 = TeFindCentroid(cell);
		}
		
		double dist = TeDistance(point, centroid2);
		if(dist<=max_dist)
			IdsDistOut[objId] = dist;

	} while (flag); //fetchrow
	
	//sort(IdsDistOut.begin(), IdsDistOut.end());

	delete portal;
	return true;
}

bool TeGetArea(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeDatabase* db, double& area)
{
	if(actRep != TePOLYGONS && actRep != TeCELLS )
		return false;

	string Ids = getStringIds(actIdsIn);
	
	TeDatabasePortal* portal = db->getPortal();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";

	if((actRep == TePOLYGONS) && (portal->getDatabase()->dbmsName() != "PostGIS"))
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = false;
	double sumArea = 0.;
	do
	{
		if(actRep == TePOLYGONS)
		{
			TePolygon poly;
			flag = portal->fetchGeometry(poly);
			sumArea+= TeGeometryArea(poly);
		}
		else if(actRep == TeCELLS)
		{
			TeCell cell;
			flag = portal->fetchGeometry(cell);
			sumArea+= TeGeometryArea(cell.box());
		}
	}while (flag);
	
	area = sumArea;
	delete portal;
	return true;
}


bool TeGetLength(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeDatabase* db, double& length)
{
	if((actRep != TePOLYGONS) && (actRep != TeLINES) && (actRep != TeCELLS) )
		return false;

	string Ids = getStringIds(actIdsIn);
	
	TeDatabasePortal* portal = db->getPortal();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";

	if((actRep == TePOLYGONS) && (portal->getDatabase()->dbmsName() != "PostGIS"))
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = false;
	double sumLen = 0.;
	do
	{
		if(actRep == TePOLYGONS)
		{
			TePolygon p;
			flag = portal->fetchGeometry(p);
			
			for(unsigned int i=0; i<p.size(); i++)
				sumLen += TeLength(p[i]);
		}
		else if (actRep == TeLINES)
		{
			TeLine2D l;
			flag = portal->fetchGeometry(l);
			sumLen += TeLength(l);
		}
		else if (actRep == TeCELLS)
		{
			TeCell c;
			flag = portal->fetchGeometry(c);

			TePolygon p = TeMakePolygon(c.box());
			sumLen += TeLength(p[0]);
		}
		
	}while (flag);
	
	length = sumLen;
	delete portal;
	return true;
}


bool TeGetDistance(const string& actGeomTable, TeGeomRep actRep, TeKeys& IdsIn, TeDatabase* db, double& distance)
{
	string Ids = getStringIds(IdsIn);
	
	TeDatabasePortal* portal = db->getPortal();

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN (" + Ids + ")";
	sql += " ORDER BY object_id ASC ";

	if((actRep == TePOLYGONS) && (portal->getDatabase()->dbmsName() != "PostGIS"))
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;  
	}
	
	bool flag = false;
	string lastObjId ="";
	TeCoord2D coord1, coord2; 
	int step = 0;
	do
	{
		TeCoord2D coord;
		string objId;
		
		if(actRep == TePOLYGONS)
		{
			TePolygon p;
			flag = portal->fetchGeometry(p);
			objId = p.objectId();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(p);
		}
		else if (actRep == TeLINES)
		{
			TeLine2D l;
			flag = portal->fetchGeometry(l);
			objId = l.objectId ();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(l);
		}
		else if (actRep == TePOINTS)
		{
			TePoint p;
			flag = portal->fetchGeometry(p);
			coord = p.location();
		}
		else if (actRep == TeCELLS)
		{
			TeCell c;
			flag = portal->fetchGeometry(c);
			objId = c.objectId ();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(c);
		}
		
		if(step==0)
			coord1 = coord;
		else
			coord2 = coord;

		lastObjId = objId;
		++step;

	}while (flag);
	
	distance = TeDistance(coord1, coord2);
	delete portal;
	return true;
}

bool TeGetConvexHull(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, TePolygonSet& convexHullSet)
{
	if(actRep != TePOLYGONS)
		return false;

	TeDatabasePortal* portal = db->getPortal();
	string Ids = getStringIds(actIds);

	string sql = "SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN ("+ Ids +")";
	sql += " ORDER BY object_id ASC ";

	if(portal->getDatabase()->dbmsName() != "PostGIS")
		sql += ", parent_id ASC, num_holes DESC ";
		
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do
	{
		TePolygon   poly, result;
		flag = portal->fetchGeometry(poly);
		TePolygon pol = TeConvexHull(poly);
		result.add(pol[0]);
		convexHullSet.add(result);
	} while (flag);
	
	delete portal;
	return true;
}

bool TeGetCentroid(const string& actGeomTable, TeGeomRep actRep, TeDatabase* db, TePointSet& centroidSet, TeKeys& actIds, const string& actCollTable)
{
	TeDatabasePortal* portal = db->getPortal();
	string Ids = getStringIds(actIds);

	string select = " SELECT * "; 
	string from  =  " FROM " + actGeomTable;
	string where =  " WHERE 1=1 ";

	if(!actCollTable.empty())
	{
		select += ", "+ actCollTable;
		where += " AND object_id = c_object_id ";
	}

	//if empty it calculates the centroids to all geometries 
	if(!Ids.empty())
		where += " AND object_id IN ("+ Ids +")";
			
	string orderBy = " ORDER BY object_id ASC ";
	
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")
		orderBy += ", parent_id ASC, num_holes DESC ";
	
	// ---- progress bar 
	int numSteps = 0;
	string sqlProg = " SELECT COUNT(*) "+ from + where; 
	if(portal->query(sqlProg) && portal->fetchRow())
		numSteps = portal->getInt(0);
	
	portal->freeResult();

	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(numSteps);
	//------
	
	string sql = select + from + where + orderBy;
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	string lastObjId ="";
	int step = 0;
	do
	{
		TeCoord2D coord;
		string objId;
		
		if(actRep == TePOLYGONS)
		{
			TePolygon p;
			flag = portal->fetchGeometry(p);
			objId = p.objectId();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(p);
		}
		else if (actRep == TeLINES)
		{
			TeLine2D l;
			flag = portal->fetchGeometry(l);
			objId = l.objectId ();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(l);
		}
		else if (actRep == TeCELLS)
		{
			TeCell c;
			flag = portal->fetchGeometry(c);
			objId = c.objectId ();
			if(lastObjId==objId)
				continue;
			coord = TeFindCentroid(c);
		}
		
		lastObjId = objId;
		TePoint point(coord);
		point.objectId (objId);
		centroidSet.add(point);

		if(TeProgress::instance())
		{
			if (TeProgress::instance()->wasCancelled())
			{
				TeProgress::instance()->reset();
				return false;
			}
			else
				TeProgress::instance()->setProgress(step);
		}	
		++step;

	} while (flag);
	
	if (TeProgress::instance())
		TeProgress::instance()->reset();

	delete portal;
	return true;
}


bool TeGetBuffer(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, TePolygonSet& bufferSet, double dist)
{
	
	if(actIds.empty())
		return false;
	
	string Ids = getStringIds(actIds);

	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;
	
	string sql = " SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN ("+ Ids +")";
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	bool res = true;
	do
	{
		TePolygonSet polSet;

		if(actRep == TePOINTS)
		{
			TePoint p;
			TePolygon pol;
			flag = portal->fetchGeometry(p);
			res = TeBUFFERREGION::TeBufferRegion(p.location(), dist, 16, pol);
			polSet.add (pol);
		}

		else if(actRep == TeLINES)
		{
			TeLine2D l;
			flag = portal->fetchGeometry(l);
			res = TeBUFFERREGION::TeBufferRegion(l, dist, 8, polSet);
		}

		else if(actRep == TePOLYGONS)
		{
			TePolygon p;
			flag = portal->fetchGeometry(p);
			res = TeBUFFERREGION::TeBufferRegion(p, dist, 8, polSet);
		}	

		else if(actRep == TeCELLS)
		{
			TeCell c;
			flag = portal->fetchGeometry(c);
			res = TeBUFFERREGION::TeBufferRegion(TeMakePolygon(c.box()), dist, 8, polSet);
		}	
		
		if(res)
		{
			for(unsigned int i=0; i<polSet.size(); i++)
				bufferSet.add(polSet[i]);
		}
	
	} while (flag && res);

	delete portal;
	return true;
}


bool TeGetOverlay(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, TeGeometryVect& geomVect, const short& operation)
{
	if((actIds.empty()) || (actRep!=TePOLYGONS && actRep!=TeCELLS))
		return false;
	
	string Ids = getStringIds(actIds);

	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	string sql = " SELECT * FROM " + actGeomTable;
	sql += " WHERE object_id IN ("+ Ids +")";
	sql += " ORDER BY object_id ASC ";
	if(actRep == TePOLYGONS && portal->getDatabase()->dbmsName() != "PostGIS")	
		sql += ", parent_id ASC, num_holes DESC ";
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
		
	bool flag = true;
	TePolygonSet polSet1;
	
	if(actRep == TePOLYGONS)
	{
		TePolygon p;
		flag = portal->fetchGeometry(p);
		polSet1.add (p);
	}
	else if(actRep == TeCELLS)
	{
		TeCell c; 
		flag = portal->fetchGeometry(c);
		polSet1.add(TeMakePolygon(c.box()));
	}

	while (flag)
	{
		TePolygonSet polSet2, polInter;

		if(actRep == TePOLYGONS)
		{
			TePolygon p;
			flag = portal->fetchGeometry(p);
			polSet2.add (p);
		}	

		else if(actRep == TeCELLS)
		{
			TeCell c;
			flag = portal->fetchGeometry(c);
			polSet2.add(TeMakePolygon(c.box()));
		}
		
		if(operation==TeUNION)
		{
			if(!TeOVERLAY::TeUnion(polSet1, polSet2, polInter))
				return false;
		}
		else if (operation==TeINTERSECTION)
		{
			if(!TeOVERLAY::TeIntersection(polSet1, polSet2, polInter))
				return false;
		}
		else if (operation==TeDIFFERENCE)
		{
			if(!TeOVERLAY::TeDifference(polSet1, polSet2, polInter))
				return false;
		}

		polSet1 = polInter;
	}

	TeGeometry* geom;
	TePolygonSet* pol = new TePolygonSet();
	*pol = polSet1;
	geom = pol;
	
	geomVect.push_back (geom);
	delete portal;
	return true;
}


TeRaster*
TeMask(TeRaster* whole, TePolygon& poly, TeStrategicIterator st)
{
	if (!whole || (whole->params().status_ != TeRasterParams::TeReadyToRead && 
		whole->params().status_ != TeRasterParams::TeReadyToWrite))
		return 0;

	if (!TeIntersects(poly.box(),whole->params().boundingBox()))
		return 0;
	
	TeRasterParams par = whole->params();
	par.decoderIdentifier_ = "MEM";
	par.mode_ = 'c';
	par.setDummy(0);

	TeCoord2D bll = whole->coord2Index(poly.box().lowerLeft());
	TeCoord2D bur = whole->coord2Index(poly.box().upperRight());

	bll = whole->index2Coord(TeCoord2D(TeRoundRasterIndex(bll.x_),TeRoundRasterIndex(bll.y_)));
	bur = whole->index2Coord(TeCoord2D(TeRoundRasterIndex(bur.x_),TeRoundRasterIndex(bur.y_)));

	par.boxResolution(bll.x_, bll.y_, bur.x_, bur.y_,par.resx_, par.resy_);
	par.setDummy(0.0);
	par.useDummy_ = true;

	TeRaster*  clip = new TeRaster(par);
	clip->init();
	if (clip->params().status_ != TeRasterParams::TeReadyToWrite)
		return 0;

	TeCoord2D cd = TeCoord2D(0,0);
	TeCoord2D ul = clip->params().index2Coord(cd);
	TeCoord2D dxdy = whole->params().coord2Index(ul);
	int j = TeRoundRasterIndex(dxdy.x_); // column delta
	int i = TeRoundRasterIndex(dxdy.y_); // line delta

	TeRaster::iteratorPoly it = whole->begin(poly, st);
//	int nlines = it.nLinesInPoly();
//	int ncols = it.nColsInPoly();
	while(!it.end())
    {
		int lin = it.currentLine();
		int col = it.currentColumn();
		int nbands = it.nBands();
		vector<double> vals;
		vals = (*(TeRaster::iterator)it);
		for (int b=0; b<nbands; b++)
			clip->setElement(col-j,lin-i,vals[b],b);
		++it;
	}
	return clip;
}





