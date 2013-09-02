#include "TeCellAlgorithms.h"
#include "TeGeometryAlgorithms.h"
#include <TeTheme.h>
#include <TeDatabase.h>
#include <TeLayer.h>
#include <TeDatabase.h>

TeLayer* 
TeCreateCells(const string& layerName, TeDatabase* db, TeProjection* proj, TeBox& bb, double resX, double resY)
{
	TeDatabasePortal* portal = db->getPortal();
	if (!portal)
		return 0;

	TeBox newBox = adjustToCut(bb,resX,resY);
	double x,y,x1,x2,y1,y2;
	x1 = newBox.lowerLeft().x();
	y1 = newBox.lowerLeft().y();
	x2 = newBox.upperRight().x();
	y2 = newBox.upperRight().y();

	int maxcols, maxlines;
	maxcols = int((y2-y1)/resY);
	maxlines = int((x2-x1)/resX);

	TeAttribute attribute;
	TeAttributeList attList;
	TeAttributeList keyList;
	attribute.rep_.name_ = "object_id";
	attribute.rep_.type_ = TeSTRING;
	attribute.rep_.numChar_ = 48;
	attList.push_back ( attribute );
	keyList.push_back(attribute);
	attribute.rep_.name_ = "Col";
	attribute.rep_.type_ = TeINT;
	attList.push_back ( attribute );
	attribute.rep_.name_ = "Lin";
	attribute.rep_.type_ = TeINT;
	attList.push_back ( attribute );	

	TeLayer* newLayer = new TeLayer(layerName,db,newBox,proj);
	newLayer->addGeometry(TeCELLS);
	
	TeTable attTable (layerName,attList,TeAttrStatic,0);
	newLayer->createAttributeTable(attTable,keyList);

	TeCellSet cells;
	cells.resX(resX);
	cells.resY(resY);

	int col,lin,ncels=0;
	bool status;
	for (y=y2-resY,lin=0; y>=y1; y-=resY,lin++)
	{	
		for (x=x1,col=0; x<x2; x+=resX,col++)
		{
			TeBox box(x-resX/2.,y-resY/2.,x+resX/2.,y+resY/2.);
			TeCell cell;
			TeTableRow row;

			// Build cell Id
			char celId[32];
			sprintf(celId,"C%02dL%02d",col,lin);
			cell.setBox (box);
			cell.objectId(string(celId));
			cell.column(col);
			cell.line(lin);
			cells.add(cell);
			row.push_back(cell.objectId()); 
			row.push_back(Te2String(col)); 
			row.push_back(Te2String(lin)); 
			attTable.add(row);
			ncels++;
		}
		if (attTable.size() > 0)	// if there is some attributes in this line
		{
			status = newLayer->saveAttributeTable(attTable);
			attTable.clear();
			status = newLayer->addCells(cells);
			cells.clear();
		}
	}
	if (ncels > 0)
		return newLayer;
	else
		return 0;
}

	
TeLayer*
TeCreateCells(const string& layerName,TeTheme* theme, double resX, double resY)
{
	TeLayer* inputLayer = theme->layer();
	TeRepresentation* pp = inputLayer->getRepresentation(TePOLYGONS);
	if (!pp)
		return 0;

	TeDatabase* db = inputLayer->database();
	TeDatabasePortal* portal = db->getPortal();
	if (!portal)
		return 0;

	TeBox newBox = adjustToCut(pp->box_,resX,resY);
	double x,y,x1,x2,y1,y2;
	x1 = newBox.lowerLeft().x();
	y1 = newBox.lowerLeft().y();
	x2 = newBox.upperRight().x();
	y2 = newBox.upperRight().y();

	int maxcols, maxlines;
	maxcols = int((y2-y1)/resY);
	maxlines = int((x2-x1)/resX);

	TeAttribute attribute;
	TeAttributeList attList;
	TeAttributeList keyList;
	attribute.rep_.name_ = "object_id";
	attribute.rep_.type_ = TeSTRING;
	attribute.rep_.numChar_ = 48;
	attList.push_back ( attribute );
	keyList.push_back(attribute);
	attribute.rep_.name_ = "Col";
	attribute.rep_.type_ = TeINT;
	attList.push_back ( attribute );
	attribute.rep_.name_ = "Lin";
	attribute.rep_.type_ = TeINT;
	attList.push_back ( attribute );	

	TeLayer* newLayer = new TeLayer(layerName,db,newBox,inputLayer->projection());
	newLayer->addGeometry(TeCELLS);
	
	TeTable attTable (layerName,attList,TeAttrStatic,0);
	newLayer->createAttributeTable(attTable,keyList);

	string polTableName = inputLayer->tableName(TePOLYGONS);

	TeCellSet cells;
	cells.resX(resX);
	cells.resY(resY);

	TePolygon poly;
	int col,lin;
	TePolygonSet ps;
	bool status;
	int ncels = 0;

	for (y=y2-resY,lin=0; y>=y1; y-=resY,lin++)
	{	
		for (x=x1,col=0; x<x2; x+=resX,col++)
		{
			TeBox box(x-resX/2.,y-resY/2.,x+resX/2.,y+resY/2.);
			if (TeIntersect(box,ps.box()) || db->loadPolygonSet(polTableName,box,ps))
			{
				for (int i=0; i<(int)ps.size(); i++)
				{
					if (theme->isInCollection(ps[i].objectId()))
					{
						TeCell cell;
						TeTableRow row;

						// Build cell Id
						char celId[32];
						sprintf(celId,"C%02dL%02d",col,lin);
						cell.setBox (box);
						cell.objectId(string(celId));
						cell.column(col);
						cell.line(lin);
						cells.add(cell);
						row.push_back(cell.objectId()); 
						row.push_back(Te2String(col)); 
						row.push_back(Te2String(lin)); 
						attTable.add(row);
						ncels++;
						break;
					}
				}
			}
			ps.clear();
		}
		if (attTable.size() > 0)	// if there is some attributes in this line
		{
			status = newLayer->saveAttributeTable(attTable);
			attTable.clear();
			status = newLayer->addCells(cells);
			cells.clear();
		}
	}
	if (ncels > 0)
		return newLayer;
	else
		return 0;
}


bool 
TeStatisticsDB(const string& tableGeomCell, const string& tableCollCell, const string& tableAttrCell, TeStatisticsMap& stat, 
			   const string& tableGeomIn, const string& tableCollIn, TeGeomRep repIn, const string& tableAttrIn, const string& attrIn, TeDatabase* db)
{
	
	TeStatisticsMap notGet;

	//Calculate the statistics from database functions
	string query = db->getStatisticsSQL (tableGeomCell, tableCollCell, stat, tableGeomIn, tableCollIn, repIn, tableAttrIn, attrIn, notGet);
	
	TeDatabasePortal* portal = db->getPortal();

	if(!portal->query(query))
	{
		delete portal;
		return false;
	}
	
	if(!stat.empty())
	{
		//Keep statistics in the tableAttrCell table   
		if(!TeKeepStatistics(tableAttrCell, stat, portal))
		{
			delete portal;
			return false;
		}
	}

	portal->freeResult();

	// notGet: statistics don´t calculated from database functions
	if(notGet.empty())
	{
		delete portal;
		return true;
	}

	//Calculate the statistics don´t calculated from database functions
	string sel = "SELECT "+ tableGeomCell +".object_id, "+ tableAttrIn +"."+ attrIn;
	sel += " FROM " + tableGeomCell +","+ tableGeomIn +","+ tableAttrIn;
	
	string sqlWhere ="";
	if(!tableCollIn.empty())
	{
		sel += ", "+ tableCollIn;
		sqlWhere = " AND "+ tableGeomIn +".object_id = "+ tableCollIn +".c_object_id ";
	}
	if(!tableCollCell.empty())
	{
		sel += ", "+ tableCollCell;
		sqlWhere += " AND "+ tableGeomCell +".object_id = "+ tableCollCell +".c_object_id ";
	}

	sel += " WHERE "+ tableAttrIn +".object_id = "+ tableGeomIn +".object_id ";
	
	if(!sqlWhere.empty())
		sel += sqlWhere; 

	sel += "AND " + db->getWhereClause (tableGeomCell, tableGeomIn, repIn, TeCELLS);
	sel += " ORDER BY "+ tableGeomCell +".object_id";

	if(!portal->query (sel))
	{
		delete portal;
		return false;
	}

	//to mount the TeSelectedObjectMap from of the portal  
	TeSelectedObjectMap		selObjMap;
	
	TePropertyVector prVector;
	string lastObj = "";
	string obj = "";
	
	int cont=0;
	while(portal->fetchRow())
	{
		obj = string(portal->getData(0));
		if(!cont)
			lastObj = obj;

		if (obj == lastObj)
		{
			string val = portal->getData(1);
			
			TeProperty  pr;
			pr.attr_.rep_.name_ = attrIn;
			pr.value_ = val;
			prVector.push_back(pr);
		}
		
		else
		{
			TeSelectedObject	selObj;
			selObj.properties_ = prVector;
			selObjMap[lastObj] = selObj;
			
			prVector.erase (prVector.begin(), prVector.end());

			//to catch the next value
			string val = portal->getData(1);
			TeProperty  pr;
			pr.attr_.rep_.name_ = attrIn;
			pr.value_ = val;
			prVector.push_back(pr);
		}

		lastObj = obj;
		cont++;
	}

	TeSelectedObject	selObj;
	selObj.properties_ = prVector;
	selObjMap[lastObj] = selObj;
		
	TeObjStatisticsMap objStats;
	
	//Calculate the statistics in memory
	if(!TeStatisticsMem(selObjMap, objStats))
	{
		delete portal;
		return false;
	}

	//Keep statistics claculated in memory in the tableAttrCell table
	if(!TeKeepStatistics(tableAttrCell, notGet, db, objStats))
	{
		delete portal;
		return false;
	}
	
	delete portal;
	return true;
}
	
	
bool 
TeStatisticsMem(TeSelectedObjectMap& selObjMap, TeObjStatisticsMap& result)
{
		
	TeSelectedObjectMap::iterator itMap = selObjMap.begin();
	
	double sum, mean, var, dev, val, minval, maxval;
	int count=0;

	while(itMap!= selObjMap.end())
	{
		string				objId;
		TeSelectedObject	obj;
		TePropertyVector	prVector;
		TePropertyVector::iterator itPr;
		
		obj = (*itMap).second;
		objId = (*itMap).first;
		prVector = obj.properties_;
		itPr = prVector.begin();
		count = prVector.size();
		
		sum=mean=var=dev=0.0;
		minval= TeMINFLOAT;
		maxval= TeMAXFLOAT;

		int c=0;
		while(itPr != prVector.end())
		{
			val = atof((*itPr).value_.c_str()); 
			
			if(!c)
			{
				minval = val;
				maxval = val;
			}

			if(val<minval)
				minval=val;
			if(val>maxval)
				maxval=val;

			sum += val;
			itPr++;
			c++;
		}

		if(count!=0)
			mean=sum/count;
			
		itPr = prVector.begin();
		while(itPr!=prVector.end())
		{
			val = atof((*itPr).value_.c_str()); 
			var += pow(val - mean, 2);
			itPr++;
		}

		if(mean!=0)
			var = var/count;
		
		dev = sqrt(var);

		TeStatisticValMap statMap;
		statMap[TeCOUNT] = count;
		statMap[TeSUM] = sum;
		statMap[TeMAXVALUE] = maxval;
		statMap[TeMINVALUE] = minval;
		statMap[TeSTANDARDDEVIATION] = dev;
		statMap[TeMEAN] = mean;
		statMap[TeVARIANCE] = var;

		result[objId] = statMap;
		itMap++;
	}

	return true;
}


bool	
TeKeepStatistics(const string& tableAttrCell, TeStatisticsMap& stat, TeDatabase* db, TeObjStatisticsMap& selObjStat)
{
	
	//Add columns in the table if not exist
	TeStatisticsMap::iterator it = stat.begin();
	if(it == stat.end())
		return false;
	
	TeAttributeRepList	att;
	
	while(it != stat.end())
	{	
		TeAttributeRep	attrRep;
		attrRep.name_ = (*it).second;
		attrRep.type_ = TeREAL;
		att.push_back (attrRep);
		it++;
		
		if(!db->columnExist(tableAttrCell, attrRep.name_))
			db->addColumn (tableAttrCell, attrRep);
	}
	
	//Mount the update sql from TeObjStatisticsMap 
	TeObjStatisticsMap::iterator itt = selObjStat.begin();
	while(itt!= selObjStat.end())
	{
		string update = "UPDATE "+ tableAttrCell +" SET ";
		
		TeStatisticValMap statVal;
		statVal = (*itt).second;
		TeStatisticValMap::iterator itStVal = statVal.begin();
		int cont=0; 
		while(itStVal!=statVal.end())
		{
			TeStatisticType stType = (*itStVal).first;
			double stVal = (*itStVal).second;
			string colName = stat[stType];
			if(!colName.empty())
			{
				if(cont)
					update += ",";
				update += colName +" = "+ Te2String(stVal);
				cont++;
			}
			itStVal++;
		}

		update += " WHERE object_id = '"+ (*itt).first +"'";
		
		if(!db->execute(update))
			return false;
		
		itt++;
	}

	return true;
}
	
	
bool	
TeKeepStatistics(const string& tableAttrCell, TeStatisticsMap& stat, TeDatabasePortal* portal)
{
	
	//Add columns in the table if not exist
	TeStatisticsMap::iterator it = stat.begin();
	if(it == stat.end())
		return false;
	
	TeAttributeRepList	attrs;
	int cont = 0;
	while(it != stat.end())
	{	
		TeAttributeRep	attrRep;
		attrRep.name_ = (*it).second;
		attrRep.type_ = TeREAL;
		attrs.push_back (attrRep);
		it++;
		cont++;

		if(!portal->getDatabase()->columnExist(tableAttrCell, attrRep.name_))
			portal->getDatabase()->addColumn (tableAttrCell, attrRep);
	}
	
	//Mount the update sql from portal
	string update1 = "UPDATE "+ tableAttrCell +" SET ";
	string update2;
	while(portal->fetchRow())
	{
		update2 = "";
		string sql = ""; 
		
		for(int i=0; i<cont; i++)
		{
			if(i)
				update2 += ", ";
			update2 += attrs[i].name_ +" = '"+ portal->getData(i+1) +"'";
		}
		
		update2 += " WHERE object_id = '" + string(portal->getData(0)) + "'"; 

		sql = update1 + update2;

		if(!portal->getDatabase()->execute(sql))
			return false;
	}
	return true;
}
	
