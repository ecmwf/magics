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

#include "TeLayer.h"
#include "TeUtils.h"
#include "TeException.h"
#include "TeDatabase.h"
#include "TeRaster.h"
#include "TeProjection.h"


TeLayer::TeLayer(const string& name, TeDatabase* db, TeProjection* proj): 
	name_(name),
	id_(-1),
	db_(db),
	projection_ ( proj ),
	raster_(0)
{
	if (!db_ || name.empty())
		return;

	// verifies if layer already exists in the database
	string sql = "SELECT * FROM te_layer WHERE name = '" + name + "'";
	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return;
	if (!portal->query(sql))
	{
		delete portal;
		return;
	}
	if (!portal->fetchRow()) 
	{
		if (!proj)	// provides a default projection No Projection
			projection_ = new TeNoProjection();
		else
			projection_ = TeProjectionFactory::make (proj->params());
		db_->insertLayer(this);
	}
	else
		db_->loadLayer(this);
	int pid = projection_->id();
	delete projection_;
	projection_ = db_->loadProjection(pid);
	delete portal;
}


TeLayer::TeLayer(const string& name, TeDatabase* db, TeBox& box, TeProjection* proj):
	name_(name),
	id_(-1),
	db_(db),
	projection_(proj),
	box_(box),
	raster_(0)
{
	if (!db_ || name.empty())
		return;

	// verifies if layer already exists in the database
	string sql = "SELECT * FROM te_layer WHERE name = '" + name + "'";
	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return;
	if (!portal->query(sql))
	{
		delete portal;
		return;
	}
	if (!portal->fetchRow()) 
	{
		if (!proj)	// provides a default projection No Projection
			projection_ = new TeNoProjection();
		else
			projection_ = TeProjectionFactory::make (proj->params());
		db_->insertLayer(this);
	}
	else
		db_->loadLayer(this);
	delete portal;
}

TeLayer::~TeLayer()
{
	if ( projection_ != 0 )
	{
		delete projection_; 
		projection_ = 0;
	}
	for (TeRepresPointerVectorIterator it = repVector_.begin(); it != repVector_.end(); it++)
	{
		if (*it)
		{
			delete (*it);
			(*it) = 0;
		}
	}
	repVector_.clear();
	if (raster_!=0)
	{
		delete raster_;
		raster_ = 0;
	}
}

TeLayer::TeLayer( const TeLayer& other )
{
	if (projection_)
		delete projection_;
	projection_ = 0;
	if (other.projection_)
		projection_ = TeProjectionFactory::make ( other.projection_->params());
	name_		= other.name_;
	id_			= other.id_;
	db_			= other.db_;
}

TeLayer& 
TeLayer::operator= ( const TeLayer& other )
{
	if ( this != &other )
	{	
		if (projection_)
			delete projection_;
		projection_ = 0;
		if (other.projection_)
			projection_ = TeProjectionFactory::make ( other.projection_->params());
		name_		= other.name_;
		id_			= other.id_;
		box_		= other.box_;
		db_ = other.db_; 
	}
	return *this;
}
	

void 
TeLayer::setLayerBox(const TeBox& box)
{   
	box_ = box;
	if ((id_ > -1) && db_)
		db_->updateLayerBox(this);
}

void
TeLayer::updateLayerBox(const TeBox& box)
{
	updateBox(box_,box);
	if ((id_ > -1) && db_)
		db_->updateLayerBox(this);
}

void
TeLayer::updateLayerBox()
{
	if (id_ < 0 || !db_)
		return;

	string sql = "SELECT lower_x, lower_y, upper_x, upper_y FROM te_representation WHERE layer_id = " + Te2String(id_);
	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return;
	if (!portal->query(sql))
	{
		delete portal;
		return;
	}

	TeBox box;
	while (portal->fetchRow())
	{
		TeBox brep(portal->getDouble(0),portal->getDouble(1),portal->getDouble(2),portal->getDouble(3));
		updateBox(box,brep);
	}
	delete portal;
	box_ = box;
	db_->updateLayerBox(this);
	return;
}

int 
TeLayer::getNewObjectId()
{
	if(!db_)
		return -1;

	TeDatabasePortal* portal = db_->getPortal();
	if(!portal)
		return -1;

	try
	{
		int objId = 0;
		string sql;
		for (TeRepresPointerVectorIterator it = vectRepres().begin(); it != vectRepres().end(); it++)
		{
           TeRepresentation* rep = (*it);

           sql = "SELECT MAX(geom_id) FROM ";
           sql += rep->tableName_;
           if (portal->query(sql) && portal->fetchRow())
           {
				string data = portal->getData(0);
				int value = 0;
				if(!data.empty())
				   value = atoi(data.c_str());
				objId += value;
           }
           portal->freeResult();
       }
       delete portal;
       return ++objId;
   }
   catch(...)
   {
       if (portal)
       {
           delete portal;
           return -1;
       }
   }
    return -1;
} 

void  
TeLayer::setProjection ( TeProjection* proj )
{	
	if (!proj)
		return;

	// if no database set just update pointer
	if (!db_)
	{
		if ( projection_)
			delete projection_;
		projection_ = proj;
		return;
	}

	if (projection_)
	{
		proj->id(projection_->id());
		delete projection_;
	}
	projection_ = proj; 
	bool status = true;
	
	if ( projection_->id() <= 0  )	// new projection not in the database
	{
		status = db_->insertProjection(projection_);
		// update layer information
		if (status && id_ > 0)
		{
			string sql = "UPDATE te_layer SET projection_id = " + Te2String(projection_->id());
				sql += " WHERE layer_id=" + Te2String(id_);
			db_->execute(sql);
		}
	}
	else
		status = db_->updateProjection(projection_);

	if (raster_)
	{
		delete raster_;
		raster_ = 0;
	}
}

void 
TeLayer::raster( TeRaster* raster)
{	
	if (raster_ )
	{
		delete raster_;
		raster_ = 0;
	}
	raster_ = raster; 
}

bool
TeLayer::hasGeometry (TeGeomRep rep)
{ 
	if (repVector_.empty())
		return false;
	TeRepresPointerVectorIterator it;
	it = TeFindRepresentation(repVector_.begin(), repVector_.end(),rep);
	return (it != repVector_.end());
} 

int
TeLayer::geomRep()
{
	if (repVector_.empty())
		return TeGEOMETRYNONE;
	int r = 0;
	TeRepresPointerVectorIterator it;
	for (it=repVector_.begin(); it != repVector_.end(); ++it)
		r = (int)(*it)->geomRep_ | r;
	return r;
}

string
TeLayer::tableName(TeGeomRep rep) 
{
	TeRepresPointerVectorIterator it;
	it = TeFindRepresentation(repVector_.begin(), repVector_.end(),rep);
	if (it != repVector_.end())
		return (*it)->tableName_;
	else
		return string("");
}

int 
TeLayer::nGeometries(TeGeomRep rep)
{
	TeRepresPointerVectorIterator it;
	it = TeFindRepresentation(repVector_.begin(), repVector_.end(),rep);
	int ngeo = 0;
	if (it != repVector_.end())
	{
		string sql = "SELECT COUNT(geom_id) FROM " + (*it)->tableName_;
		TeDatabasePortal* portal = db_->getPortal();
		if (portal && portal->query(sql) && portal->fetchRow())
			ngeo = portal->getInt(0);
		delete portal;
	}
	return ngeo;
}


bool
TeLayer::getRepresentation(TeGeomRep repType, TeRepresPointerVector& result)
{
	TeRepresPointerVectorIterator it;
	it = repVector_.begin();
	while (it != repVector_.end())
	{
		if ((*it)->geomRep_ == repType)
			result.push_back((*it));
		++it;
	}
	if (result.empty())
		return false;
	return true;
}

TeRepresentation*
TeLayer::getRepresentation(TeGeomRep repType, const string& tName)
{

	TeRepresPointerVectorIterator it;
	it = repVector_.begin();
	while (it != repVector_.end())
	{
		if ((*it)->geomRep_ == repType)
		{
			if (tName.empty() || (*it)->tableName_== tName)
				return (*it);
		}
		it++;
	}
	return 0;
}

bool 
TeLayer::removeGeometry (TeGeomRep repType, const string& tName)
{
	if (!db_)
		return false;

	if (repType == TeTEXT && tName.empty())	// layer can have more than one TEXT table
		return false;

	TeRepresPointerVectorIterator it;
	it = TeFindRepresentation(repVector_.begin(), repVector_.end(),repType);
	while (it != repVector_.end())
	{
		if (tName.empty() || (*it)->tableName_== tName)
		{
			string del;
			TeRepresentation* rep = (*it);
			if (rep->id_ > 0)
			{
				del = "DELETE FROM te_representation WHERE repres_id=" + Te2String(rep->id_);
				if (!db_->execute(del))
					return false;
			}

			if (!rep->tableName_.empty())
			{
				if(rep->geomRep_ == TeRASTER || rep->geomRep_ == TeRASTERFILE)
				{
					TeDatabasePortal* portal = db_->getPortal();
					string sql, tname;

					sql = "SELECT lut_table, raster_table FROM " + rep->tableName_;
					portal->query(sql);

					while(portal->fetchRow())
					{
						// delete lut table
						tname = portal->getData(0);
						if(!tname.empty())
							db_->deleteTable(tname);
						// delete raster table
						tname = portal->getData(1);
						if(rep->geomRep_ != TeRASTERFILE && !tname.empty())
							db_->deleteTable(tname);
					}
					portal->freeResult();
					delete portal;

					// delete metadata table
					sql = "DROP TABLE " + rep->tableName_ + "_metadata";
					db_->execute(sql);
					if (raster_)
					{
						delete raster_;
						raster_ = 0;
					}
				}
				if (!db_->deleteTable(rep->tableName_))					
					return false;
			}
			repVector_.erase(it);
			delete rep;
			break;
		}
		it++;
		it = TeFindRepresentation(it,repVector_.end(),repType);
	}
	updateLayerBox();
	return true;
}

bool TeLayer::addGeometry(TeGeomRep repType, const string& tName, const string& desc)
{  
	// check if representation is already in layer
	if (repType == TeTEXT)
	{
		string tt;
		if (!tName.empty())
			tt = tName;
		else
			tt =  name_ + "Texts";
		
		if (getRepresentation(repType,tt))
			return true;
	}
	else if (getRepresentation(repType))
		return true;

	if (!db_)			// layer has no database associated
		return false;

	if (id_ < 0)		// layer has not saved in the database
	{
		if (!db_->insertLayer (this))
			return false;
	}

	TeRepresentation* represe = new TeRepresentation();
	represe->geomRep_ = repType;
	if (!tName.empty())
		represe->tableName_ = tName;
	if (!desc.empty())
		represe->description_ = desc;

	if (repType == TePOLYGONS)
	{
		if (tName.empty())
			represe->tableName_ = "Polygons" + Te2String(id_);
		if (!db_->createPolygonGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TeLINES)
	{
		if (tName.empty())
			represe->tableName_ = "Lines" + Te2String(id_);
		if (!db_->createLineGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TePOINTS)
	{
		if (tName.empty())
			represe->tableName_ = "Points" + Te2String(id_);
		if (!db_->createPointGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TeTEXT)
	{
		if (tName.empty())
			represe->tableName_ = "Texts" + Te2String(id_);
		if (!db_->createTextGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TeARCS)
	{
		if (tName.empty())
			represe->tableName_ = "Arcs"+ Te2String(id_);
 		if (!db_->createArcGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TeNODES)
	{
		if (tName.empty())
			represe->tableName_ = "Nodes"+ Te2String(id_);
 		if (!db_->createNodeGeometry (represe->tableName_))
			return false;
	}
	else if (repType == TeCELLS)
	{
		if (tName.empty())
			represe->tableName_ = "Cells"+ Te2String(id_);
 		if (!db_->createCellGeometry (represe->tableName_))
			return false;
	}
	else
	{
		return false;
	}

	bool res =  db_->insertRepresentation(id_,*represe);

	if (res) 
		repVector_.push_back(represe);
	else
		return false;
	return true;
}

bool 
TeLayer::addRasterGeometry(TeRasterParams& par, const string& objectId, 
						   const string& tName, const string& desc)
{
	string tableName = tName;
	TeRepresentation* rep = getRepresentation(TeRASTER,tableName);
	if (!rep)
	{
		rep = new TeRepresentation();
		updateBox(rep->box_,par.boundingBox());

		if (!tableName.empty())
			rep->tableName_ = tableName;
		else
			rep->tableName_ = "RasterLayer" + Te2String(id_);

		if (!desc.empty())
			rep->description_ = desc;

		rep->geomRep_ = TeRASTER;
		if (!db_->insertRepresentation(id_,*rep))
			return false;

		tableName = rep->tableName_;
		if (!db_->createRasterGeometry(tableName))
			return false;

		string metadataTableName = tableName+"_metadata";
		if (!db_->createRasterMetadataTable(metadataTableName))
			return false;

		repVector_.push_back(rep);
	}
	else
		tableName = rep->tableName_;

	string oid = objectId;
	if (oid.empty())
		oid = "O1";

	if (par.fileName_.empty())
	{
		string aux = "RasterLayer" + Te2String(id_) + "_R_" + objectId;
		bool flag = db_->tableExist(aux);
		short i=1;
		while (flag)
		{
			aux = "RasterLayer" + Te2String(id_) + "_R_" + objectId + "_" + Te2String(i);
			++i;
		}
		par.fileName_ = aux;
	}

	if (!db_->createRasterTable(par.fileName_))
		return false;

	if (!db_->insertRasterGeometry(tableName,par,objectId))
		return false;

	updateLayerBox(par.boundingBox());
	par.layerId_ = id_;
	par.objectId_ = oid;

	return true;
}

bool
TeLayer::addRasterFileGeometry(TeRaster* raster, const string& /* objectId */, const string& desc)
{
	if (!raster)
		return false;
	TeRepresentation* rep = getRepresentation(TeRASTERFILE);
	if (!rep)
	{
		rep = new TeRepresentation();
		updateBox(rep->box_,raster->params().boundingBox());
		rep->tableName_ = "RasterLayer" + Te2String(id_);
		rep->geomRep_ = TeRASTERFILE;
		rep->resX_ = raster->params().resx_;
		rep->resY_ = raster->params().resy_;
		rep->nCols_= raster->params().ncols_;
		rep->nLins_= raster->params().nlines_;
		if (!desc.empty())
			rep->description_ = desc;
		if (!db_->insertRepresentation(id_,*rep))
			return false;
		if (!db_->createRasterGeometry(rep->tableName_))
			return false;
		if (!db_->createRasterMetadataTable(rep->tableName_+"_metadata"))
			return false;
		repVector_.push_back(rep);
	}
	string oid = TeGetBaseName(raster->params().fileName_.c_str());
	if (oid.empty())
		oid = "O1";
	string sql = "delete from " +  rep->tableName_ + " where object_id = '" + oid + "'";
	db_->execute(sql);
	if (!db_->insertRasterGeometry(rep->tableName_,raster->params(),oid))
		return false;
	updateLayerBox(raster->params().boundingBox());
	raster->params().layerId_ = id_;
	raster->params().objectId_ = oid;
	return true;
}

bool 
TeLayer::addRasterGeometry(TeRaster* raster, const string& objectId)
{
	if (!raster)
		return false;
	TeRepresentation* rep = getRepresentation(TeRASTER);
	if (!rep)
	{
		rep = new TeRepresentation();
		updateBox(rep->box_,raster->params().boundingBox());
		rep->tableName_ = "RasterLayer" + Te2String(id_);
		rep->geomRep_ = TeRASTER;
		if (!db_->insertRepresentation(id_,*rep))
			return false;
		if (!db_->createRasterGeometry(rep->tableName_))
			return false;
		if (!db_->createRasterMetadataTable(rep->tableName_+"_metadata"))
			return false;
		repVector_.push_back(rep);
	}

	string oid = objectId;
	if (oid.empty())
		oid = "O1";
	string sql = "delete from " +  rep->tableName_ + " where object_id = '" + oid + "'";
	db_->execute(sql);
	if (!db_->insertRasterGeometry(rep->tableName_,raster->params(),oid))
		return false;
	updateLayerBox(raster->params().boundingBox());
	raster->params().layerId_ = id_;
	raster->params().objectId_ = oid;
	return true;
}

TeRaster*
TeLayer::raster(const string& objectId, const char& mode)
{	
   if (id_ <= 0 ||
	  !(hasGeometry(TeRASTER) || hasGeometry(TeRASTERFILE)))
       return 0; 

	if (raster_  && !objectId.empty() && 
	   (objectId != raster_->objectId() || 
		mode != raster_->params().mode_ ))
	{
		delete raster_;
		raster_ = 0; 
	}

	if (!raster_)
		raster_ = db_->loadLayerRaster(id_,objectId, mode);
	return raster_;
}

bool
TeLayer::getRasterGeometries(vector<string>& objectIds, unsigned int tilingType)
{
	objectIds.clear();
	if (!hasGeometry(TeRASTER) || id_ <= 0 )
		return false;

	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return false;

	string get = "SELECT geom_table FROM te_representation WHERE layer_id = " + Te2String(id_);
	get += " AND geom_type = 512";
	
	// error executing query or no there is no raster representation 
	if (!portal->query(get) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	string tableName = portal->getData(0);
	if (tableName.empty())
	{
		delete portal;
		return false;
	}
	portal->freeResult();
	get = "SELECT object_id FROM " + tableName;
	if (tilingType == 1 || tilingType == 2)
		get += " WHERE tiling_type=" + Te2String(tilingType);

	if (!portal->query(get))
	{
		delete portal;
		return false;
	}
	while (portal->fetchRow())
		objectIds.push_back(portal->getData(0));
	delete portal;
	return !objectIds.empty();
}


int 
TeLayer::nObjects(const string& tName)
{
	if (attTables_.empty())
		return 0;
	string linkName;
	vector<TeTable>::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((*it).name() == tName)
		{
			linkName = (*it).linkName();
			break;
		}
		it++;
	}
	if (linkName.empty())
		return 0;
	int no = 0;
	string sql;
	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return 0;
	sql = "SELECT COUNT(" + linkName + ") FROM " + (*it).name();
	if (portal->query(sql) && portal->fetchRow())
		no = portal->getInt(0);
	delete portal;
	return no;
}


bool 
TeLayer::createAttributeTable(TeTable& table)
{
	if (!db_->tableExist(table.name()))
	{
		db_->validTable(table);
  		if (!db_->createTable(table.name(),table.attributeList())) 
			return false;
	}
	
	if(!db_->insertTableInfo(id_,table))
		return false;
	
	vector<TeTable>::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((*it).name() == table.name())
			return true;
		++it;
	}
	attTables_.push_back(table);
	return true;
}

bool 
TeLayer::addAttributeTable(TeTable& table)
{
	vector<TeTable>::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((*it).name() == table.name())
			return false;
		it++;
	}
	attTables_.push_back(table);
	return true;
}

bool 
TeLayer::removeAttributeTable(string tableName)
{
	vector<TeTable>::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((*it).name() == tableName)
		{
			attTables_.erase(it);
			return true;
		}
		it++;
	}
	return false;
}

void 
TeLayer::updateAttributeTable(TeTable& table)
{
	vector<TeTable>::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((*it).name() == table.name())
		{
			attTables_.erase(it);
			break;
		}
		it++;
	}
	attTables_.push_back(table);
}

bool
TeLayer::loadLayerTables() 
{
	TeDatabase *db = database();
	if(!db)
		return false;

	attTables_.clear();
	return(db->loadLayerTable(this));
}

bool 
TeLayer::getAttrTables(TeAttrTableVector& atts, TeAttrTableType attType)
{
	TeAttrTableVector::iterator it = attTables_.begin();
	while (it != attTables_.end())
	{
		if ((attType == TeAllAttrTypes) || ((*it).tableType() == attType))
			atts.push_back((*it));
		it++;
	}
	return (!atts.empty());
}

bool 
TeLayer::getAttrTablesByName(vector<string> &tableNames, TeAttrTableVector& atts, TeAttrTableType attType)
{
	TeAttrTableVector::iterator it;
	vector<string>::iterator it2;

	for(it2 = tableNames.begin(); it2 != tableNames.end(); ++it2)
	{
		for(it = attTables_.begin(); it != attTables_.end(); ++it)
		{
			if ((attType == TeAllAttrTypes) || ((*it).tableType() == attType))
			{
				string name = (*it).name();
				string name2 = (*it2);
				if(name == name2)
				{
					atts.push_back((*it));
					break;
				}
			}
		}
	}
	return (!atts.empty());
}


bool 
TeLayer::getAttrTablesByName(const string& attrTableName, TeTable& table, TeAttrTableType attType)
{
	TeAttrTableVector::iterator it;

	for(it = attTables_.begin(); it != attTables_.end(); ++it)
	{
		if ((attType == TeAllAttrTypes) || ((*it).tableType() == attType))
		{
			string name = (*it).name();
			if(name == attrTableName)
			{
				table = (*it);
				return true;
			}
		}
	}
	
	return false;
}

bool 
TeLayer::addPolygons (TePolygonSet& polySet)
{
	if (!db_)
		return false;

	if (polySet.size() > 0 )
	{
		if (!this->hasGeometry(TePOLYGONS))
			this->addGeometry(TePOLYGONS);

		TeRepresentation* rep = getRepresentation(TePOLYGONS);
		if (rep)
		{
			if (!db_->insertPolygonSet (rep->tableName_, polySet))
				return false;
			updateLayerBox(polySet.box());
			updateBox(rep->box_,polySet.box());
			if (!db_->updateRepresentation(id_,*rep))
				return false;
		}
		else
			return false;
	}
	return true;
}

bool 
TeLayer::addLines (TeLineSet& lineSet)
{
	if (!db_)
		return false;

	if (lineSet.size() > 0)
	{
		if (!this->hasGeometry(TeLINES))
			this->addGeometry(TeLINES);
		TeRepresentation* rep = getRepresentation(TeLINES);
		if (rep)
		{
			if (!db_->insertLineSet (rep->tableName_, lineSet))
				return false;
			updateLayerBox(lineSet.box());
			updateBox(rep->box_,lineSet.box());
			if (!db_->updateRepresentation(id_,*rep))
				return false;
		}
		else
			return false;
	}
	return true;
}

bool 
TeLayer::addPoints (TePointSet& pointSet)
{
	if (!db_)
		return false;

	if (pointSet.size() > 0)
	{
		if (!this->hasGeometry(TePOINTS))
			this->addGeometry(TePOINTS);
		string tblName = tableName(TePOINTS);

		TeRepresentation* rep = getRepresentation(TePOINTS);
		if (rep)
		{	
			if (!db_->insertPointSet (rep->tableName_, pointSet))
				return false;
			updateLayerBox(pointSet.box());
			updateBox(rep->box_,pointSet.box());
			if (!db_->updateRepresentation(id_,*rep))
				return false;
		}
		else
			return false;
	}
	return true;
}

bool 
TeLayer::addText (TeTextSet& textSet, const string& tName)
{
	if (!db_)
		return false;

	TeRepresentation* rep =0;
	string tabName;
	if (tName.empty())
	{
		int i=0;
		string name; 
		do
		{
			name = "Text_" + Te2String(id_) + "_" + Te2String(i); 
			rep = getRepresentation(TeTEXT,name); 
			++i;
		}
		while (rep); 
		tabName = name;
	}
	else
		tabName = tName;

	if (textSet.size() > 0 )
	{
		rep = getRepresentation(TeTEXT,tabName);
		if (!rep)
		{
			this->addGeometry(TeTEXT,tabName);
			rep = getRepresentation(TeTEXT,tabName);
		}
		if (rep)
		{	
			if (!db_->insertTextSet (tabName, textSet))
				return false;
			updateLayerBox(textSet.box());
			updateBox(rep->box_,textSet.box());
			if (!db_->updateRepresentation(id_,*rep))
				return false;
		}
		else
			return false;
	}
	return true;
}

bool
TeLayer::addCells(TeCellSet& cellSet)
{
	if (!db_)
		return false;

	if (cellSet.size() > 0)
	{
		string tblName = tableName(TeCELLS);
		if (!this->hasGeometry(TeCELLS))
			this->addGeometry(TeCELLS);

		TeRepresentation* rep = getRepresentation(TeCELLS);
		if (rep)
		{
			rep->resX_ = cellSet.resX();
			rep->resY_ = cellSet.resY();
			if (!db_->insertCellSet(tableName(TeCELLS), cellSet))
				return false;
			updateLayerBox(cellSet.box());
			updateBox(rep->box_,cellSet.box());
			if (!db_->updateRepresentation(id_,*rep))
				return false;
		}		
		else
			return false;
	}
	return true;
}


bool 
TeLayer::saveAttributeTable(TeTable& table)
{
	if (!db_ || id_ <= 0)
		return false;
	if (!db_->tableExist(table.name()))
	{
		if (!db_->createTable(table.name(), table.attributeList()))
			return false;
	}
	if (db_->insertTable (table))
	{
		if (table.id() <= 0)
			return db_->insertTableInfo(id_,table);
		else
			return true;
	}
	return false;
}

bool
TeLayer::getPolygons(TePolygonSet &ps, const string& whereClause)
{
	if (!hasGeometry(TePOLYGONS))
		return false;
	return db_->selectPolygonSet(tableName(TePOLYGONS), whereClause, ps);
}

bool
TeLayer::getLines(TeLineSet &ls, const string& whereClause)
{
	if (!hasGeometry(TeLINES))
		return false;
	return db_->selectLineSet(tableName(TeLINES), whereClause, ls);
}

bool
TeLayer::getPoints(TePointSet &ps, const string& whereClause)
{
	if (!hasGeometry(TePOINTS))
		return false;
	return db_->selectPointSet(tableName(TePOINTS), whereClause, ps);
}

bool
TeLayer::getText(TeTextSet &ts, const string& whereClause)
{
	if (!hasGeometry(TeTEXT))
		return false;
	return db_->selectTextSet(tableName(TeTEXT), whereClause, ts);
}

bool
TeLayer::getCells(TeCellSet &cs, const string& whereClause)
{
	if (!hasGeometry(TeCELLS))
		return false;
	return db_->selectCellSet(id_, tableName(TeCELLS), whereClause, cs);
}

bool 
TeLayer::locatePolygon (TeCoord2D &pt, TePolygon &polygon, const double& tol)
{
	if (!hasGeometry(TePOLYGONS))
		return false;
	return db_->locatePolygon(tableName(TePOLYGONS),pt, polygon, tol);
}

bool 
TeLayer::locateLine (TeCoord2D &pt, TeLine2D &line, const double& tol)
{
	if (!hasGeometry(TeLINES))
		return false;
	return db_->locateLine(tableName(TeLINES),pt, line, tol);
}

bool 
TeLayer::locatePoint (TeCoord2D &pt, TePoint &point, const double& tol)
{
	if (!hasGeometry(TePOINTS))
		return false;
	return db_->locatePoint(tableName(TePOINTS), pt, point, tol);
}

bool 
TeLayer::locateText(TeCoord2D &pt, TeText &text, const double& tol)
{
	if (!hasGeometry(TeTEXT))
		return false;
	return db_->locateText(tableName(TeTEXT), pt, text, tol);
}

bool 
TeLayer::locateCell (TeCoord2D &pt, TeCell &cell, const double& tol)
{
	if (!hasGeometry(TeCELLS))
		return false;
	return db_->locateCell(tableName(TeCELLS), pt, cell, tol);
}

bool
TeLayer::loadGeometrySet(const string& geoid, TePolygonSet &ps)
{
	if (!hasGeometry(TePOLYGONS))
		return false;
	return db_->loadPolygonSet(tableName(TePOLYGONS), geoid, ps);
}

bool
TeLayer::loadGeometrySet (const string& geoid, TeLineSet &ls)
{
	if (!hasGeometry(TeLINES))
		return false;
	return db_->loadLineSet(tableName(TeLINES), geoid, ls);;
}

bool
TeLayer::loadGeometrySet (const string& geoid, TePointSet &ps)
{
	if (!hasGeometry(TePOINTS))
		return false;
	return db_->loadPointSet(tableName(TePOINTS), geoid, ps);
}

bool
TeLayer::loadGeometrySet (const string& geoid, TeCellSet &cs)
{
	if (!hasGeometry(TeCELLS))
		return false;
	return db_->loadCellSet(id_, tableName(TeCELLS), geoid, cs);
}

bool
TeLayer::loadGeometrySet (const string& geoid, TeTextSet &ts)
{
	if (!hasGeometry(TeTEXT))
		return false;
	return db_->loadTextSet(tableName(TeTEXT), geoid, ts);
}

string
TeLayer::mediaTable()
{
	TeAttrTableVector attrs;
	getAttrTables(attrs, TeAttrMedia);

	if(attrs.size()<1)
		return "";

	return (attrs[0].name());
}

void 
TeLayer::mediaTable(const string& name)
{
	TeAttributeList attList;
	TeTable table(name, attList,"object_id", "object_id", TeAttrMedia);
	attTables_.push_back (table);
}
