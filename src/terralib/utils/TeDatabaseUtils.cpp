/************************************************************************************
TerraView - visualization and exploration of geographical databases using TerraLib.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.
This file is part of TerraView. TerraView is free software; you can redistribute it 
and/or modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

You should have received a copy of the GNU General Public License
along with TerraView.
The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The software provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use of
this program and its documentation.
*************************************************************************************/

#include <TeDatabaseUtils.h>
#include <TeLayerFunctions.h>


bool isTerralibModel(TeDatabase* db)
{
	vector<string>::iterator it;
	vector<string> tables;
	db->listTables(tables);

	for(unsigned int i = 0; i<tables.size(); i++)
	{
		if((it = find(tables.begin(), tables.end(), "te_projection")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_layer")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_layer_table")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_tables_relation")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_representation")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_view")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_theme")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_grouping")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_theme_table")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_legend")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_visual")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_visual_raster")) == tables.end())
			return false;
		if((it = find(tables.begin(), tables.end(), "te_database")) == tables.end())
			return false;
	}
	if(tables.size() == 0)
		return false;

	return true;
}


bool TeCopyDatabase(TeDatabase* dbFrom, TeDatabase* dbTo)
{
	// -- Step 1 : copies the external tables
	TeAttrTableVector externs;
	dbFrom->getAttrTables(externs,TeAttrExternal);
	if (!externs.empty())
	{
		TeDatabasePortal* portal = dbFrom->getPortal();
		if (!portal)
		{
			// log error here
			return false;
		}

		for (unsigned int nt=0; nt < externs.size(); nt++)
		{
			TeTable table = externs[nt];
			table.setId(-1);
			string sql = "select * from " + table.name();
			if (!portal->query(sql) || portal->numRows() == 0)
			{
				portal->freeResult();
				continue;
			}
			if (!dbTo->createTable(table.name(),table.attributeList()))
			{
				portal->freeResult();
				continue;
			}
			unsigned int nr=0;
			while (portal->fetchRow())
			{
				TeTableRow row;
				for(int i = 0; i < portal->numFields(); i++)
					row.push_back(portal->getData(i));
				table.add(row);
				nr++;
				if (nr % 200)
				{
					dbTo->insertTable(table);
					table.clear();
					nr = 0;
				}
			}
			if (table.size() >0)
				dbTo->insertTable(table);
				table.clear();	
			dbTo->insertTableInfo(-1,table);
			portal->freeResult();
		}
		delete portal;
	}

	// -- Step 2: Copies the layers
	if(!dbFrom->loadLayerSet())
	{
		// log error here
		return false;
	}

	TeLayerMap& fromLayerMap = dbFrom->layerMap();
	TeLayerMap::iterator itFrom = fromLayerMap.begin();

	while(itFrom != fromLayerMap.end())
	{
		TeLayer* fromLayer = (*itFrom).second;
		TeLayer* toLayer = new TeLayer(fromLayer->name(), dbTo, fromLayer->projection());
		map<string, string> tables;
		if (!TeCopyLayerToLayer(fromLayer, toLayer, &tables))
		{
			// log error here: layer couldn´t be copied
		}
		++itFrom;
	}
	// -- Set 3: Copies the views and themes
	if(!dbFrom->loadViewSet(dbFrom->user()))
	{
		// log error here
		return false;
	}

	if(!dbTo->loadLayerSet())
	{
		// log error here
		return false;
	}

	TeViewMap&	fromViewMap = dbFrom->viewMap();
	TeViewMap::iterator itvFrom = fromViewMap.begin();
	while (itvFrom != fromViewMap.end())
	{
		TeView* fromView = (*itvFrom).second;			
		TeView* toView = new TeView();

		TeProjection* toViewProjection = 0;
		if (fromView->projection())
			 toViewProjection = TeProjectionFactory::make(fromView->projection()->params());
		
		toView->projection(toViewProjection);
		toView->name(fromView->name());
		toView->user(dbTo->user());
		toView->isVisible(fromView->isVisible());

		TeBox b;
		toView->setCurrentBox(b);
		toView->setCurrentTheme(-1);

		if (!dbTo->insertView(toView))
		{
			// log error here
			++itvFrom;
			continue;
		}
		dbTo->insertView(toView);
		if(dbTo->projectMap().empty() == false)
		{
			TeProjectMap& pm = dbTo->projectMap();
			TeProject* project = pm.begin()->second;
			project->addView(toView->id());
		}
		dbTo->insertProjectViewRel(1, toView->id());

		TeLayerMap& toLayerMap = dbTo->layerMap();			
		vector<TeViewNode*>& themeVec = fromView->themes();
		for (unsigned int i = 0; i < themeVec.size(); ++i)
		{
			TeTheme* themeFrom = (TeTheme*) themeVec[i];
			string fromLayerName = themeFrom->layer()->name();
			TeLayer* toLayer = 0;
			TeLayerMap::iterator itTo = toLayerMap.begin();
			while(itTo != toLayerMap.end())
			{
				if(itTo->second->name() == fromLayerName)
				{
					toLayer = itTo->second;
					break;
				}
				++itTo;
			}

			if (!toLayer )
			{
				// log error here
				continue;
			}
			
			TeTheme* themeTo = new TeTheme(themeFrom->name(), toLayer);
			toView->add(themeTo);
						
			themeTo->outOfCollectionLegend(themeFrom->outOfCollectionLegend());
			themeTo->withoutDataConnectionLegend(themeFrom->withoutDataConnectionLegend ());
			themeTo->defaultLegend(themeFrom->defaultLegend());
			themeTo->pointingLegend(themeFrom->pointingLegend());
			themeTo->queryLegend(themeFrom->queryLegend());

			TeAttrTableVector tftablevec = themeFrom->attrTables();
			TeAttrTableVector tttablevec;

			for (unsigned int nt=0; nt<tftablevec.size(); nt++)
			{
				TeTable attTable(tftablevec[nt].name());
				dbTo->loadTableInfo(attTable);
				tttablevec.push_back(attTable);
			}
			themeTo->setAttTables(tttablevec);
			themeTo->attributeRest(themeFrom->attributeRest());
			themeTo->temporalRest(themeFrom->temporalRest());
			themeTo->spatialRest(themeFrom->spatialRest());
			themeTo->visibleRep(themeFrom->visibleRep());
			if(!themeTo->save() || !themeTo->buildCollection())
			{
				// log error here
				continue;
			}
			themeTo->generateLabelPositions();
			
			if(themeFrom->grouping().groupMode_ != TeNoGrouping)
			{
				TeGrouping grouping;
				grouping = themeFrom->grouping();				
				themeTo->buildGrouping(grouping);
				TeLegendEntryVector& legends = themeFrom->legend();
				for (unsigned int nl=0; nl<legends.size(); nl++)
					themeTo->setGroupingVisual(nl+1,legends[nl].getVisualMap());
				if (!themeTo->saveGrouping())
				{
						// log error here
				}
			}
		} // end for each theme
		++itvFrom;
	} // end for each view
	return true;
}

vector<string> getObjects(TeTheme* theme, int sel)
{
	vector<string> svec;
	if(!theme)
		return svec;

	TeDatabase* db = 0;
	if(theme->getProductId() == TeEXTERNALTHEME)
		db = static_cast<TeExternalTheme*>(theme)->getSourceDatabase();
	else if(theme->getProductId() == TeTHEME)
		db = static_cast<TeTheme*>(theme)->layer()->database();

	if(db == 0)
		return svec;

	string C = theme->collectionTable();
	string CA = theme->collectionAuxTable();
	string input;

	if(sel == TeSelectedByPointing)
		input += " WHERE " + C + ".c_object_status = 1 OR " + C + ".c_object_status = 3";
	else if(sel == TeNotSelectedByPointing)
		input += " WHERE " + C + ".c_object_status = 0 OR " + C + ".c_object_status = 2";
	else if(sel == TeSelectedByQuery)
		input += " WHERE " + C + ".c_object_status = 2 OR " + C + ".c_object_status = 3";
	else if(sel == TeNotSelectedByQuery)
		input += " WHERE " + C + ".c_object_status = 0 OR " + C + ".c_object_status = 1";
	else if(sel == TeSelectedByPointingAndQuery)
		input += " WHERE " + C + ".c_object_status = 3";
	else if(sel == TeSelectedByPointingOrQuery)
		input += " WHERE " + C + ".c_object_status <> 0";
	else if(sel == TeGrouped)
		input += " WHERE " + C + ".c_legend_id <> 0";
	else if(sel == TeNotGrouped)
		input += " WHERE " + C + ".c_legend_id = 0";

	string query = "SELECT " + C + ".c_object_id FROM " + C + " LEFT JOIN " + CA;
	query += " ON " + C + ".c_object_id = " + CA + ".object_id" + input;

	TeDatabasePortal* portal = db->getPortal();
	if (portal->query(query) == false)
	{
		delete portal;
		return svec;
	}
	while (portal->fetchRow())
		svec.push_back(portal->getData(0));

	delete portal;
	return svec;
}

vector<string> getItems(TeTheme* theme, int sel)
{
	vector<string> svec;
	if(!theme)
		return svec;

	TeDatabase* db = 0;
	if(theme->getProductId() == TeEXTERNALTHEME)
		db = static_cast<TeExternalTheme*>(theme)->getSourceDatabase();
	else if(theme->getProductId() == TeTHEME)
		db = static_cast<TeTheme*>(theme)->layer()->database();

	if(db == 0)
		return svec;

	string C = theme->collectionTable();
	string CA = theme->collectionAuxTable();
	string input;

	if(sel == TeSelectedByPointing)
		input += " WHERE " + CA + ".grid_status = 1 OR " + CA + ".grid_status = 3";
	else if(sel == TeNotSelectedByPointing)
		input += " WHERE " + CA + ".grid_status = 0 OR " + CA + ".grid_status = 2";
	else if(sel == TeSelectedByQuery)
		input += " WHERE " + CA + ".grid_status = 2 OR " + CA + ".grid_status = 3";
	else if(sel == TeNotSelectedByQuery)
		input += " WHERE " + CA + ".grid_status = 0 OR " + CA + ".grid_status = 1";
	else if(sel == TeSelectedByPointingAndQuery)
		input += " WHERE " + CA + ".grid_status = 3";
	else if(sel == TeSelectedByPointingOrQuery)
		input += " WHERE " + CA + ".grid_status <> 0";
	else if(sel == TeGrouped)
		input += " WHERE " + C + ".c_legend_id <> 0";
	else if(sel == TeNotGrouped)
		input += " WHERE " + C + ".c_legend_id = 0";

	string query = "SELECT " + CA + ".unique_id FROM " + C + " LEFT JOIN " + CA;
	query += " ON " + C + ".c_object_id = " + CA + ".object_id" + input;

	TeDatabasePortal* portal = db->getPortal();
	if (portal->query(query) == false)
	{
		delete portal;
		return svec;
	}
	while (portal->fetchRow())
		svec.push_back(portal->getData(0));

	delete portal;
	return svec;
}

vector<string> getObjects(TeTheme* theme, vector<string>& itens)
{
	vector<string> svec;
	if(!theme)
		return svec;

	TeDatabase* db = 0;
	if(theme->getProductId() == TeEXTERNALTHEME)
		db = static_cast<TeExternalTheme*>(theme)->getSourceDatabase();
	else if(theme->getProductId() == TeTHEME)
		db = static_cast<TeTheme*>(theme)->layer()->database();

	if(db == 0)
		return svec;

	TeDatabasePortal* portal = db->getPortal();
	string C = theme->collectionTable();
	string CA = theme->collectionAuxTable();

	string query = "SELECT " + C + ".c_object_id FROM " + C + " LEFT JOIN " + CA;
	query += " ON " + C + ".c_object_id = " + CA + ".object_id";
	query += " WHERE " + CA + ".unique_id IN ";

	set<string> idSet;
  
  vector< string >::iterator it_begin = itens.begin();
  vector< string >::iterator it_end = itens.end();
  
	vector<string> inVec = generateInClauses(it_begin, it_end, db, false);
	vector<string>::iterator it;
	for(it=inVec.begin(); it!=inVec.end(); ++it)
	{
		if((*it).empty() == false)
		{
			string sel = query + *it;
			if (portal->query(sel) == false)
			{
				delete portal;
				return svec;
			}
			while (portal->fetchRow())
				idSet.insert(portal->getData(0));
			portal->freeResult();
		}
	}
	delete portal;

	set<string>::iterator sit;
	for(sit=idSet.begin(); sit!=idSet.end(); ++sit)
		svec.push_back(*sit);
	return svec;
}

vector<string> getItems(TeTheme* theme, vector<string>& objects)
{
	vector<string> svec;
	if(!theme)
		return svec;

	TeDatabase* db = 0;
	if(theme->getProductId() == TeEXTERNALTHEME)
		db = static_cast<TeExternalTheme*>(theme)->getSourceDatabase();
	else if(theme->getProductId() == TeTHEME)
		db = static_cast<TeTheme*>(theme)->layer()->database();

	if(db == 0)
		return svec;

	TeDatabasePortal* portal = db->getPortal();
	string C = theme->collectionTable();
	string CA = theme->collectionAuxTable();

	string query = "SELECT " + CA + ".unique_id FROM " + C + " LEFT JOIN " + CA;
	query += " ON " + C + ".c_object_id = " + CA + ".object_id";
	query += " WHERE " + C + ".c_object_id IN ";
  
  vector< string >::iterator it_begin = objects.begin();
  vector< string >::iterator it_end = objects.end();  

	vector<string> inVec = generateInClauses(it_begin, it_end, db);
	vector<string>::iterator it;
	for(it=inVec.begin(); it!=inVec.end(); ++it)
	{
		if((*it).empty() == false)
		{
			string sel = query + *it;
			if (portal->query(sel) == false)
			{
				delete portal;
				return svec;
			}
			while (portal->fetchRow())
				svec.push_back(portal->getData(0));
			portal->freeResult();
		}
	}
	delete portal;
	return svec;
}

map<string, vector<string> > getObject2ItemsMap(TeTheme* theme, vector<string>& itens)
{
	map<string, vector<string> > outMap;
	if(!theme)
		return outMap;

	TeDatabase* db = 0;
	if(theme->getProductId() == TeEXTERNALTHEME)
		db = static_cast<TeExternalTheme*>(theme)->getSourceDatabase();
	else if(theme->getProductId() == TeTHEME)
		db = static_cast<TeTheme*>(theme)->layer()->database();

	if(db == 0)
		return outMap;

	TeDatabasePortal* portal = db->getPortal();
	string C = theme->collectionTable();
	string CA = theme->collectionAuxTable();

	string query = "SELECT " + C + ".c_object_id, " + CA + ".unique_id FROM " + C + " LEFT JOIN " + CA;
	query += " ON " + C + ".c_object_id = " + CA + ".object_id";
	query += " WHERE " + CA + ".unique_id IN ";
  
  vector< string >::iterator it_begin = itens.begin();
  vector< string >::iterator it_end = itens.end();   

	vector<string> inVec = generateInClauses(it_begin, it_end, db, false);
	vector<string>::iterator it;
	for(it=inVec.begin(); it!=inVec.end(); ++it)
	{
		if((*it).empty() == false)
		{
			string sel = query + *it;
			if (portal->query(sel) == false)
			{
				delete portal;
				return outMap;
			}
			while (portal->fetchRow())
				outMap[portal->getData(0)].push_back(portal->getData(1));
			portal->freeResult();
		}
	}
	delete portal;

	return outMap;
}

vector<string>
generateItemsInClauseVec(TeTheme* theme, string& where)
{
	vector<string> inClauseVector;
	TeDatabase* db = 0;
	string	CT = theme->collectionTable();
	string	CA = theme->collectionAuxTable();
	string	from = " FROM " + CT + " LEFT JOIN " + CA + " ON " + CT + ".c_object_id = " + CA + ".object_id";

	if(theme->getProductId()==TeTHEME)
		db = theme->layer()->database();
	else if(theme->getProductId()==TeEXTERNALTHEME)
		db = ((TeExternalTheme*) theme)->getSourceDatabase();

	if(db == 0)
		return inClauseVector;

	TeDatabasePortal* portal = db->getPortal();
	string sel = "SELECT " + CT + ".c_object_id, " + CA + ".unique_id " + from;

	if(where.empty() == false)
		sel += " " + where;

	map<string, vector<string> > objMap;
	if(portal->query(sel))
	{
		while(portal->fetchRow())
			objMap[portal->getData(0)].push_back(portal->getData(1));
	}

	map<string, vector<string> >::iterator mit;
	vector<string>::iterator it;

	int i, chunkSize = 200;
	string inClause;
	
	i = 0;
	bool chunk = true;
	for(mit=objMap.begin(); mit!=objMap.end(); ++mit)
	{
		if (chunk == true)
		{
			chunk = false;
			if (!inClause.empty())
			{
				inClause[inClause.size() - 1] = ')';
				inClauseVector.push_back(inClause);
				inClause.clear();
			}
			inClause = "(";
		}
		for(it=mit->second.begin(); it!=mit->second.end(); ++it)
		{
			inClause += *it + ",";
			i++;
			if (i%chunkSize == 0)
				chunk = true;
		}
	}
	if (!inClause.empty())
	{
		inClause[inClause.size() - 1] = ')';
		inClauseVector.push_back(inClause);
	}
	return inClauseVector;
}



