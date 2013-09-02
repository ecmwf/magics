//***********************************************************************
//      TerraLib is a GIS Classes and Functions Library that 
//      strongly explores Spatial Database Technologies 
//
//      Copyright © 2002 INPE and Tecgraf/PUC-Rio. 
//
//      This library is free software; you can redistribute it 
//      and/or modify it under the terms of the GNU Lesser General 
//      Public License as published by the Free Software Foundation
//      version 2.1.(http://www.opensource.org/licenses/lgpl-license.php)
//
//      
//
//      Send questions or suggestions about the TerraLib Project 
//      to terralib@dpi.inpe.br .
//**************************************************************************//
/*! \file TeCellAlgorithms.h
    This file contains Algorithms for Operations on cell representation
*/

#ifndef TeCellAlgorithms_H
#define TeCellAlgorithms_H

#include <string>
#include <map>
#include <TeDataTypes.h>
#include <TeSelectedObject.h>
using namespace std;

class TeLayer;
class TeTheme;
class TeDatabase;
class TeDatabasePortal;
struct TeBox;
class TeProjection;

//! A map of an statistic to the column name where its value is stored
typedef map<TeStatisticType, string> TeStatisticsMap;

//! A map of an statistic to its value
typedef map<TeStatisticType, double> TeStatisticValMap;

//! A map of an object id to its statistics
typedef map<string, TeStatisticValMap> TeObjStatisticsMap;

//! Creates a layer of the cell representation  from the polygons of a theme 
TeLayer* TeCreateCells(const string&,TeTheme*, double, double);

//! Creates a layer of the cell representation recovering a bounding box entirely
TeLayer* TeCreateCells(const string&, TeDatabase* db, TeProjection* proj, TeBox& bb, double, double);

//! Calculates the statistics to each cell through database functions and keep in a database table
bool TeStatisticsDB (const string& tableGeomCell, const string& tableCollCell, const string& tableAttrCell, TeStatisticsMap& stat, 
				     const string& tableGeomIn, const string& tableCollIn, TeGeomRep repIn, const string& tableAttrIn, const string& attrIn, TeDatabase* db=0);

//! Calculates the statistics to each cell in memory 
bool TeStatisticsMem(TeSelectedObjectMap& selObjMap, TeObjStatisticsMap& result);

//! Keeps the statistics in a database table from a TeObjStatisticsMap
bool TeKeepStatistics(const string& tableAttrCell, TeStatisticsMap& stat, TeDatabase* db, TeObjStatisticsMap& selObjStat);

//! Keeps the statistics in a database table from a TeDatabasePortal
bool TeKeepStatistics(const string& tableAttrCell, TeStatisticsMap& stat, TeDatabasePortal* portal);

#endif

