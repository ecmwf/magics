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
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeDataTypes.h
    \brief This file contains enumerators and data types used by TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_DATATYPE_H
#define  __TERRALIB_INTERNAL_DATATYPE_H

#ifdef WIN32 
#pragma warning ( disable: 4786 ) 
#endif

#include <vector>
#include <string>
#include <map> 

using namespace std;

//! SPRINGs data model entities
enum TeSpringModels
{   TeSPRCadastral=0, TeSPRNetwork=1, TeSPRThematic=2, TeSPRMNT=3 };


//!  Types for Attributes (stored in object-relational databases)
enum TeAttrDataType 
{TeSTRING, TeREAL, TeINT, TeDATETIME, TeBLOB, TeOBJECT, TeCHARACTER, TeUNKNOWN, TeUNSIGNEDINT,
 TePOINTTYPE, TeLINE2DTYPE, TePOLYGONTYPE, TeCELLTYPE, TeTEXTTYPE, TeNODETYPE,
 TePOINTSETTYPE, TeLINESETTYPE, TePOLYGONSETTYPE, TeCELLSETTYPE, TeTEXTSETTYPE, TeNODESETTYPE,
 TeRASTERTYPE};


//! Geometrical representations 
/*! Uses a binary ennumeration since the same layer 
    may contain more than one geometrical representation
*/
enum TeGeomRep 
{	TePOLYGONS = 1, TeLINES = 2, TePOINTS = 4, TeSAMPLES = 8, TeCONTOURS = 16,
	TeARCS = 32, TeNODES = 64  , TeTEXT = 128, TeCELLS = 256 , TeRASTER = 512,
	TeRASTERFILE = 1024, TeGEOMETRYNONE = 2048, TeSHAPEFILE = 4096, TeCOVERAGE = 8192};

//! Chart types
enum TeChartType
{   TeBarChart, TePieChart };

//! Objects that will be grouped or used in a chart
enum TeSelectedObjects
{	TeAll, TeSelectedByPointing, TeNotSelectedByPointing, TeSelectedByQuery, TeNotSelectedByQuery,
	TeSelectedByPointingAndQuery, TeSelectedByPointingOrQuery, TeGrouped, TeNotGrouped };

//! Modes of object selection
enum TeObjectSelectionMode
{   TeDefaultSelection, TePointingSelection, TeQuerySelection, TePointingAndQuerySelection };

//! Modes of grouping objects
enum TeGroupingMode
{ TeEqualSteps = 0, TeQuantil = 1, TeStdDeviation = 2, TeUniqueValue = 3,
  TeRasterSlicing = 4, TeNoGrouping = 100 };

//! Direction of use in a ramp color
enum TeColorDirection
{	TeColorAscSatEnd, TeColorDescSatEnd, TeColorAscSatBegin, TeColorDescSatBegin };

//! Types of attribute tables in a layer
enum TeAttrTableType
{	TeAllAttrTypes, TeAttrStatic, TeAttrMedia, TeAttrExternal, TeAttrEvent, TeFixedGeomDynAttr, 
	TeDynGeomDynAttr, TeGeomAttrLinkTime, TeGeocodingData };

//! Types of integrity relations among tables in a database
enum TeDBRelationType
{	TeNoRelation, TeRINoCascadeDeletion, TeRICascadeDeletion };

//! Data types
enum TeDataType 
{	TeBIT, TeUNSIGNEDCHAR, TeCHAR, TeUNSIGNEDSHORT, TeSHORT, TeINTEGER, TeUNSIGNEDLONG, TeLONG, TeFLOAT, TeDOUBLE };


//!	Statistics
/*!
	 - TeSUM				sum of the values
	 - TeMAXVALUE			maximum value
	 - TeMINVALUE			minimal value
	 - TeCOUNT				total number of the values
	 - TeVALIDCOUNT			valid number of the values
	 - TeSTANDARDDEVIATION	standard deviation 
	 - TeKERNEL				kernel
	 - TeMEAN				mean
	 - TeVARIANCE			variance
	 - TeSKEWNESS			third moment about the Mean 			
	 - TeKURTOSIS			forth moment about the Mean
	 - TeAMPLITUDE			maximum value minus minimal value
	 - TeMEDIAN				median value
	 - TeVARCOEFF			coefficient of variation 
	 - TeMODE				value more frequent 
*/ 
enum TeStatisticType
{	TeNOSTATISTIC, TeSUM, TeMAXVALUE, TeMINVALUE, TeCOUNT, TeVALIDCOUNT, TeSTANDARDDEVIATION, TeKERNEL,  
	TeMEAN, TeVARIANCE, TeSKEWNESS, TeKURTOSIS, TeAMPLITUDE, TeMEDIAN, TeVARCOEFF, TeMODE};

//! A map of an statistic to its value
typedef map<TeStatisticType, double> TeStatisticValMap;

//! A map of an statistic to its string value
typedef map<TeStatisticType, string> TeStatisticStringValMap;

//! A map of an object id to its statistics
typedef map<string, TeStatisticValMap> TeObjStatisticsMap;

//! A map of an statistic to the column name where its value is stored
typedef map<TeStatisticType, string> TeStatisticsMap;

//! Intersection Algorithms.
enum TeIntersectorAlgorithm { TeBentleyOttman, TeRedBlue, TeTrivial };

//! Topological relations
enum TeSpatialRelation	
{	TeDISJOINT = 1, TeTOUCHES = 2, TeCROSSES = 4, TeWITHIN = 8, 
	TeOVERLAPS = 16, TeCONTAINS = 32, TeINTERSECTS = 64, TeEQUALS = 128, TeCOVERS = 256,
	TeCOVEREDBY = 512, TeUNDEFINEDREL = 1024};

//! Temporal predicates
enum TeTemporalRelation { TeTIMEUNDEFINED, TeTIMEEQUAL, TeTIMEBEFORE, TeTIMEAFTER, TeTIMEMEETS, TeTIMEDURING, TeTIMEOVERLAPS, TeTIMEENDS, TeTIMESTARTS };

//! Logical predicates
enum TeLogicalOperator { TeAND = 1, TeOR = 2 };

//! Time chronons
enum TeChronon {	TeNOCHRONON, TeSECOND, TeMINUTE, TeHOUR, TeDAY, TeMONTH, TeYEAR,
					TeDAYOFWEEK, TeDAYOFMONTH, TeDAYOFYEAR, TeMONTHOFYEAR, TeSEASON, TeWEEKOFYEAR, 
					TeHOUROFDAY, TeMINUTEOFHOUR, TeSECONDOFMINUTE };

//! Polygon style types
enum TePolyBasicType { TePolyTypeTransparent=0, TePolyTypeFill=1, TePolyTypeHorizontal=2,
					 TePolyTypeVertical=3, TePolyTypeFDiagonal=4, TePolyTypeBDiagonal=5,
					 TePolyTypeCross=6, TePolyTypeDiagonalCross=7 };
//! Line style types
enum TeLnBasicType { TeLnTypeContinuous=0, TeLnTypeDashed=1, TeLnTypeDotted=2,  
					 TeLnTypeDashDot=3, TeLnTypeDashDotDot=4, TeLnTypeNone=5,
					 TeLnTypeCustom=6 };
//! Point style types
enum TePtBasicType { TePtTypePlus=0, TePtTypeStar=1, TePtTypeCircle=2, TePtTypeX=3,  
					 TePtTypeBox=4, TePtTypeDiamond=5, TePtTypeHollowCircle=6, 
					 TePtTypeHollowBox=7, TePtTypeHollowDiamond=8 };

//! Type of segment intersection
enum TeSegmentIntersectionType { TeProperIntersection, TeImproperIntersection };

//! Types of implementations of a generalized proximity matrix 
enum TeGPMImplementation { TeGraphBreymann };

//! Construction strategies to the generalized proximity matrix 
enum TeGPMConstructionStrategy { TeAdjacencyStrategy, TeDistanceStrategy, TeNearestNeighboursStrategy,
								 TeClosedNetworkStrategy, TeOpenNetworkStrategy, TeOpenNetworkStrategy2 };

//! Slicing strategies to the generalized proximity matrix 
enum TeGPMSlicingStrategy { TeNoSlicingStrategy, TeZonesSlicingStrategy };
	
//! Weight strategies to the generalized proximity matrix 
enum TeGPMWeightsStrategy { TeNoWeightsStrategy, TeInverseDistanceStrategy, 
							TeSquaredInverseDistStrategy, TeConnectionStrenghtStrategy};


#endif
