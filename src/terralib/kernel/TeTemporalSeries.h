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
/*! \file TeTemporalSeries.h
    \brief This file contains structures and definitions to deal with temporal series.
*/

#ifndef  __TERRALIB_INTERNAL_TS_H
#define  __TERRALIB_INTERNAL_TS_H


#include "TeTheme.h"
#include "TeTimeInterval.h"


/* 
	ATTR_DYN: a spatial object with a dynamic attribute 
	ATTR_DYN_GROUP: some spatial objects with a dynamic attribute table 
	EVENT: count of events
	ATTR_EVENT: grouping the attributes of the events
*/
//!	type of temporal series
enum TeTSType  { TeATTRDYN, TeATTRDYNGROUP, TeEVENT, TeATTREVENT};

 
//! type of the data collected in the serie temporal 
enum TeTSDataType  { TeDATA, TeMISSING, TeDMISSING, TeOUTLIER, TeDOUTLIER, TePREDICTION };


//! A class that represent each time frame of a temporal series
class TL_DLL TeTSEntry
{
public:
	int					timeFrame_;  //the time frame of the temporal series 
	double 				value_;
	TeTSDataType 		type_; 
	int 				nItens_; //number of instances
	TeTimeInterval		time_;
	int					timeInt_;

	//! Constructor
	TeTSEntry(): 
		timeFrame_(-1),
		value_(0), 
		type_(TeMISSING), 
		nItens_(0),
		timeInt_(-1)
		{}

	//! Constructor
	TeTSEntry(const double& val, TeTimeInterval& time, int timeFrame=-1, int nItens=0, TeTSDataType dType=TeMISSING); 
		
	
	//! Copy constructor
	TeTSEntry(const TeTSEntry& other)
	{
		timeFrame_ = other.timeFrame_;
		value_ = other.value_;
		time_ = other.time_;
		nItens_ = other.nItens_;
		type_ = other.type_;
		timeInt_ = other.timeInt_;
	}

	//! Operator = 
	TeTSEntry& operator=(const TeTSEntry& other); 

};

//! Parameters of a temporal series
class TL_DLL TeTSParams 
{
public:
	TeTSType			type_;			//! type of the series
  	TeChronon			chronon_;		//! Chronon used to generated the series
  	TeTimeInterval		time_;			//! Date/Time validy 
  	TeStatisticType		timeOper_; 		//! operator to temporal grouping - to group objects in each time frame
  	TeStatisticType		spatOper_; 		//! operator to spatial grouping - to group instances to each object 
  
	// Measures
  	TeTheme*		orTheme_;		//Pointer to the origin theme 
	string			objectId_;
  	string			attrTable_;	    //temporal table 
	string			attrColumn_;	//date column 


	//! Empty constructor
	TeTSParams():
		chronon_(TeNOCHRONON),
		timeOper_(TeNOSTATISTIC),
		spatOper_(TeNOSTATISTIC),
		orTheme_(0),
		objectId_(""),
		attrTable_(""),
		attrColumn_("")
		{ }

	//! Constructor - - serie type is ATTR_DYN_GROUP, EVENT, ATTR_EVENT
	TeTSParams(TeTheme* theme, TeChronon chr, const string& table, const string& col, TeStatisticType tOper, TeStatisticType sOper):
		chronon_(chr),
		timeOper_(tOper), 
		spatOper_(sOper),
		orTheme_(theme),
		objectId_(""),
		attrTable_(table),
		attrColumn_(col)
		{}

	//! Constructor - serie type is ATTR_DYN
	TeTSParams(TeTheme* theme, const string& objId, TeChronon chr, const string& table, const string& col, TeStatisticType tOper, TeStatisticType sOper):
		chronon_(chr),
		timeOper_(tOper),
		spatOper_(sOper),
		orTheme_(theme),
		objectId_(objId),
		attrTable_(table),
		attrColumn_(col)
		{}

	//! Operator = 
	TeTSParams& operator=(const TeTSParams& other); 
}; 

//! A class to represent temporal series
class TL_DLL TeTemporalSeries
{
public:
	TeTSParams			TSparams_; 
	vector<TeTSEntry> 	series_; //! serie

	//! Constructor
	TeTemporalSeries(): 
		TSparams_(TeTSParams())
	{}

	//! Constructor
	TeTemporalSeries(TeChronon chr, TeTheme* theme=0, const string& table="", const string& col="", TeStatisticType tOper=TeNOSTATISTIC, TeStatisticType sOper=TeNOSTATISTIC): 
		TSparams_(TeTSParams(theme, chr, table, col, tOper, sOper))
	{}

	//! Constructor
	TeTemporalSeries(TeTheme* theme, const string& objId, TeChronon chr, const string& table, const string& col, TeStatisticType tOper, TeStatisticType sOper): 
		TSparams_(TeTSParams(theme, objId, chr, table, col, tOper, sOper))
	{}

	//! Gets the time frame  (0 until n-1)
	bool getTSEntry(TeTSEntry& ts, unsigned int frame); 

	//! Operator = 
	TeTemporalSeries& operator=(const TeTemporalSeries& other); 

	//! Fills the time interval for each frame
	bool buildFrameIntervals();

	//! Returns the number of time frames
	int numTimeFrames() { return series_.size(); }

	//! Returns the time of each frame
	int timeInt(int frame) { return ((series_[frame-1]).timeInt_); }

	//! Sets the temporal series entry value
	bool setTSEntryValue(unsigned int frame, double val, TeTSDataType type); 

	//! Returns the time of each frame
	TeTimeInterval time(int frame) { return ((series_[frame-1]).time_); }

};

#endif




