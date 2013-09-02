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

#include "TeTemporalSeries.h"
#include "TeDatabase.h"


TeTSEntry::TeTSEntry(const double& val, TeTimeInterval& time, int timeFrame, int nItens, TeTSDataType dType)
{
	timeFrame_= timeFrame;
	value_ = val;
	type_ = dType;
	nItens_ = nItens;
	time_ = time;
	timeInt_ = -1;

	if (time.intervalChronon()==TeMONTHOFYEAR) 
		timeInt_ = time.getT1().month();
	else if(time.intervalChronon()==TeDAYOFMONTH)
		timeInt_ = time.getT1().day();
	else if(time.intervalChronon()==TeDAYOFWEEK)
		timeInt_ = time.getT1().weekDay();
	else if(time.intervalChronon()==TeDAYOFYEAR)
		timeInt_ = time.getT1().yearDay();
	else if(time.intervalChronon()==TeHOUROFDAY)
		timeInt_ = time.getT1().hour();
	else if(time.intervalChronon()==TeMINUTEOFHOUR)
		timeInt_ = time.getT1().minute();
	else if(time.intervalChronon()==TeSECONDOFMINUTE)
		timeInt_ = time.getT1().hour();
}



TeTSEntry& 
TeTSEntry::operator=(const TeTSEntry& other)
{
	if ( this != &other )
	{
		timeFrame_ = other.timeFrame_;
		value_ = other.value_;
		time_ = other.time_;
		nItens_ = other.nItens_;
		type_ = other.type_;
		timeInt_ = other.timeInt_;
	}
	return (*this);
}

TeTSParams& 
TeTSParams::operator=(const TeTSParams& other)
{
	if ( this != &other )
	{
		type_ = other.type_;
  		chronon_ = other.chronon_;
  		time_ = other.time_; 
  		timeOper_ = other.timeOper_;
  		spatOper_ = other.spatOper_;
  	  	orTheme_ = other.orTheme_;
		objectId_ = other.objectId_;
  		attrTable_ = other.attrTable_;	
		attrColumn_ = other.attrColumn_;
	}
	return (*this);
}


TeTemporalSeries& 
TeTemporalSeries::operator=(const TeTemporalSeries& other)
{
	if ( this != &other )
	{
		TSparams_ = other.TSparams_;
		series_ = other.series_;
	}
	return (*this);
}


bool 
TeTemporalSeries::getTSEntry(TeTSEntry& ts, unsigned int frame)
{
	if(frame >= (series_.size()))
		return false;

	ts = series_[frame];
	return true;
}

bool 
TeTemporalSeries::buildFrameIntervals()
{
	// ----- information about temporal table 
	TeTable tempTable;
	TeDatabase* db = TSparams_.orTheme_->layer()->database();

	if(!TSparams_.orTheme_->getTemporalTable(tempTable))
		return false;

	TSparams_.attrTable_ = tempTable.name();
	string initialtime = tempTable.name() +"."+ tempTable.attInitialTime(); 
	string finaltime = tempTable.name() +"."+ tempTable.attFinalTime(); 

	TeTime iTime, fTime;
	
	// ----- fill time interval
	// ---------------------------- you don´t know how many time frames will be generated

	if(	(TSparams_.chronon_==TeYEAR)   || (TSparams_.chronon_==TeMONTH) ||
		(TSparams_.chronon_==TeDAY)    || (TSparams_.chronon_==TeHOUR)  ||
		(TSparams_.chronon_==TeMINUTE) || (TSparams_.chronon_==TeSECOND) )
	{
		
		string fromClause, whereClause;
		if(!db->tableExist(TSparams_.orTheme_->collectionAuxTable()))
		{
			TeAttrTableVector atts; 
			TSparams_.orTheme_->getAttTables(atts); 
			if(atts.empty())
				return false;

			fromClause = " FROM " + tableJoin(atts);
			whereClause = TSparams_.orTheme_->sqlWhereRestrictions();
		}
		else
		{
			//fromClause = TSparams_.orTheme_->sqlGridFrom();
			string collAuxTable = TSparams_.orTheme_->collectionAuxTable();
			string uniqueIdName = tempTable.name() +"."+ tempTable.uniqueName(); 
			string objectIdName = tempTable.name() +"."+ tempTable.linkName(); 

			fromClause = " FROM "+ tempTable.name()+" RIGHT JOIN "+ collAuxTable; 
			
			if(tempTable.tableType()==TeFixedGeomDynAttr)
			{
				fromClause += " ON "+ uniqueIdName +" = ";
				fromClause += collAuxTable +".aux0";
			}
			else
			{
				fromClause += " ON "+ objectIdName +" = ";
				fromClause += collAuxTable +".object_id";
			}
		}

		string sql = " SELECT min("+ initialtime +"), max("+ finaltime +") ";
		sql += fromClause;	
		
		if(!whereClause.empty())
			sql += " WHERE "+ whereClause;

		TeDatabasePortal* portal = db->getPortal();
		if(!portal)
			return false;

		if((!portal->query(sql)) || (!portal->fetchRow()))
		{
			delete portal;
			return false;
		}

		//fill the temporal series
        TeTime date1 =  portal->getDate(0);
        TeTime date2 =  portal->getDate(1);
		TeTimeInterval interval(date1, date2);
		interval.intervalChronon(TSparams_.chronon_); //min/max

		iTime = interval.getT1();
 		fTime = interval.getT2();

		delete (portal);
	}

	// ---------------------------- you know how many time frames will be generated

	else if (TSparams_.chronon_==TeMONTHOFYEAR) 
	{
		TeTime t1("01/01/2001", TeMONTHOFYEAR, "DDsMMsYYYY");
		TeTime t2("01/12/2001", TeMONTHOFYEAR, "DDsMMsYYYY");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeDAYOFMONTH)
	{
		TeTime t1("01/01/2001", TeDAYOFMONTH, "DDsMMsYYYY");
		TeTime t2("31/01/2001", TeDAYOFMONTH, "DDsMMsYYYY");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeDAYOFWEEK)
	{
		TeTime t1("07/01/2001", TeDAYOFWEEK, "DDsMMsYYYY");
		TeTime t2("13/01/2001", TeDAYOFWEEK, "DDsMMsYYYY");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeDAYOFYEAR)
	{
		TeTime t1("01/01/2001", TeDAYOFYEAR, "DDsMMsYYYY");
		TeTime t2("31/12/2001", TeDAYOFYEAR, "DDsMMsYYYY");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeHOUROFDAY)
	{
		TeTime t1("01/01/2001 00", TeHOUROFDAY, "DDsMMsYYYYsHH");
		TeTime t2("01/01/2001 23", TeHOUROFDAY, "DDsMMsYYYYsHH");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeMINUTEOFHOUR)
	{
		TeTime t1("01/01/2001 00:00", TeMINUTEOFHOUR, "DDsMMsYYYYsHHsmm");
		TeTime t2("01/01/2001 00:59", TeMINUTEOFHOUR, "DDsMMsYYYYsHHsmm");
		iTime = t1;
		fTime = t2;
	}
	else if(TSparams_.chronon_==TeSECONDOFMINUTE)
	{
		TeTime t1("01/01/2001 00:00:00", TeSECONDOFMINUTE, "DDsMMsYYYYsHHsmmsSS");
		TeTime t2("01/01/2001 00:00:59", TeSECONDOFMINUTE, "DDsMMsYYYYsHHsmmsSS");
		iTime = t1;
		fTime = t2;
	}

	int timeFrame = 0;
	while (iTime <= fTime)
	{
		TeTimeInterval inter(iTime,iTime);
		inter.intervalChronon(TSparams_.chronon_); //min/max
		TeTSEntry tsEntry(0., inter, timeFrame);
		this->series_.push_back(tsEntry);
		++iTime;
		++timeFrame;
	}

	return true;
}

bool 
TeTemporalSeries::setTSEntryValue(unsigned int frame, double val, TeTSDataType type)
{

	TeTSEntry entry;
	if(!getTSEntry(entry, frame))
		return false;

	double val1 = entry.value_;
	double result = val1;

	if(TSparams_.timeOper_==TeCOUNT)
		result += 1;
	else if (TSparams_.timeOper_==TeSUM)
		result += val;
	else if ((TSparams_.timeOper_==TeMAXVALUE) && (val>val1))
		result = val;
	else if((TSparams_.timeOper_==TeMINVALUE) && (val<val1))
		result = val;
	else if(TSparams_.timeOper_==TeMEAN)
		result = (val1+val)/2;

	series_[frame].value_ = result;
	series_[frame].type_ = type;
	return true;
}




