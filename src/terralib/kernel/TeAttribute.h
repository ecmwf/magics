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
/*! \file TeAttribute.h
    \brief This file contains structures and definitions about attributes of geographical objects
*/
#ifndef  __TERRALIB_INTERNAL_ATTRIBUTE_H
#define  __TERRALIB_INTERNAL_ATTRIBUTE_H

#include "TeDefines.h"
#include "TeDataTypes.h"
#include "TeTime.h"

/*! \enum TeMeasurementScale 
	\brief Sscale of measurement according to Stevens (1949) 
           modified by Chrisman (1998) to include CYCLIC and PROBABILITY
           we also include FUZZY (possibility scale) 
*/
enum TeMeasurementScale 
{ ORDINAL, NOMINAL, RATIO, INTERVAL, CYCLIC, PROBABILITY, FUZZY };

/*! \struct TeAttributeRep
	\brief Attribute physical representation
*/
struct TL_DLL TeAttributeRep
{
	string			name_;		//!< attribute name	
	TeAttrDataType  type_;		//!< attribute type
	int				numChar_;   //!< width of an attribute
	int				decimals_;	//!< number of decimal digits
	bool			isPrimaryKey_;	//!< flag to indicate that the attribute is part of primary key
	bool			isAutoNumber_; 	//!< flag to indicate that the attribute is auto number
	bool            null_;			//!< flag to indicate that attribute can be a null value (true) or not (false)
	string			defaultValue_;	//!< default value (without "'")

	//! Empty constructor
	TeAttributeRep():
		name_(""),
		type_(TeSTRING),
		numChar_(0),
		decimals_(0),
		isPrimaryKey_(false),
		isAutoNumber_(false),
		null_(true),
		defaultValue_("")
		{}

	//! Constructor
	TeAttributeRep(const string& name):
		name_(name),
		type_(TeSTRING),
		numChar_(0),
		decimals_(0),
		isPrimaryKey_(false),
		isAutoNumber_(false),
		null_(true),
		defaultValue_("")
		{}

	//! Operator =
	TeAttributeRep& operator= ( const TeAttributeRep& at )
	{
		if ( this != &at )
		{	
			name_ = at.name_;
			type_ = at.type_;
			numChar_ = at.numChar_;
			decimals_ = at.decimals_;
			isPrimaryKey_ = at.isPrimaryKey_;
			isAutoNumber_ = at.isAutoNumber_;
			null_ = at.null_;
			defaultValue_ = at.defaultValue_;
		}
		return *this;
	}

	//! Operator ==
	bool operator== ( const TeAttributeRep& at )
	{
		return (name_==at.name_ && type_ == at.type_
			&& numChar_ == at.numChar_ && decimals_ == at.decimals_ &&
            isPrimaryKey_ == at.isPrimaryKey_ && 
			isAutoNumber_ == at.isAutoNumber_ && 
			null_ == at.null_ && defaultValue_ == at.defaultValue_);
	}

	//! Operator <
	bool	operator< (const TeAttributeRep& at) const 
	{return (name_ < at.name_);}	
};

/*! \struct TeAttribute
	\brief Attribute description
*/
struct TL_DLL TeAttribute
{
	TeAttributeRep	rep_;		//!< representation of attribute	
	string	semantic_;			//!< reference in a Ontology database (e.g., entry in WordNet )	
	string	unit_;				//!< measurement unit ( e.g., m ) if applicable
	TeMeasurementScale  scale_;	//!< scale of measurement

	// for RATIO data sets
	string				minValue_;	//!< minimum value of the attribute
	string				maxValue_;	//!< maximum value of the attrbute	

	// for NOMINAL or ORDINAL data sets
	vector<string>		validValueList_; //!< list of valid values

	// for INTERVAL data sets (??)
	string				origin_;	//!< origin of the intervals	
	string				interval_;	//!< mesurement interval

	string				dateTimeFormat_;	//!< format for date and time values
	string				indicatorAM_;		//!< AM indicator for a 12 hour clock
	string				indicatorPM_;		//!< PM indicator for a 12 hour clock
	string				dateSeparator_;		//!< date separator
	string				timeSeparator_;		//!< time separator
	TeChronon			dateChronon_;		//!< date chronon

	//! Empty constructor
	TeAttribute():
		rep_			(TeAttributeRep()),
		dateTimeFormat_	("DsMsYYYYsHHsmmsSS"), 
		indicatorAM_	("AM"),
		indicatorPM_	("PM"),
		dateSeparator_	("/"),
		timeSeparator_	(":"), 
		dateChronon_    (TeSECOND)
		{}
};

/*! \struct TeProperty
	\brief A property of an object
 */
struct TL_DLL TeProperty
{
	TeAttribute		attr_;		//!< attribute description
    string			value_;		//!< its value stored as an string
};

//! A vector of TeProperties
typedef vector<TeProperty> TePropertyVector;

//! A vector of attributes representation
typedef vector<TeAttributeRep>  TeAttributeRepList;

//! A vector of attributes 
typedef vector<TeAttribute>		TeAttributeList;

//! A Map of the attribute names to the statistical types 
typedef vector< pair<TeAttributeRep, TeStatisticType> > TeGroupingAttr;

#endif


