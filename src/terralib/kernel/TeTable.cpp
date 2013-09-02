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

#include "TeTable.h"
#include "TeUtils.h"
#include <iostream>

using namespace std;


string tableJoin(TeAttrTableVector& vecTable, string geomTable, string attrLink)
{
	string parClause = "";
	string fromResult = "";
	string firstTable = "";
	string cJoin = "";

	TeAttrTableVector::iterator it; 
	
	if(geomTable.empty())
	{
		//find the first static table
		it = vecTable.begin();
		while(it!=vecTable.end())
		{
			if((*it).tableType() != TeAttrExternal)
			{
				firstTable = (*it).name();
				attrLink = (*it).linkName();
				break;
			}
			++it;
		}
		cJoin = " LEFT "; //the first table is a attribute table
	}
	else
	{
		firstTable = geomTable;
		cJoin = " LEFT "; //the first table is a geometry table
	}
		
	
	fromResult += firstTable; 
	
	
	//if don´t exist a table static or temporal 
	if(firstTable.empty() || attrLink.empty())	
		return "";
			
	it = vecTable.begin();

	while(it!=vecTable.end())
	{
		if( ((*it).name()!=firstTable) && ((*it).tableType()!=TeAttrExternal))
		{
			fromResult += " "+ cJoin +" JOIN "+ (*it).name() +" ON "+ firstTable;
			fromResult += "."+ attrLink +" = "+ (*it).name() +"."+ (*it).linkName() +")";
			parClause += "(";
		}

		else if ((*it).name()!=firstTable)
		{
			int idTableStatic = (*it).relatedTableId();
			string colTableStatic = (*it).relatedAttribute();
			string nameTableStatic;   

			//verify if static table is in the vector of the attribute tables
			for(unsigned int j=0; j<vecTable.size(); j++)
			{
				if(vecTable[j].id()==idTableStatic)
				{
					nameTableStatic = vecTable[j].name();
					break;
				}
			}

			if(nameTableStatic.empty())
				return "";

			fromResult += " "+ cJoin +" JOIN "+ (*it).name() +" ON "+ nameTableStatic +"."+ colTableStatic;
			fromResult += " = " + (*it).name() +"."+ (*it).linkName() +")";
			parClause += "(";
		}

		++it;
		cJoin = " LEFT ";
	}

	return (parClause + fromResult);
}


// ---- Handle class - TeTable

TeTable::TeTable():
	id_(-1),
	name_(""),
	type_(TeAttrStatic),
	order_(-1),
	attLink_(""),
	attUnique_(""),
	separator_(','),
	attInitialTime_(""),
	attFinalTime_(""),
	attTimeUnit_(TeSECOND),
	relatedTableId_(-1),
	relatedAttribute_("")
{
	pImpl_ = new TeTableImpl;
	pImpl_->refCount_ = 1;	
}

TeTable::TeTable(const string& name):
	id_(-1),
	name_(name),
	type_(TeAttrStatic),
	order_(-1),
	attLink_(""),
	attUnique_(""),
	separator_(','),
	attInitialTime_(""),
	attFinalTime_(""),
	attTimeUnit_(TeSECOND),
	relatedTableId_(-1),
	relatedAttribute_("")
{
	pImpl_ = new TeTableImpl;
	pImpl_->refCount_ = 1;	
}

TeTable::TeTable(const string& name, const TeAttributeList& attList, const string& uniqueName,
				 const string& linkName, TeAttrTableType tableType):
	id_(-1),
	name_(name),
	type_(tableType),
	order_(-1),
	attList_(attList),
	attLink_(linkName),
	attUnique_(uniqueName),
	separator_(','),
	attInitialTime_(""),
	attFinalTime_(""),
	attTimeUnit_(TeSECOND),
	relatedTableId_(-1),
	relatedAttribute_("")
{
	pImpl_ = new TeTableImpl;
	pImpl_->refCount_ = 1;	
}

TeTable::~TeTable()
{
	if ( --(pImpl_->refCount_) <= 0 )
		delete pImpl_;	
	attList_.clear();
}

// Copy constructor
// copies the representation pointer
// increments the reference counter

TeTable::TeTable(const TeTable& other)
{
	id_= other.id_;
	name_ = other.name_;
	type_ = other.type_;
	attLink_ = other.attLink_;
	attUnique_ = other.attUnique_;
	separator_ = other.separator_;
	order_ = other.order_;
	attInitialTime_ = other.attInitialTime_;
	attFinalTime_ = other.attFinalTime_;
	attTimeUnit_ = other.attTimeUnit_;
	attList_.clear();
	attList_.resize(other.attList_.size());
	attList_=other.attList_;
	relatedTableId_ = other.relatedTableId_;
	relatedTableName_ = other.relatedTableName_;
	relatedAttribute_ = other.relatedAttribute_;
	pImpl_ = other.pImpl_;
	pImpl_->refCount_++;
}

// Operator =
// Copies the representation pointer
// Decrements the reference counter of the current object
// Increments the reference counter for the new object
TeTable& 
TeTable::operator=(const TeTable& rhs)
{
	if ( this != &rhs )
	{
		rhs.pImpl_->refCount_++;
	
		if ( --(pImpl_->refCount_) <= 0 )
			delete pImpl_;
		pImpl_ = rhs.pImpl_;

		separator_ = rhs.separator_;
		attLink_ = rhs.attLink_;
		attUnique_ = rhs.attUnique_;
		order_ = rhs.order_;
		attInitialTime_ = rhs.attInitialTime_;
		attFinalTime_ = rhs.attFinalTime_;
		attTimeUnit_ = rhs.attTimeUnit_;
		type_ = rhs.type_;
		name_ = rhs.name_;	
		attList_.clear();
		attList_ = rhs.attList_;
		relatedTableId_ = rhs.relatedTableId_;
		relatedTableName_ = rhs.relatedTableName_;
		relatedAttribute_ = rhs.relatedAttribute_;
		id_ = rhs.id_;
	}
	return *this;
}

bool 
TeTable::setTableType( TeAttrTableType attType, int relatedTableId, const string& relatedAttribute)
{ 
	type_ = attType; 
	if ( attType == TeAttrExternal && relatedTableId > 0 && !relatedAttribute.empty())
	{
		relatedTableId_ = relatedTableId;
		relatedAttribute_ = relatedAttribute;

		return true;
	}
	return false;
}

void
TeTable::add ( const TeTableRow& row )
{
	pImpl_->add ( row );
}

void
TeTable::setValue (int row, int col, string& val)
{
	pImpl_->setValue (row,col,val);
}

unsigned int
TeTable::size ()
{
	return pImpl_->size();
}

void TeTable::clear ()
{
	pImpl_->clear();
}

TeTableRow
TeTable::operator [] (int i) 
{
	return pImpl_->operator [] ( i );
}

string 
TeTable::operator () ( int row, int col )
{
	return pImpl_->operator () (row, col );
}


bool
TeTable::attrLink(TeAttribute& att)
{	
	TeAttributeList::iterator it = attList_.begin();
	while (it != attList_.end())
	{
		if (TeConvertToUpperCase((*it).rep_.name_) == TeConvertToUpperCase(attLink_))
		{
			att = (*it);
			return true;
		}
		++it;
	}
	return false;
}

bool 
TeTable::attrUnique(TeAttribute& attr)
{
	TeAttributeList::iterator it = attList_.begin();
	while(it!=attList_.end())
	{
		if(TeConvertToUpperCase((*it).rep_.name_) == TeConvertToUpperCase(attUnique_))
		{
			attr = (*it);
			return true;
		}
		++it;
	}
	return false;
}

int
TeTable::attrLinkPosition()
{
	int i = 0;
	TeAttributeList::iterator it = attList_.begin();
	while (it != attList_.end())
	{
		if (TeConvertToUpperCase((*it).rep_.name_) == TeConvertToUpperCase(attLink_))
			return i;
		++it;
		++i;
	}
	return -1;
}

int
TeTable::attrUniquePosition()
{
	int i = 0;
	TeAttributeList::iterator it = attList_.begin();
	while (it != attList_.end())
	{
		if (TeConvertToUpperCase((*it).rep_.name_) == TeConvertToUpperCase(attUnique_))
			return i;
		++it;
		++i;
	}
	return -1;
}

bool 
TeTable::attributeNames(vector<string>& attrs)
{
	attrs.clear();
	TeAttributeList::iterator it = attList_.begin();
	while(it!=attList_.end())
	{
		attrs.push_back((*it).rep_.name_);
		++it;
	}
	return true;
}


void 
TeTable::primaryKeys(vector<string>& keys)
{
	keys.empty();
	TeAttributeList::iterator it = attList_.begin();
	while(it!=attList_.end())
	{
		if((*it).rep_.isPrimaryKey_)
			keys.push_back((*it).rep_.name_);
		++it;
	}
	return;
}
