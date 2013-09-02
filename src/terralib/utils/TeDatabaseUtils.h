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
/*! \file TeDatabaseUtils.h
    \brief This file contains some utilitary functions to extended the TerraLib data model for TerraView like applications.
*/
#ifndef __TERRALIB_INTERNAL_DATABASEUTILS_H
#define __TERRALIB_INTERNAL_DATABASEUTILS_H


#include <TeDatabase.h>
#include <TeExternalTheme.h>
#include <string>
#include <vector>
using std::string;
using std::vector;


/** @defgroup DBUtils Utilitary functions
    @ingroup  DatabaseUtils
	A set of utilitary functions
 *  @{
 */
/**  This function copies the contents of a TerraLib database to another TerraLib database
	\param dbFrom A pointer to the source database
	\param dbTo	  A pointer to the destination database
*/
bool TeCopyDatabase(TeDatabase* dbFrom, TeDatabase* dbTo);


/** Returns true whether a database contains the TerraLib data model
	\param db	pointer do a TerraLib database
*/
bool isTerralibModel(TeDatabase* db);

vector<string> generateItemsInClauseVec(TeTheme* theme, string& where);

vector<string> getObjects(TeTheme* theme, int sel);

vector<string> getItems(TeTheme* theme, int sel);

vector<string> getObjects(TeTheme* theme, vector<string>& itens);

map<string, vector<string> > getObject2ItemsMap(TeTheme* theme, vector<string>& itens);

vector<string> getItems(TeTheme* theme, vector<string>& objcts);

//! Breaks a collection of strings into a series of IN clauses to be used in query expressions
/*
	\params begin Iterator that p oints to the beginning of the collection
	\params end Iterator that points to the end of the collection
	\param db pointer to the database where the query expression will be applied
	\param addPlicae flat to indicate that the character ' should enclose each string
*/
template <typename Iterator>
vector<string>
generateInClauses(Iterator& begin, Iterator& end, TeDatabase* db, bool addPlicae=true)
{
	int i, chunkSize = 200;
	string inClause;
	vector<string> inClauseVector;

	Iterator temp = begin;
	i = 0;
	while (temp != end)
	{
		if (i%chunkSize == 0)
		{
			if (!inClause.empty())
			{
				inClause[inClause.size() - 1] = ')';
				inClauseVector.push_back(inClause);
				inClause.clear();
			}
			inClause = "(";
		}

		if (addPlicae)
			inClause += "'" + db->escapeSequence(*temp) + "',";
		else
			inClause += db->escapeSequence(*temp) + ",";
		i++;
		++temp;
	}
	if (!inClause.empty())
	{
		inClause[inClause.size() - 1] = ')';
		inClauseVector.push_back(inClause);
	}
	return inClauseVector;
}

template <typename Iterator>
vector<string>
generateItemsInClauses(Iterator& begin, Iterator& end, TeTheme* theme)
{
	vector<string> itenVec;
	Iterator temp = begin;
	while (temp != end)
	{
		itenVec.push_back(*temp);
		temp++;
	}

	map<string, vector<string> > objMap = getObject2ItemsMap(theme, itenVec);
	map<string, vector<string> >::iterator mit;
	vector<string>::iterator it;

	int i, chunkSize = 200;
	string inClause;
	vector<string> inClauseVector;
	
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


//@}

#endif

