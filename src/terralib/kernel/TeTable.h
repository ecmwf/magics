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
/*! \file TeTable.h
    \brief This file contains class TeTable definitions.
*/

#ifndef  __TERRALIB_INTERNAL_TABLE_H
#define  __TERRALIB_INTERNAL_TABLE_H

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include <string>
#include <vector>
#include "TeComposite.h"
using namespace std;

#include "TeAttribute.h"

class TeTableImpl;

//! A table row is a vector of strings
typedef vector<string> TeTableRow;

//! The table implementation, this is the body class, of the handle/body idiom
/*!
	All members are private. This class is used only through the TeTable class
*/
class TL_DLL TeTableImpl: public TeComposite<TeTableRow>
{

friend class TeTable;

	int refCount_;

	//! Empty constructor
	TeTableImpl(): refCount_(0){}

	//! Returns the value of a cell in table
	string operator () ( int row, int col )
	{
		return components_[row].operator[] ( col );
	}

	void setValue (int row, int col, string& val)
	{
		TeTableRow tRow = components_[row];
		if (col >= (int)(tRow.size()))
			tRow.resize(col+1);
		tRow[col] = val;
		components_[row] = tRow;
	}

};

//! An attribute table in memory
/*!
	Instances of this classes represent non-spatial tables, or tables
	that don´t have a spatial data as one of its columns.
	The class stores the table schema and it also can store rows of data.

	Rows of data are stored according to handle/hody idiom, to make
	copies optimized.

  \sa
  TeAttribute TeTableImpl
*/
class TL_DLL TeTable
{
public:
	//! Empty constructor
	/*!
		\note
		Instance of TeTable will be created with the following default values:
		\arg name: "" (empty)
		\arg id_:  -1 (no database identifier)
        \arg type_: static attribute table
		\arg attList: empty list of attributes
        \arg attLink_:	"" (no index to geometries)
        \arg attUnique_: "" (no unique values)
		\arg separator_: ','  comma separator (used to read ASCII tables from files)
		\arg attInitialTime_: ""  no initial time information
		\arg attFinalTime_: ""  no final time information
		\arg attTimeUnit_: TeYear 
		\arg relatedTableId: -1 (no relation to other table)
		\arg relatedTableName: "" (name of the related table)
		\arg relatedAttribute: "" (attribute of the related table)
	*/
	TeTable();

	//! Constructor by name
	/*!
		\param name table name
		\note
		Instance of TeTable will be created with the following default values:
		\arg id_:  -1 (no database identifier)
        \arg tableType_: static attribute table
		\arg attList: empty list of attributes
        \arg attLink_:	"" (no index to geometries)
        \arg attUnique_: "" (no unique values)
		\arg separator_: ','  comma separator (used to read ASCII tables from files)
		\arg attInitialTime_: ""  no initial time information
		\arg attFinalTime_: ""  no final time information
		\arg attTimeUnit_: TeYear 
		\arg relatedTableId: -1 (no relation to other table)
		\arg relatedTableName: "" (name of the related table)
		\arg relatedAttribute: "" (attribute of the related table)
	*/
	TeTable(const string& name);

	//! Constructor by name and attribute list
	/*!
      \param name table name
      \param attList list of attributes
	  \param uniqueName column that unique attributes to be used as a primary key
	  \param linkName column that links attributes to geometries
	  \param tableType type of attribute table
	  \note
		Instance of TeTable will be created with the following default values:
	   \arg id_:  -1 (no database identifier)
	   \arg separator_: ','  comma separator (used to read ASCII tables from files)
	   \arg attInitialTime_: ""  no initial time information
	   \arg attFinalTime_: ""  no final time information
	   \arg attTimeUnit_: TeYear 
	   \arg relatedTableId: -1 (no relation to other table)
	   \arg relatedTableName: "" (name of the related table)
	   \arg relatedAttribute: "" (attribute of the related table)
	*/
	TeTable(const string& name, const TeAttributeList& attList, const string& uniqueName,
			const string& linkName="", TeAttrTableType tableType=TeAttrStatic);

	 //! Destructor
	~TeTable();

	//! Copy Constructor
	/*! Copies the representation pointer increments the reference counter	*/
	TeTable(const TeTable&);

	//! Operator =
	/*!
		Copies the representation pointer
		Decrements the reference counter of the current object
		Increments the reference counter for the new object
	*/
	TeTable& operator=(const TeTable&);

// -- Methods

	//! Returns the table name
	string name()
	{ return name_; }

	//! Sets the table name
	void name(const string& n)
	{ name_ = n; }

	//! Returns the unique id of the table in a database
	int id() { return id_; }

	//! Sets the id of the table
	void setId(int n) { id_ = n; }

	//! Set the position of the table in a join
	void setOrder(int n) { order_ = n; }

	//! Get the position of the table in a join
	int getOrder() { return order_;}
	
	//! Defines the list of attributes of this table
	void setAttributeList ( const TeAttributeList& attList)
	{ attList_ = attList; }

	//! Retrieves the list of attributes of this table
	TeAttributeList& attributeList()
	{ return attList_; }

	//! Retrieves the list of attribute names of this table
	bool attributeNames(vector<string>& attrs);
	
	//! Returns the type of this attribute table
	TeAttrTableType tableType()
	{ return type_; }

	//! Sets the table separator
	void setSeparator ( const char& c )
	{ separator_ = c ; }

	//! Returns the table separator
	char separator ()
	{ return separator_; }

	//! Sets the table type
	/*!
      \param attType table type
	  \note In case of external tables
      \param relatedTableId is the id of the related table 
	  \param relatedAttribute is column name of the related table
	*/
	bool setTableType( TeAttrTableType attType, int relatedTableId=-1, const string& relatedAttribute="");

	//! Sets an attribute as the table index.
	/*!
		Index in this context, means an attribute that is a link between
		the attribute table and a spatial table (a table of geometrical
		representation of objects).
	*/
	void setLinkName ( const string& linkName ) { attLink_ = linkName;}

	//! Returns the name of the column used as link between the attribute table and a spatial table
	string linkName()
	{	return attLink_; }

	//! Sets the name of the column that has unique values
	void setUniqueName ( const string& uniqueName ) { attUnique_ = uniqueName; }	

	//! Returns the name of the column used as primary key
	string uniqueName()
	{	return attUnique_;	}

	//! If there is one, returns the attribute used as an index
	/*!
		\param att to return the attribute used as an index
		\return TRUE if there is an index, FALSE otherwise
	*/
	bool attrLink(TeAttribute& att);

	//! If there is one, returns the attribute used as primary key
	/*!
		\param attr to return the attribute used as primary key
		\return TRUE if there is an index, FALSE otherwise
	*/
	bool attrUnique(TeAttribute& attr);
	
	//! Returns the position of the link attribute in the attribute list
	int attrLinkPosition();

	//! Returns the position of the unique attribute in the attribute list
	int attrUniquePosition();

	/** @name External tables
	    The following members are used to manipulate information about tables that
		are external: don´t have a direct link to geometries, but can be linked by
		a column to another attribute table
	*/
	//@{
	//! Sets the  id of a table to which this is related (only for TeAttrExternal tables)
	void relatedTableId(int id)
	{   relatedTableId_ = id; }

	//! Returns the id of a related table (only for TeAttrExternal tables)
	int relatedTableId()
	{   return relatedTableId_; }

	//! Sets the name of a table to which this is related (only for TeAttrExternal tables)
	void relatedTableName(string tableName)
	{   relatedTableName_ = tableName; }

	//! Returns the name of a related table (only for TeAttrExternal tables)
	string relatedTableName()
	{   return relatedTableName_; }

	//! Set the name of a column to link to a related table (only for TeAttrExternal tables)
	void relatedAttribute(const string& columnName)
	{	relatedAttribute_ = columnName; }

	//! Return the column that links to a related table (only for TeAttrExternal tables)
	string relatedAttribute()
	{ return relatedAttribute_; }

	//@}

	/** @name Temporal Attributes
	    The following members are used to manipulate the attribute tables with temporal
		information.
		\par Temporal tables contains columns to store a time interval in which the 
		 attributes (or a row) of an object is valid.
	*/
	//@{		

	//! Sets the name of the column that has the initial time of the validity interval
	void attInitialTime(const string& t)
	{	attInitialTime_ = t; }

	//! Returns the name of the column that has the initial time of the validity interval
	string attInitialTime()
	{	return attInitialTime_; }

	//! Sets the name of the column that has the final time of the validity interval
	void attFinalTime(const string& t)
	{	attFinalTime_ = t; }

	//! Returns the name of the column that has the final time of the validity interval
	string attFinalTime()
	{	return attFinalTime_; }

	//! Sets the time unit relative to the validity interval
	void attTimeUnit(TeChronon t)
	{	attTimeUnit_ = t; }

	//! Returns the time unit relative to the validity interval
	TeChronon attTimeUnit()
	{	return attTimeUnit_; }
	//@}

	/** @name Data in Memory
	    The following members are used to manipulate rows of the table in memory
	*/
	//@{
	//! Add a row to a table
	void add ( const TeTableRow& row );

	//! Returns the i-th row
	TeTableRow operator [] ( int row ) ;

	//! Returns the element indexed by (row,col)
	string operator () ( int row, int col );

	//! Sets the value of a cell indexed by row x col to value val
	void setValue (int row, int col, string& val);

	//! Returns the number of rows in a table
	unsigned int size ();

	//! Clears the table storage in memory
	void clear ();
	//@}

	//! Returns the primary key names of the table 
	void primaryKeys(vector<string>& keys);

private:
	
	int					id_;	// unique id from a database where this table may be stored
	string				name_;	// name
	TeAttrTableType		type_;	// type of table
	int					order_; // the table position relative to others in a join 

	TeAttributeList		attList_;		// list of attributes
	string				attLink_;		// attribute that links objects to their spatial representation
										// in case of external table it is used to link to the related table
	string				attUnique_;		// primary key 
	char                separator_;		// in case of CSV table 

// --- The following attributes are relative to tables with temporal information
 
	string		attInitialTime_;	// initial time attribute
	string		attFinalTime_;		// final time attribute
	TeChronon	attTimeUnit_;		// time granularity

// --- The following attributes are relative to external tables
 	int		relatedTableId_;	// id of a table to which this is related	
 	string	relatedTableName_;	// name of a table to which this is related	
	string  relatedAttribute_;	// column name of table which this is related

// -- Table Implementation
	TeTableImpl* pImpl_;		// the rows of a table

};

//! A vector of tables
typedef vector<TeTable> TeAttrTableVector;

//! Return the sql join related with the table vector  
TL_DLL string tableJoin(TeAttrTableVector& vecTable, string firstTable="", string attrLink=""); 

/*! \example createTable.cpp
	Shows how to execute some operations relative to attribute tables.
 */

#endif


