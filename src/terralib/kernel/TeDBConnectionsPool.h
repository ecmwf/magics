
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
/*! \file TeDBConnectionsPool.h
	\brief This file contains the class TeDBConnectionsPool that defines 
	a pool of database connections. 
*/

#ifndef __TERRALIB_INTERNAL_DBCONNECTIONPOOL_H 
#define __TERRALIB_INTERNAL_DBCONNECTIONPOOL_H

#include "TeSingleton.h"
#include "TeDBConnection.h"
#include "TeDatabase.h"

#include <vector>
#include <map>
#include <string>

//! A database connections vector.
typedef std::vector<TeDBConnection> TeDBConnectionsVector;


///!  A class that implements a manager for a database connections pool.
/*!  
	 \sa
	 TeDatabase, TeDBConnection
*/
class TL_DLL TeDBConnectionsPool : public TeSingleton<TeDBConnectionsPool> 
{
public:

	//! Constructor.
	TeDBConnectionsPool(); 
	
	//! Destructor.
	virtual ~TeDBConnectionsPool(); 

	//! Returns an available connection in the pool for the given connection parameters.
	/*!
		Searches one available connection in the pool. If there is, returns this 
		pointer otherwise creates a new connection based on the given descriptor, 
		connects it, sets it "in use" and inserts it in the pool, then returns the new connection.	
		\param desc The database descriptor.
		\return If success returns a connection otherwise returns NULL.
	*/
    TeDatabase* getDatabase(const std::string& hostName, const std::string& databaseName, 
		const std::string& userName, const std::string& dbmsName, const int& portNumber = 0,
		const std::string& userPasswd = ""); 

	//! Returns one available connection in the pool for a given connection index.
	/*!
		Searches one available connection in the pool based on the given index. 
		\param desc The database descriptor.
		\return If success returns a connection otherwise returns NULL.
	*/
	TeDatabase* getDatabase(const int& dbConnectionIndx); 

	//! Returns one available connection in the pool for a given database connection key.
	/*!
		Searches one available connection in the pool. If there is, returns this 
		pointer otherwise creates a new connection based on the given descriptor key, 
		connects it, sets it "in use" and inserts it in the pool, then returns the new connection.
		\param database descriptor key.
		\return If success returns a connection otherwise returns NULL.
	*/
	TeDatabase* getDatabase(const std::string& dbKey); 
		
	//! Releases the given database connection.
	/*!
		Checks if the database connection is in the pool, then set 
		the connection as "available".
		\param db A pointer to a database connection.
	*/
	bool releaseDatabase(TeDatabase* db); 

	//! Releases all connection in the pool.
	/*!
		Sets all connections in the pool as "available".
	*/
	void releaseAllDatabases(); 
	
	//! Inserts a database connection in the pool.
	/*!
		Inserts a database connection in the pool. If the database connection identity
		is equal to -1, this method creates a new identity.
		\param db	the database connection.
	*/
	bool insertDBConnection(TeDBConnection& dr);

	//! Removes the database connection of the pool.
	/*!
		Removes a database connection of the pool. 
		\param db	the database connection.
	*/
	bool removeDBConnection(TeDBConnection& dr);

	//! Returns the connection index for the given connection parameters.
	/*!
		If success returns the found connection index otherwise returns -1.	
		\param desc The connection descriptor.
		\return If success returns a connection index otherwise returns -1.
	*/
	int getDBConnectionIndx(TeDBConnection& dbCon); 
	
	//! Returns the connection index for a given connection key.
	/*!
		If success returns the found connection index otherwise returns -1.	
		\param dbKey The database key.
		\return If success returns a connection index otherwise returns -1.
	*/
	int getDBConnectionIndx(const std::string& dbKey); 

	//! Returns the connection key for a given connection index.
	/*!
		If success returns the found connection key otherwise returns an empty string.	
		\param dbConnectionIndx The connection index.
		\return If success returns a connection key returns an empty string.
	*/
	std::string getDBConnectionKey(const int& dbConnectionIndx); 
		
	//! Closes all connections and clears the pool.
	/*!
		Closes each database connection in the vectors and clear the maps.
	*/
	void clearAllDBConnections(); 
	
	//! Sets the database connection as not available and close it. 
	/*!
		Checks if the database connection is in the pool, 
		then set it as not available and close it. 
		\param db A pointer to a database connection.
	*/
	void closeDBConnection(TeDatabase* db); 
	void closeDBConnection(std::string& dbDescriptorKey); 

	//! Returns how many connections exist in the pool
	unsigned int size() { return dbIndxs_.size(); }

	//! Returns a reference to the connection container
	std::map<int, TeDBConnectionsVector>& getDBConnections() { return dbs_; }


protected:

	//! Checks if the database connection is in the pool.
	/*!
		\param db A pointer to a database connection.
		\return Returns true if the database connection is in the pool 
		otherwise returns false.
	*/
	bool isDBConnectionInPool(TeDatabase* db);  

	//! Searches the first available database connection in the pool for the given index.
	/*!
		\param dbConnectionIndx The connection index.
		\return Returns a valid pointer if there is a valid connected in the pool otherwise return NULL.   
	*/
	TeDBConnection* findDBConnection(const int& dbConnectionIndx); 

	//! Searches the first available database connection in the pool for the given database key.
	/*!
		\param dbkey The database connection key.
		\return Returns a valid pointer if there is a valid connected in the pool otherwise return NULL.   
	*/
	TeDBConnection* findDBConnection(const std::string& dbkey); 

	//! Closes all connections in the given vector and clears it.
	void clearDBConnectionVector(TeDBConnectionsVector& connectionsVector); 

	//! Creates a new database connection based on the given connection parameters, puts it in the pool and returns it
	TeDBConnection* createDBConnection(const std::string& hostName, const std::string& databaseName, 
		const std::string& userName, const std::string& dbmsName, const int& portNumber = 0,
		const std::string& userPasswd = "");

	//! Creates a new database connection based on the given connection parameters, puts it in the pool and returns it
	TeDBConnection* createDBConnection(TeDBConnection& dbCon);

private:

	//! Hash table that maps a database key with a database connections vector.
	std::map<int, TeDBConnectionsVector, std::less<int> >	dbs_;
	std::map< std::string, int, std::less<std::string> >	dbIndxs_;

	//! Creates a new valid connection database identity 
	int createsValidConId();
};

#endif 

