
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
/*! \file TeDBConnection.h
    \brief This file contains the class TeDBConnection that defines a database
	connection. 
*/

#ifndef __TERRALIB_INTERNAL_DBCONNECTION_H 
#define __TERRALIB_INTERNAL_DBCONNECTION_H

#include "TeDatabaseFactoryParams.h"
#include <string>

class TeDatabase;

///!  A class to represent a database connection.
/*!  
	 This class defines a database connection.

	 \sa
	 TeDatabase
*/
class TL_DLL TeDBConnection
{

protected:
	
	//! A number to identify a database connection
	int id_;
	
	//! Flag to indicate if this connection is in use or not
	bool driverInUse_;

	//! Flag to indicate if this connection can be shared by more than one user
	bool isShared_;
	
	//! A Database pointer
	TeDatabase* db_;

	//! The name of the DMBS 
	std::string dbmsName_;

	//! The host name
	std::string host_;

	//! The database name
	std::string databaseName_;
	
	//! The user name
	std::string user_;

	//! The user passwor
	std::string password_;

	//! The user password
	int portNumber_;

public:

	//! Empty constructor
	TeDBConnection(const bool& isShared=true);

	//! Constructor
	TeDBConnection(const std::string& hostName, const std::string& databaseName, 
		const std::string& userName, const std::string& dbmsName, const int& portNumber = -1,
		const std::string& userPasswd = "", const int& conId = -1, TeDatabase* db = 0, const bool& isShared=true);

	//! Constructor
	TeDBConnection(const int& conId, TeDatabase* db = 0, const bool& isShared=true);

	//! Constructor
	TeDBConnection(const std::string& dbKey, const bool& isShared=true);

	//! Copy constructor
	TeDBConnection(const TeDBConnection& other);

	//! Operator =
	TeDBConnection& operator=(const TeDBConnection& other);

	//! Destructor
	virtual ~TeDBConnection();

	//! Equal operator
	bool operator==(const TeDBConnection& other);

	//! Clear 
	void clear();

	//! Set the connection identity
	virtual void setId(const int& id);
	//! Get the connection identity
	virtual int	getId();

	//! Verify if this connection is in use
	virtual bool isInUse();
	//! Set if this connection is in use or not
	virtual void setInUse(const bool& inUse);

	//! Verify if this connection is shared
	virtual bool isShared();
	//! Set if this connection is shared or not
	virtual void setShared(const bool& isShared);

	//! Get the database pointer
	virtual TeDatabase* getDatabase();
	//! Set the database pointer
	virtual void setDatabase(TeDatabase* db);

	//! Returns the name of the DBMS
	virtual std::string getDbmsName(); 
	//! Sets the name of the DBMS
	virtual void setDbmsName(const std::string& dbmsName);

	//! Returns the host name 
	virtual std::string getHost();
	virtual void setHost(const std::string& host);

	//! Returns the database name.
	virtual std::string getDatabaseName();
	//! Sets the database name.
	virtual void setDatabaseName(const std::string& databaseName);

	//! Returns user name
	virtual std::string getUser();
	//! Sets the user name
	virtual void setUser(const std::string&  user);

	//! Returns the user password
	virtual std::string getPassword();
	//! Sets the user password	
	virtual void setPassword(const std::string& password);

	//! Returns the port number
	virtual int getPortNumber();
	//! Sets the port number
	virtual void setPortNumber(const int& port);

	//! Returns the database key (with user password)
	virtual std::string getDbKey();
	
	//! Fills the connection parameters (dbms name, database name, host, port number, user and password) based on the database key given
	virtual void fillDBConnectionInfo(const std::string& dbKey);

	//! Lock this connection setting it as in use
	virtual void lock();

	//! Opens a database connection based on the connection parameters
	virtual bool connect();
	
	//! Creates a new database and the terralib data model 
	virtual bool createDatabaseModel();

	//! Return the connection parameters
	virtual TeDatabaseFactoryParams asDatabaseFactoryParams() const;

	//! Loads a specific database connection from a database
	static bool load(TeDatabase* sourceDB, const int& dbId, TeDBConnection& dbConn);

	//! Loads all database connections from database and put them in the database connection pool.
	static bool load(TeDatabase* sourceDB);

	//! Save a database connection in the database 
	static bool save(TeDatabase* sourceDB, TeDBConnection& dbConn);

	//! Remove a database connection in the database 
	static bool remove(TeDatabase* sourceDB, TeDBConnection& dbConn);

	/** \brief Creates the table where we store a list of connections to other databases.
		\param sourceDB A connection to a TerraLib database that stores information about others databases that can be used for example by remote themes. (Input)
        \return Returns 1 if the table was created, -1 if it already exists and 0 on error.
     */
    static int createDBConnectionTable(TeDatabase* sourceDB);

	//! Updates the database connection identity from database
	static bool updateId(TeDatabase* sourceDB, TeDBConnection& dbConn);

private:
	//! Creates a new opened database connection based on the connection parameters
	virtual bool createDatabaseConnection();

};


#endif 


