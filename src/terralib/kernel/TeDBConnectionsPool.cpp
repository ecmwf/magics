
#include "TeDBConnectionsPool.h"
#include "TeDBConnection.h"

#include <vector>
#include <map>


TeDBConnectionsPool::TeDBConnectionsPool()  
{ }

TeDBConnectionsPool::~TeDBConnectionsPool() 
{
	clearAllDBConnections();
}

TeDatabase* 
TeDBConnectionsPool::getDatabase(const std::string& hostName, const std::string& databaseName, 
	const std::string& userName, const std::string& dbmsName, const int& portNumber, const std::string& userPasswd) 
{
	TeDatabase* db = 0;
	TeDBConnection* dr = 0;

	TeDBConnection conn(hostName, databaseName, userName, dbmsName, portNumber, userPasswd);
	std::string dbKey = conn.getDbKey();
	
	//Look for a valid connection
	dr = findDBConnection(dbKey);
	if(dr)
	{
		dr->setInUse(true);
		return dr->getDatabase();
	}

	//Creates a new connection and put it in the pool
	dr = createDBConnection(conn);
	if(dr)
	{
		dr->setInUse(true);
		return dr->getDatabase();
	}

	return db;
}

TeDatabase* 
TeDBConnectionsPool::getDatabase(const int& dbDriverIndx) 
{
	TeDatabase* db = 0;
	TeDBConnection* dr = 0;
	
	//Look for a valid connection
	dr = findDBConnection(dbDriverIndx);
	if(dr)
	{
		dr->setInUse(true);
		return dr->getDatabase();
	}
	return db;
}

TeDatabase* 
TeDBConnectionsPool::getDatabase(const std::string& dbDescriptorKey) 
{
	TeDatabase* db = 0;
	TeDBConnection* dr = 0;
	
	//Look for a valid connection
	dr = findDBConnection(dbDescriptorKey);
	if(dr)
	{
		dr->setInUse(true);
		return dr->getDatabase();
	}

	//Creates a new connection and put it in the pool
	TeDBConnection dbCon;
	dbCon.fillDBConnectionInfo(dbDescriptorKey);
	dr = createDBConnection(dbCon);
	if(dr)
	{
		dr->setInUse(true);
		return dr->getDatabase();
	}
	return db;
}
bool TeDBConnectionsPool::releaseDatabase(TeDatabase* db) 
{
	if (!db)
		return false;

	std::string dbKey = db->getDatabaseDescription();	

	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbKey);
	if(itKey == dbIndxs_.end())
		return false;

	//Look for this database in the database connection vector
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator itAux = dbs_.find(itKey->second);
	if(itAux==dbs_.end())
		return false;

	TeDBConnectionsVector::iterator it;
	for (it = itAux->second.begin(); it != itAux->second.end(); ++it)
	{			
		TeDatabase* aux_db = it->getDatabase();
		if (aux_db == db)
		{
			it->setInUse(false);
			return true;
		}
	}
	return false;
}

void TeDBConnectionsPool::releaseAllDatabases() 
{	
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.begin();
	while(it!=dbs_.end())
	{
		TeDBConnectionsVector::iterator itCon = it->second.begin();
		while(itCon!=it->second.end())
		{
			itCon->setInUse(false);
			++itCon;
		}
		++it;
	}
	return;
}

bool TeDBConnectionsPool::insertDBConnection(TeDBConnection& dr)
{
	int indx=0;
	//creates the database key based on the connection parameters
	std::string dbKey = dr.getDbKey();
	
	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbKey);
	if(itKey == dbIndxs_.end())
	{
		//creates a new entity in the both maps
		indx = dr.getId();
		if(indx<0)
		{
			//look for a valid database connection id
			indx = createsValidConId();  
			dr.setId(indx);
		}
				
		TeDBConnectionsVector vec;
		vec.push_back(dr);
		dbs_.insert(std::pair<int, TeDBConnectionsVector>(indx, vec));
		dbIndxs_.insert(std::pair<std::string, int>(dbKey, indx));
		return true;
	}

	indx = itKey->second;
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.find(indx);
	if(it == dbs_.end())
		return false;
	
	//This implementation supports only one connection by database descriptor
	//because of the maps in the TeDatabase (layerMap, ThemeMap, etc...)
	clearDBConnectionVector(it->second); 
	it->second.push_back(dr);
	return true;
}

bool TeDBConnectionsPool::removeDBConnection(TeDBConnection& dr)
{
	//creates the database key based on the connection parameters
	std::string dbKey = dr.getDbKey();
	
	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbKey);
	if(itKey==dbIndxs_.end())
		return false;

	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.find(itKey->second);
	if(it==dbs_.end())
		return false;

	TeDBConnectionsVector::iterator itVec = it->second.begin();
	while(itVec!=it->second.end())
	{
		if((*itVec) == dr)
		{
			it->second.erase(itVec);
			continue;
		}
		++itVec;
	}

	if(it->second.size()>0)
		return true;

	dbs_.erase(it);
	dbIndxs_.erase(itKey);
	return true;
}

int 
TeDBConnectionsPool::getDBConnectionIndx(TeDBConnection& dbCon) 
{
	string dbKey = dbCon.getDbKey();
	int status = this->getDBConnectionIndx(dbKey);
	return status;
}

int 
TeDBConnectionsPool::getDBConnectionIndx(const std::string& dbDescriptorKey) 
{
	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbDescriptorKey);
	if (itKey != dbIndxs_.end())
		return itKey->second;
	return -1;
}

std::string
TeDBConnectionsPool::getDBConnectionKey(const int& dbConnectionIndx) 
{
	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.begin();
	while(itKey!=dbIndxs_.end())
	{
		if(itKey->second == dbConnectionIndx)
			return itKey->first;
		++itKey;
	}
	return "";
}

void TeDBConnectionsPool::clearAllDBConnections()  
{
	//clean the the drivers pool
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.begin();
	while(it!=dbs_.end())
	{
		clearDBConnectionVector(it->second);
		++it;
	}

	dbs_.clear();
	dbIndxs_.clear();
}

void TeDBConnectionsPool::closeDBConnection(TeDatabase* db) 
{
	if(!db)
		return;
	releaseDatabase(db);
	db->close();		
}

void TeDBConnectionsPool::closeDBConnection(std::string& dbDescriptorKey) 
{
	TeDBConnection* dr = findDBConnection(dbDescriptorKey);
	if (!dr)
		return; 
	
	dr->setInUse(false);
	TeDatabase* db = dr->getDatabase();
	if (db)
		db->close();		
}

//protected

bool TeDBConnectionsPool::isDBConnectionInPool(TeDatabase* db) 
{
	if(!db)
		return false;

	std::string dbKey = db->getDatabaseDescription();	

	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbKey);
	if(itKey == dbIndxs_.end())
		return false;

	//Look for this database in the database connection vector
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator itAux = dbs_.find(itKey->second);
	if(itAux==dbs_.end())
		return false;

	TeDBConnectionsVector::iterator it;
	for (it = itAux->second.begin(); it != itAux->second.end(); ++it)
	{			
		TeDatabase* aux_db = it->getDatabase();
		if (aux_db == db)
			return true;
	}
	return false;
}

TeDBConnection* 
TeDBConnectionsPool::findDBConnection(const int& dbDriverIndx)  
{
	TeDBConnection* dr = 0;		
	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.find(dbDriverIndx);
	if(it==dbs_.end())
		return dr; // There are no database connection to this index 
	
	//Find one databases connection
	TeDBConnectionsVector::iterator it2;
	for (it2 = it->second.begin(); it2 != it->second.end(); ++it2)
	{			
		//Verify if this connection is in use. If it is, continue to look for another connection.
		if (it2->isInUse() && !it2->isShared())
			continue;
		
		TeDatabase* db = it2->getDatabase();
		if((!db) || (!db->isConnected()))
		{
			it2->connect();	 //Open this db connection based on its descriptor
			db = it2->getDatabase();
		}
		
		if (db->isConnected())
			return &(*it2);
	}
	return dr;
}

TeDBConnection* 
TeDBConnectionsPool::findDBConnection(const std::string& dbkey) 
{
	TeDBConnection* db = 0;		
	std::map< std::string, int, std::less<std::string> >::iterator itKey = dbIndxs_.find(dbkey);
	if (itKey != dbIndxs_.end())
		return findDBConnection(itKey->second);
	return db;
}

void TeDBConnectionsPool::clearDBConnectionVector(TeDBConnectionsVector& connections) 
{
	//the clear method calls the destructor of all TeDBConnection 
	connections.clear();
}

TeDBConnection* 
TeDBConnectionsPool::createDBConnection(const std::string& hostName, const std::string& databaseName, 
		const std::string& userName, const std::string& dbmsName, const int& portNumber, const std::string& userPasswd)
{
	TeDBConnection dbCon(hostName, databaseName, userName, dbmsName, portNumber, userPasswd);
	return createDBConnection(dbCon);
}

TeDBConnection* 
TeDBConnectionsPool::createDBConnection(TeDBConnection& dbCon)
{
	TeDBConnection* con = 0;
	if(dbCon.getId()<0)
	{
		int indx = createsValidConId();
		dbCon.setId(indx);
	}
	
	if(!dbCon.connect()) //create a new opened database connection 
		return con;
	
	//Insert this new connection in the maps
	if(!insertDBConnection(dbCon))
		return con;

	std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.find(dbCon.getId());
	if(it==dbs_.end())
		return con;
    
	TeDBConnectionsVector::iterator itVec = it->second.begin();
	if(itVec == it->second.end())
		return con;
	return &(*itVec);
}

int
TeDBConnectionsPool::createsValidConId()
{
	int indx = dbIndxs_.size();
	bool isNotValid = true;
	//verify if this number already exists
	while(isNotValid)
	{
		std::map<int, TeDBConnectionsVector, std::less<int> >::iterator it = dbs_.find(indx);
		if(it==dbs_.end())
			isNotValid = false;
		else
			++indx;
	}
	return indx;
}















