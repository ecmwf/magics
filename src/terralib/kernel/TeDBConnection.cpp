
#include <sstream>
#include "TeDBConnection.h"
#include "TeDatabase.h"
#include "TeDatabaseFactory.h"
#include "TeDBConnectionsPool.h"


TeDBConnection::TeDBConnection(const bool& isShared) :
	id_(-1), driverInUse_(false), isShared_(isShared), db_(0), dbmsName_(""),
	host_(""), databaseName_(""), user_(""), password_(""), portNumber_(-1)
{ }

TeDBConnection::TeDBConnection(const std::string& hostName, const std::string& databaseName, 
		const std::string& userName, const std::string& dbmsName, const int& port, 
		const std::string& userPasswd, const int& conId, TeDatabase* db, const bool& isShared) :
	id_(conId), driverInUse_(false), isShared_(isShared), db_(db), dbmsName_(dbmsName),
	host_(hostName), databaseName_(databaseName), user_(userName), password_(userPasswd),
	portNumber_(port) 
{
}

TeDBConnection::TeDBConnection(const int& conId, TeDatabase* db, const bool& isShared) :
	id_(conId), driverInUse_(false), isShared_(isShared), db_(db), dbmsName_(""),
	host_(""), databaseName_(""), user_(""), password_(""), portNumber_(-1) 
{
	if(db)
	{
		string dbKey = db->getDatabaseDescription();
		fillDBConnectionInfo(dbKey);
	}
}

TeDBConnection::TeDBConnection(const std::string& dbKey, const bool& isShared) :
	id_(-1), driverInUse_(false), isShared_(isShared), db_(0), dbmsName_(""),
	host_(""), databaseName_(""), user_(""), password_(""), portNumber_(-1)
{
	fillDBConnectionInfo(dbKey);
} 

TeDBConnection::TeDBConnection(const TeDBConnection& other)
{
	id_ = other.id_;
    driverInUse_ = other.driverInUse_;
	isShared_ = other.isShared_;
	dbmsName_ = other.dbmsName_;
	host_ = other.host_;
	databaseName_ = other.databaseName_;
	user_ = other.user_;
	password_ = other.password_;
	portNumber_ = other.portNumber_;
	db_=0;
	if(other.db_)
	{
		db_ = TeDatabaseFactory::make(other.asDatabaseFactoryParams());
		*db_ = *other.db_;
	}
}

TeDBConnection&
TeDBConnection::operator=(const TeDBConnection& other)
{
	if(this != &other)
	{
		id_ = other.id_;
        driverInUse_ = other.driverInUse_;
		isShared_ = other.isShared_;
		dbmsName_ = other.dbmsName_;
		host_ = other.host_;
		databaseName_ = other.databaseName_;
		user_ = other.user_;
		password_ = other.password_;
		portNumber_ = other.portNumber_;
		if(db_)
		{
			db_->close();
			delete db_;
			db_=0;
		}
		if(other.db_)
		{
			db_ = TeDatabaseFactory::make(other.asDatabaseFactoryParams());
			*db_ = *other.db_;
		}
	}
	return *this;
}


TeDBConnection::~TeDBConnection()
{	
	if (db_)
	{
		db_->close();
		delete db_;
	}
	db_ = 0;
}

bool 
TeDBConnection::operator==(const TeDBConnection& other)
{
	return ((id_==other.id_) && 
			(dbmsName_ == other.dbmsName_) && 
			(host_==other.host_) &&	(portNumber_==other.portNumber_) &&
			(databaseName_==other.databaseName_) &&
			(user_ == other.user_) && (password_== other.password_));
}

void 
TeDBConnection::clear()
{
	id_ = -1;
	driverInUse_ = false; 
	isShared_ = true; 
	if(db_)
	{
		db_->close();
		delete db_;
	}
	db_ = 0;
	dbmsName_ = ""; 
	host_ = "";
	portNumber_ = -1;
	databaseName_= ""; 
	user_= ""; 
	password_= ""; 
}

void 
TeDBConnection::setId(const int& id)
{
	id_ = id;
}
	
int	
TeDBConnection::getId()
{
	return id_;
}

bool TeDBConnection::isInUse()
{
	return driverInUse_;
}

void TeDBConnection::setInUse(const bool& inUse)
{
	driverInUse_ = inUse;
}

bool TeDBConnection::isShared()
{
	return isShared_;
}

void TeDBConnection::setShared(const bool& isShared)
{
	isShared_ = isShared;
}

TeDatabase* TeDBConnection::getDatabase()
{
	return db_;
}

void TeDBConnection::setDatabase(TeDatabase* db)
{
	db_ = db;
}

std::string TeDBConnection::getDbmsName() 
{ 
	return dbmsName_; 
}

void TeDBConnection::setDbmsName(const std::string& dbmsName)
{
	dbmsName_ = dbmsName;
}

std::string TeDBConnection::getHost() 
{ 
	return host_; 
}

void TeDBConnection::setHost(const std::string& host) 
{
	host_ = host;
}

std::string TeDBConnection::getDatabaseName() 
{ 
	return databaseName_; 
}

void TeDBConnection::setDatabaseName(const std::string& databaseName) 
{
	databaseName_ = databaseName;
}

std::string TeDBConnection::getUser()
{ 
	return user_; 
}

void TeDBConnection::setUser(const std::string&  user) 
{
	user_ = user;
}

std::string TeDBConnection::getPassword() 
{ 
	return password_; 
}

void TeDBConnection::setPassword(const std::string& password)
{
	password_ = password;
}

int TeDBConnection::getPortNumber()
{
	return portNumber_;
}

void TeDBConnection::setPortNumber(const int& port)
{
	portNumber_ = port;
}

std::string TeDBConnection::getDbKey()
{
	std::string  dbKey = ""; 
	
	dbKey += dbmsName_;
	dbKey += ";"; 
	dbKey += host_;
	dbKey += ";"; 
	dbKey += Te2String(portNumber_);
	dbKey += ";"; 
	dbKey += databaseName_;
	dbKey += ";"; 
	dbKey += user_;
	dbKey += ";"; 
	dbKey += password_;
	return dbKey;
}

void TeDBConnection::fillDBConnectionInfo(const std::string& dbKey)
{
    int NUM_PARAMS = 6;

	if (dbKey.empty())
		return;

    // Checking for valid key.
    size_t start = 0;
    vector<int> indexs;
    while (true)
    {
        start = dbKey.find_first_of(";", start);
        if (start == string::npos)
            break;

        indexs.push_back(start);
        start++;
    }

    if ((int)indexs.size() != (NUM_PARAMS - 1))
        return;

    string dbms_name = "";
    if (indexs[0] > 0)
        dbms_name = dbKey.substr(0, indexs[0]);

    string host = "";
    if ((indexs[1] - indexs[0]) > 1)
        host = dbKey.substr(indexs[0] + 1, (indexs[1] - indexs[0]) - 1);

    int port_number = -1;
    if ((indexs[2] - indexs[1]) > 1)
    {
        string v;
        v = dbKey.substr(indexs[1] + 1, (indexs[2] - indexs[1]) - 1);
        port_number = atoi((char*)v.c_str());
    }

    string database_name = "";
    if ((indexs[3] - indexs[2]) > 1)
        database_name = dbKey.substr(indexs[2] + 1, (indexs[3] - indexs[2]) - 1);

    string user = "";
    if ((indexs[4] - indexs[3]) > 1)
        user = dbKey.substr(indexs[3] + 1, (indexs[4] - indexs[3]) - 1);

    string password = "";
    if ((dbKey.size() - indexs[4]) > 1)
        password = dbKey.substr(indexs[4] + 1, (dbKey.size() - indexs[4]) - 1);

    this->setDbmsName(dbms_name);
    this->setHost(host);
    this->setPortNumber(port_number);
    this->setDatabaseName(database_name);
    this->setUser(user);
    this->setPassword(password);

	return;
}

void TeDBConnection::lock()
{
	driverInUse_ = true;
}

bool TeDBConnection::connect()
{
	if (!db_)
    {
		if ( !createDatabaseConnection() )
        {
            return false;
        }
    }
	
	if(db_->isConnected())
		db_->close();
	
	return db_->connect(host_, user_, password_, databaseName_, portNumber_);  
}

bool TeDBConnection::createDatabaseModel() 
{
	if (!db_)
		return false;
	if (!db_->newDatabase(databaseName_, user_, password_, host_))
		return false;
	return true;
}

TeDatabaseFactoryParams 
TeDBConnection::asDatabaseFactoryParams() const
{
	TeDatabaseFactoryParams params;
	params.host_ = host_;
	params.port_ = portNumber_;
	params.password_ = password_;
	params.user_ = user_;
	params.database_ = databaseName_;
	params.dbms_name_ = dbmsName_;

	return params;
}

bool TeDBConnection::load(TeDatabase* sourceDB, const int& dbId, TeDBConnection& dbCon)
{
	dbCon.clear();

	if(!sourceDB)
		return false;

	std::string strSQL = "SELECT * FROM te_database_connection WHERE connection_id = " + Te2String(dbId);
    
	TeDatabasePortal* dbPortal = sourceDB->getPortal();

    if(!dbPortal)
		return false;

    if(!dbPortal->query(strSQL) || !dbPortal->fetchRow())
    {
        dbPortal->freeResult();
        delete dbPortal;
		return false;
	}

	dbCon.setId(dbPortal->getInt("connection_id"));
	dbCon.setDbmsName(dbPortal->getData("dbms_name"));
	dbCon.setHost(dbPortal->getData("host_name"));
	dbCon.setDatabaseName(dbPortal->getData("database_name"));
	dbCon.setUser(dbPortal->getData("user_name"));
	dbCon.setPassword(dbPortal->getData("user_password"));
	dbCon.setPortNumber(dbPortal->getInt("port_number"));	

	dbPortal->freeResult();
    delete dbPortal;
	return true;
}

bool TeDBConnection::load(TeDatabase* sourceDB)
{
	if(!sourceDB)
		return false;

	std::string strSQL="SELECT * FROM te_database_connection";
    
	TeDatabasePortal* dbPortal = sourceDB->getPortal();

    if(!dbPortal)
		return false;

    if(!dbPortal->query(strSQL))
    {
        dbPortal->freeResult();
        delete dbPortal;
		return false;
	}

	while(dbPortal->fetchRow())
	{
		TeDBConnection dbConn;
		dbConn.setId(dbPortal->getInt("connection_id"));
		dbConn.setDbmsName(dbPortal->getData("dbms_name"));
		dbConn.setHost(dbPortal->getData("host_name"));
		dbConn.setDatabaseName(dbPortal->getData("database_name"));
		dbConn.setUser(dbPortal->getData("user_name"));
		dbConn.setPassword(dbPortal->getData("user_password"));
		dbConn.setPortNumber(dbPortal->getInt("port_number"));
		dbConn.setDatabase(0);

		TeDBConnectionsPool::instance().insertDBConnection(dbConn);
	}

	dbPortal->freeResult();
    delete dbPortal;
	return true;
}

bool TeDBConnection::save(TeDatabase* sourceDB, TeDBConnection& dbConn)
{
	std::string searchSQL  = "SELECT * FROM te_database_connection WHERE connection_id = ";
	            searchSQL += Te2String(dbConn.getId());

	TeDatabasePortal* portal = sourceDB->getPortal();

	if(!portal)
		return false;

	if(!portal->query(searchSQL))
	{
		portal->freeResult();
		delete portal;
		return false;
	}

	bool isInsert = !portal->fetchRow();
	portal->freeResult();
	delete portal;

	std::string strSQL = "";

	if(isInsert)
	{
		// insert
		strSQL  = "INSERT INTO te_database_connection(dbms_name, host_name, database_name, user_name, user_password, port_number) ";
	    strSQL += "VALUES ('" + dbConn.getDbmsName();
        strSQL += "', '" + dbConn.getHost();
        strSQL += "', '" + dbConn.getDatabaseName();
        strSQL += "', '" + dbConn.getUser(); 
        strSQL += "', '" + dbConn.getPassword();
        strSQL += "', " + Te2String(dbConn.getPortNumber());
        strSQL += ")";
	}
	else
	{
		// update		
		strSQL  = "UPDATE te_database_connection SET dbms_name='" + dbConn.getDbmsName();
		strSQL += "', host_name='" + dbConn.getHost();
		strSQL += "', database_name='" + dbConn.getDatabaseName();
		strSQL += "', user_name='" + dbConn.getUser();
		strSQL += "', user_password='" + dbConn.getPassword();
		strSQL += "', port_number=" + Te2String(dbConn.getPortNumber());
		strSQL += " WHERE connection_id = " + Te2String(dbConn.getId());
	}

	if(!sourceDB->execute(strSQL))
        return false;

	if(isInsert)
		return updateId(sourceDB, dbConn);

    return true;
}

bool TeDBConnection::remove(TeDatabase* sourceDB, TeDBConnection& dbConn)
{
	std::string deleteSQL  = " DELETE FROM te_database_connection WHERE connection_id = ";
		        deleteSQL += Te2String(dbConn.getId());
	
	if(!sourceDB->execute(deleteSQL))
        return false;

	TeDBConnectionsPool::instance().removeDBConnection(dbConn);
	
	return true;
}

int TeDBConnection::createDBConnectionTable(TeDatabase* sourceDB)
{
	if(!sourceDB)
		return 0;

	if(sourceDB->tableExist("te_database_connection"))
		return -1;

	TeAttributeList attList;

	TeAttribute att1;
	att1.rep_.name_ = "connection_id";
	att1.rep_.isAutoNumber_ = true;
	att1.rep_.isPrimaryKey_ = true;
	att1.rep_.null_ = false;
	att1.rep_.type_ = TeINT;
	att1.rep_.numChar_ = 0;
	attList.push_back(att1);

	TeAttribute att2;
	att2.rep_.name_ = "dbms_name";
	att2.rep_.isAutoNumber_ = false;
	att2.rep_.isPrimaryKey_ = false;
	att2.rep_.null_ = true;
	att2.rep_.type_ = TeSTRING;
	att2.rep_.numChar_ = 255;
	attList.push_back(att2);

	TeAttribute att3;
	att3.rep_.name_ = "host_name";
	att3.rep_.isAutoNumber_ = false;
	att3.rep_.isPrimaryKey_ = false;
	att3.rep_.null_ = true;
	att3.rep_.type_ = TeSTRING;
	att3.rep_.numChar_ = 255;
	attList.push_back(att3);

	TeAttribute att4;
	att4.rep_.name_ = "database_name";
	att4.rep_.isAutoNumber_ = false;
	att4.rep_.isPrimaryKey_ = false;
	att4.rep_.null_ = true;
	att4.rep_.type_ = TeSTRING;
	att4.rep_.numChar_ = 255;
	attList.push_back(att4);

	TeAttribute att5;
	att5.rep_.name_ = "user_name";
	att5.rep_.isAutoNumber_ = false;
	att5.rep_.isPrimaryKey_ = false;
	att5.rep_.null_ = true;
	att5.rep_.type_ = TeSTRING;
	att5.rep_.numChar_ = 255;
	attList.push_back(att5);

	TeAttribute att6;
	att6.rep_.name_ = "user_password";
	att6.rep_.isAutoNumber_ = false;
	att6.rep_.isPrimaryKey_ = false;
	att6.rep_.null_ = true;
	att6.rep_.type_ = TeSTRING;
	att6.rep_.numChar_ = 255;
	attList.push_back(att6);

	TeAttribute att7;
	att7.rep_.name_ = "port_number";
	att7.rep_.isAutoNumber_ = false;
	att7.rep_.isPrimaryKey_ = false;
	att7.rep_.null_ = true;
	att7.rep_.type_ = TeINT;
	att7.rep_.numChar_ = 0;
	attList.push_back(att7);

	if(!sourceDB->createTable("te_database_connection", attList))
		return 0;

	return 1;
}

bool TeDBConnection::createDatabaseConnection() 
{
	if(db_)
	{
		db_->close();
		delete db_;
		db_=0;
	}
	db_ = TeDatabaseFactory::make(this->asDatabaseFactoryParams());
	if(!db_)
		return false;
	return true;
}

bool TeDBConnection::updateId(TeDatabase* sourceDB, TeDBConnection& dbConn)
{
	std::string searchSQL  = "SELECT * FROM te_database_connection WHERE ";
	            searchSQL += "dbms_name = '" + dbConn.getDbmsName();
		        searchSQL += "' AND host_name = '" + dbConn.getHost();
		        searchSQL += "' AND database_name = '" + dbConn.getDatabaseName();
		        searchSQL += "' AND user_name = '" + dbConn.getUser();
		        searchSQL += "' AND user_password = '" + dbConn.getPassword();
		        searchSQL += "' AND port_number = " + Te2String(dbConn.getPortNumber());

	TeDatabasePortal* portal = sourceDB->getPortal();

	if(!portal)
		return false;

	bool status = false;

	if(portal->query(searchSQL) && portal->fetchRow())
	{
        dbConn.setId(portal->getInt("connection_id"));

		status = true;
	}

	portal->freeResult();
	delete portal;
	return status;
}


