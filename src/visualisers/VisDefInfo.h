/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file VisDefInfo.h
    \brief Definition of class VisDefInfo.
    
    Magics Team - ECMWF 2004
    
    Started: August 2010
    
    Changes:
    
*/

#ifndef VisDefInfo_H
#define VisDefInfo_H

#include "magics.h"

namespace magics {

class MetaDataCollector;

class VisDefInfoItem
{
public:
	VisDefInfoItem(string name) : name_(name) {};

	string name() {return name_;}
	const map<string,vector<string> > &keys() const {return keys_;}
	const map<string,string>  &attributes() const {return attributes_;}	
	void addKey(string name,string value) {keys_[name].push_back(value);}
	void addAttribute(string name, string value) {attributes_[name]=value;}

public:
	string name_;
	map<string,vector<string> > keys_;
	map<string,string>  attributes_;
}; 

class VisDefInfoBase
{
public:
	enum DataType {GribType};

	virtual ~VisDefInfoBase();
 
	VisDefInfoItem* addItem(string);
	const vector<string>& keys() {return keys_;}
	string type() {return type_;}

	//virtual vector<string> visDefFile(MvKeyProfile*,int) {return QStringList();}
	//virtual MvRequest visDefRequest(MvKeyProfile*,int) {return MvRequest();}

	virtual void getAttributes(MetaDataCollector&, map<string, string>&) {};

	bool isLoaded() {return loaded_;}
	void clear();
	void deleteItem(int);
	virtual void loadItems()=0 ;
	virtual void saveItems()=0 ;

protected:
	VisDefInfoBase(string,DataType);	
	void collectKeys();

	string fConf_;
	DataType dataType_;
	string type_;
	map<DataType,string> dataTypeName_;
	vector<VisDefInfoItem*> items_;
	vector<string> keys_;
	bool loaded_;

	map<string,string> baseAttributes_;	
};


/*class VisDefInfo : public VisDefInfoBase
{
public:
	MvQVisDefInfo(string,DataType);
	~MvQVisDefInfo() {};

	QStringList visDefFile(MvKeyProfile*,int);
	void loadItems();
	void saveItems();	
};*/

class ObstatVisDefInfo : public VisDefInfoBase
{
public:
	ObstatVisDefInfo(string,DataType);
	~ObstatVisDefInfo() {};

	void getAttributes(MetaDataCollector&, map<string, string>&);
	void loadItems();
	void saveItems() {};

protected:
	string removeZerosFromNumber(string);	
};

class VisDefInfoFactory
{
public:	
	VisDefInfoFactory() {};
	static VisDefInfoBase*  makeItem(string);
};
} // namespace magics

#endif
