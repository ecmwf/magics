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

/*! \file MetaData.h
    \brief Definition of the Template class MetaData.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan-2006

*/

#ifndef MetaData_H
#define MetaData_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"

#include "MetaDataAttributes.h"
#include "VectorOfPointers.h"
#include "BasicSceneObject.h"

namespace magics {
	
class MetaDataEntry 
{
public: 
	MetaDataEntry(const string& data) : data_(data) {}
	virtual ~MetaDataEntry() {}
	
protected:
	string data_;
	 virtual void print(ostream& s) const { s << data_; } 

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MetaDataEntry& p)
		{ p.print(s); return s; }
};

typedef  VectorOfPointers<vector<MetaDataEntry*> > MetaDataEntryList;

class MetaDataVisitor: public MetaDataAttributes, public MetaDataEntryList, public BasicSceneObject {

public:
	MetaDataVisitor();
	virtual ~MetaDataVisitor();

    virtual void set(const XmlNode& node) {
        MetaDataAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
    	 MetaDataAttributes::set(map);
    }
    virtual MetaDataVisitor* clone() const {

        MetaDataVisitor* object = new MetaDataVisitor();
        object->copy(*this);
        return object;
    }

    void collectMetaData();
    
    void add(MetaDataEntry* entry) { MetaDataEntryList::push_back(entry); }
    void add(const string& key, const string& value) {web_.insert(make_pair(key, value)); }
    void metadata(map<string,string>&);

    virtual void close(); 
    
    static void collect();

    static void start();
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 map<string, string> web_;
	 static vector<MetaDataVisitor*> meta_;
	 static string start_;
private:
    //! Copy constructor - No copy allowed
	MetaDataVisitor(const MetaDataVisitor&);
    //! Overloaded << operator to copy - No copy allowed
	MetaDataVisitor& operator=(const MetaDataVisitor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MetaDataVisitor& p)
		{ p.print(s); return s; }
};

class MetaData: public MetaDataVisitor
{
public:
    MetaData() {}
    ~MetaData() {}
	MetaDataVisitor* clone() const {
        return new MetaData();
    }
};

class NoMetaData: public MetaDataVisitor
{
public:
    NoMetaData() {}
    ~NoMetaData() {}
	MetaDataVisitor* clone() const {
        return new NoMetaData();
    }
    virtual void close() {}
    virtual void visit(BasicGraphicsObjectContainer&) {}
};

template <>
class MagTranslator<string, MetaDataVisitor> { 
public:
	MetaDataVisitor* operator()(const string& val )
	{
		return SimpleObjectMaker<MetaDataVisitor>::create(val);
	}

	MetaDataVisitor* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};


} // namespace magics
#endif
