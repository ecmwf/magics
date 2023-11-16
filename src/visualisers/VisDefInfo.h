/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

class VisDefInfoItem {
public:
    VisDefInfoItem(const string& name) : name_(name){};

    string name() const { return name_; }
    const map<string, vector<string> >& keys() const { return keys_; }
    const map<string, string>& attributes() const { return attributes_; }
    void addKey(const string& name, const string& value) { keys_[name].push_back(value); }
    void addAttribute(const string& name, const string& value) { attributes_[name] = value; }

public:
    string name_;
    map<string, vector<string> > keys_;
    map<string, string> attributes_;
};

class VisDefInfoBase {
public:
    enum DataType
    {
        GribType
    };

    virtual ~VisDefInfoBase();

    VisDefInfoItem* addItem(const string&);
    const vector<string>& keys() { return keys_; }
    string type() const { return type_; }

    // virtual vector<string> visDefFile(MvKeyProfile*,int)   {return QStringList() ;}
    // virtual MvRequest visDefRequest(MvKeyProfile*,int)   {return MvRequest() ;}

    virtual void getAttributes(MetaDataCollector&, map<string, string>&) {}


    bool isLoaded() const { return loaded_; }
    void clear();
    void deleteItem(int);
    virtual void loadItems() = 0;
    virtual void saveItems() = 0;

protected:
    VisDefInfoBase(const string&, DataType);
    void collectKeys();

    string fConf_;
    DataType dataType_;
    string type_;
    map<DataType, string> dataTypeName_;
    vector<VisDefInfoItem*> items_;
    vector<string> keys_;
    bool loaded_;

    map<string, string> baseAttributes_;
};


/*class VisDefInfo : public VisDefInfoBase
{
public:
    MvQVisDefInfo(string,DataType);
    ~MvQVisDefInfo () override {};

    QStringList visDefFile(MvKeyProfile*,int);
    void loadItems();
    void saveItems();
};*/

class ObstatVisDefInfo : public VisDefInfoBase {
public:
    ObstatVisDefInfo(const string&, DataType);
    ~ObstatVisDefInfo() override{};

    void getAttributes(MetaDataCollector&, map<string, string>&) override;
    void loadItems() override;
    void saveItems() override{};

protected:
    string removeZerosFromNumber(const string&);
};

class VisDefInfoFactory {
public:
    VisDefInfoFactory(){};
    static VisDefInfoBase* makeItem(const string&);
};
}  // namespace magics

#endif
