/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MetaData.h
    \brief Definition of the Template class MetaData.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan-2006

*/

#ifndef MetaData_H
#define MetaData_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "AutoVector.h"
#include "BasicSceneObject.h"
#include "MetaDataAttributes.h"

namespace magics {

class MetaDataEntry {
public:
    MetaDataEntry(const string& data) : data_(data) {}
    virtual ~MetaDataEntry() {}

protected:
    string data_;
    virtual void print(ostream& s) const { s << data_; }

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MetaDataEntry& p) {
        p.print(s);
        return s;
    }
};

class StyleEntry {
public:
    StyleEntry(){};
    void set(const string& def, const vector<string>& styles) {
        default_ = def;
        styles_  = styles;
    }

    virtual ~StyleEntry() {}

    string default_;
    vector<string> styles_;

    virtual void print(ostream& s) const;

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const StyleEntry& p) {
        p.print(s);
        return s;
    }
};

typedef AutoVector<MetaDataEntry> MetaDataEntryList;

class MetaDataVisitor : public MetaDataAttributes, public MetaDataEntryList, public BasicSceneObject {
public:
    MetaDataVisitor();
    virtual ~MetaDataVisitor() override;

    virtual void set(const XmlNode& node) override { MetaDataAttributes::set(node); }
    virtual void set(const map<string, string>& map) override { MetaDataAttributes::set(map); }
    virtual MetaDataVisitor* clone() const {
        MetaDataVisitor* object = new MetaDataVisitor();
        object->copy(*this);
        return object;
    }

    void collectMetaData();

    void add(MetaDataEntry* entry) { MetaDataEntryList::push_back(entry); }
    void add(StyleEntry* entry) { styles_.push_back(entry); }
    void add(const string& key, const string& value) { web_.insert(make_pair(key, value)); }

    void metadata(map<string, string>&);

    static void collect();

    MAGICS_EXPORT static void start();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    map<string, string> web_;
    AutoVector<StyleEntry> styles_;

    static vector<MetaDataVisitor*> meta_;
    static string start_;

private:
    //! Copy constructor - No copy allowed
    MetaDataVisitor(const MetaDataVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    MetaDataVisitor& operator=(const MetaDataVisitor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MetaDataVisitor& p) {
        p.print(s);
        return s;
    }
};

class MetaData : public MetaDataVisitor {
public:
    MetaData() {}
    ~MetaData() override {}
    MetaDataVisitor* clone() const override { return new MetaData(); }
};

class NoMetaData : public MetaDataVisitor {
public:
    NoMetaData() {}
    ~NoMetaData() override {}
    MetaDataVisitor* clone() const override { return new NoMetaData(); }
    virtual void visit(BasicGraphicsObjectContainer&) override {}
};

template <>
class MagTranslator<string, MetaDataVisitor> {
public:
    MetaDataVisitor* operator()(const string& val) { return SimpleObjectMaker<MetaDataVisitor>::create(val); }

    MetaDataVisitor* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};


}  // namespace magics
#endif
