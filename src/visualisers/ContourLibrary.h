/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ContourLibrary.h
    \brief Definition of the Template class ContourLibrary.

    Magics Team - ECMWF 2010

    Started: Fri 16-Jul-2010

    Changes:

*/

#ifndef ContourLibrary_H
#define ContourLibrary_H

#include "magics.h"

#include "ContourLibraryAttributes.h"
#include "MagConfig.h"
#include "MagTranslator.h"

namespace magics {

class MetaDataCollector;
class VisDefInfoBase;
class StyleEntry;

class ContourLibrary : public ContourLibraryAttributes {
public:
    ContourLibrary();
    virtual ~ContourLibrary() override;

    // set the meta data to be collected
    virtual void askId(MetaDataCollector&);

    virtual bool checkId(MetaDataCollector&, MetaDataCollector&);


    // se the map to set the contour!
    virtual void getStyle(MetaDataCollector&, MagDef&, StyleEntry&);
    virtual void getStyle(const string&, MagDef&) {}
    virtual void getScaling(MetaDataCollector&, double& scaling, double& offset) {
        scaling = 1;
        offset  = 0;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    ContourLibrary(const ContourLibrary&);
    //! Overloaded << operator to copy - No copy allowed
    ContourLibrary& operator=(const ContourLibrary&);

    bool setInfoObject(string);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ContourLibrary& p) {
        p.print(s);
        return s;
    }

    static VisDefInfoBase* info_;
};


class EcChartData : public MagConfig {
public:
    EcChartData(const string& name) {
        path_ = buildSharePath(name + ".json");
        MagConfigHandler(path_, *this);
    }

    ~EcChartData() {}

    void callback(const string&, const Value&);
    map<string, string> getMap(const int);
    void init();
    map<int, map<string, string> > data_;
    string path_;

    friend ostream& operator<<(ostream& s, const EcChartData& p) {
        for (map<int, map<string, string> >::const_iterator d = p.data_.begin(); d != p.data_.end(); ++d) {
            s << d->first << "[" << endl;
            for (map<string, string>::const_iterator i = d->second.begin(); i != d->second.end(); ++i) {
                s << "    " << i->first << " = " << i->second << endl;
            }
            s << "]" << endl;
        }
        s << "read from " << p.path_;
        return s;
    }
};

class EcChartSetData : public MagConfig {
public:
    EcChartSetData(const string& name) {
        path_ = buildSharePath(name + ".json");
        MagConfigHandler(path_, *this);
    }
    ~EcChartSetData() {}

    void callback(const string&, const Value&);
    bool hasKey(const string&);
    vector<int> getSet(const string&);

    void init();
    map<string, vector<int> > data_;
    string path_;

    friend ostream& operator<<(ostream& s, const EcChartSetData& p) {
        for (map<string, vector<int> >::const_iterator d = p.data_.begin(); d != p.data_.end(); ++d) {
            s << d->first << "[" << endl;
            for (vector<int>::const_iterator i = d->second.begin(); i != d->second.end(); ++i) {
                s << "    " << *i << endl;
            }
            s << "]" << endl;
        }
        s << "read from " << p.path_;
        return s;
    }
};

class EcChartLibrary : public ContourLibrary {
public:
    EcChartLibrary();
    virtual ~EcChartLibrary() override;

    // set the meta data to be collected
    void askId(MetaDataCollector&) override;

    bool checkId(MetaDataCollector&, MetaDataCollector&) override { return true; }
    void setCriteria(MetaDataCollector&, const string&);

    // set the map to set the contour!
    void getStyle(MetaDataCollector&, MagDef&, StyleEntry&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    EcChartData contours_;
    EcChartSetData default_set_;
    vector<string> keys_;
    map<string, EcChartSetData> index_;
};
class WebLibrary : public ContourLibrary {
public:
    WebLibrary();
    virtual ~WebLibrary() override;

    // set the meta data to be collected
    void askId(MetaDataCollector&) override;

    bool checkId(MetaDataCollector&, MetaDataCollector&) override { return false; }
    void setCriteria(MetaDataCollector&, const string&);

    // set the map to set the contour!
    void getStyle(MetaDataCollector&, MagDef&, StyleEntry&) override;
    void getStyle(const string&, MagDef&) override;

    void getScaling(MetaDataCollector&, double&, double&) override;


    static StyleLibrary* styles_;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    string libraryPath_;
};


class NoContourLibrary : public ContourLibrary {
public:
    NoContourLibrary() {}
    virtual ~NoContourLibrary() override {}

    // sete the meata dat to be collected
    void askId(MetaDataCollector&) override {}

    bool checkId(MetaDataCollector&, MetaDataCollector&) override { return false; }

    // se the map to set the contour!
    void getStyle(MetaDataCollector&, map<string, string>&, StyleEntry&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "NoContourLibrary[]"; }
};

template <>
class MagTranslator<string, ContourLibrary> {
public:
    ContourLibrary* operator()(const string& val) { return SimpleObjectMaker<ContourLibrary>::create(val); }
};
}  // namespace magics
#endif
