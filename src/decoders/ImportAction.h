/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportAction.h
    \brief Definition of the Template class ImportAction.

    Magics Team - ECMWF 2005

    Started: Wed 6-Apr-2005

    Changes:

*/

#ifndef ImportAction_H
#define ImportAction_H

#include "magics.h"

#include "Data.h"
#include "ImportActionAttributes.h"
#include "TextVisitor.h"
namespace magics {


class ImportAction : public ImportActionAttributes, public Data {
public:
    ImportAction() {}
    virtual ~ImportAction() override {}
    void set(const map<string, string>& map) override { ImportActionAttributes::set(map); }


    string path() override { return path_; }

    void visit(MetaDataCollector& collector) override {
        MetviewIcon::visit(collector);

        MetaDataCollector::iterator format = collector.find("MV_Format");
        if (format != collector.end()) {
            format->second = service_;
        }

        MetaDataCollector::iterator name = collector.find("shortName");
        if (name != collector.end()) {
            name->second = short_name_;
        }
    }

    void visit(TextVisitor& text) override {
        MetaDataCollector collector;
        collector["service_name"]  = "";
        collector["service_title"] = "";
        collector["title"]         = "";
        collector["date"]          = "";
        collector["time"]          = "";

        MetviewIcon::visit(collector);

        if (collector["service_name"] == "WMS") {
            string s = "WMS: " + collector["title"] + " " + collector["date"] + " " + collector["time"];

            text.addAutomaticTitle(s);
        }
        else if (!url_.empty() && !service_.empty()) {
            text.addAutomaticTitle(service_ + " :" + url_);
        }
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&,
                                  bool) override {}
    //! Method to access the data as a list of points
    // needMissing : if true the list will contain all the points (If they are outside the area: They will be flagged
    // missing)
    virtual PointsHandler& points(const Transformation&, bool) override { NOTIMP; }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override {
        out << "ImportAction[";
        ImportActionAttributes::print(out);
        out << "]";
    }

private:
    //! Copy constructor - No copy allowed
    ImportAction(const ImportAction&);
    //! Overloaded << operator to copy - No copy allowed
    ImportAction& operator=(const ImportAction&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ImportAction& p) {
        p.print(s);
        return s;
    }
};


class ImportLoop : public DataLoop, public vector<Data*> {
public:
    ImportLoop() {}
    virtual ~ImportLoop() override {}
    virtual void set(const map<string, string>&) override {}
    virtual void set(const XmlNode&) override {}
    virtual void set(LayerNode&) override {}

    virtual void setToFirst() override { current_ = this->begin(); }
    virtual Data* current() override { return *current_; }
    virtual bool hasMore() override { return current_ != this->end(); }
    virtual void next() override { ++current_; }
    void add(Data* data) override { push_back(data); }

protected:
    vector<Data*>::iterator current_;
};
}  // namespace magics

#endif
