/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef ObsTable_H
#define ObsTable_H

#include "BasicSceneObject.h"
#include "ObsItem.h"
#include "ObsPlotting.h"
#include "ObsTableAttributes.h"
#include "PaperPoint.h"
#include "magics.h"

namespace magics {


class ObsTemplate : public vector<ObsItem*> {
public:
    ObsTemplate(const map<string, string>& def) {
        map<string, string>::const_iterator val = def.find("columns");
        columns_                                = (val != def.end()) ? atoi(val->second.c_str()) : 3;
        val                                     = def.find("rows");
        rows_                                   = (val != def.end()) ? atoi(val->second.c_str()) : 3;
    }
    virtual ~ObsTemplate() {}
    void visit(std::set<string>& tokens) const {
        for (const_iterator item = begin(); item != end(); ++item)
            (*item)->visit(tokens);
    }
    void set(const ObsPlotting* obs) const {
        apart_  = obs->apart_;
        height_ = obs->size_;
        for (const_iterator item = begin(); item != end(); ++item)
            (*item)->set(obs);
    }

    void operator()(CustomisedPoint&, BasicGraphicsObjectContainer&) const;

protected:
    virtual void print(ostream& out) const {
        out << "ObsTemplate:[\n";
        for (const_iterator item = begin(); item != end(); ++item)
            out << "\t" << *(*item) << "\n";
        out << "\n";
    }
    double columns_;
    double rows_;
    mutable vector<PaperPoint> working_;
    mutable double apart_;
    mutable double height_;
    mutable string box_;

    friend ostream& operator<<(ostream& s, const ObsTemplate& p) {
        p.print(s);
        return s;
    }
};

class ObsTable : public map<string, ObsTemplate*>, public ObsTableAttributes {
public:
    ObsTable();
    virtual ~ObsTable() override;

    void add(const string&, const map<string, string>&);

    static void print() {
        if (!table_)
            table_ = new ObsTable();
    }

    static void release() {
        if (!table_)
            delete table_;
    }


    static const ObsTemplate& getTemplate(const string type) {
        if (!table_)
            table_ = new ObsTable();
        return table_->get(type);
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    ObsTemplate* current_;
    double rows_;
    double columns_;

    const ObsTemplate& get(const string&);

    static ObsTable* table_;

private:
    //! Copy constructor - No copy allowed
    ObsTable(const ObsTable&);
    //! Overloaded << operator to copy - No copy allowed
    ObsTable& operator=(const ObsTable&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ObsTable& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
