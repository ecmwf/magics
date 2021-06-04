/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PageID.h
    \brief Definition of the Template class PageID.

    Magics Team - ECMWF 2004

    Started: Mon 29-Mar-2004

    Changes:

*/

#ifndef PageID_H
#define PageID_H

#include "BasicSceneObject.h"
#include "PageIDAttributes.h"
#include "magics.h"


namespace magics {


class NoPageID : public BasicSceneObject {
public:
    NoPageID() {}
    virtual ~NoPageID() override {}

    virtual void set(const XmlNode&) override {
        MagLog::dev() << "PageIDBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }
    virtual NoPageID* clone() const {
        MagLog::dev() << "NoPageID::set(const map<string, string&)---> to be checked!...\n";
        return new NoPageID();
    }
    virtual void toxml(ostream&, int = 0) const {
        MagLog::dev() << "NoPageID::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }
    virtual bool needLegend() override { return false; }
    virtual void visit(BasicGraphicsObjectContainer&) override {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "NoPageID[]"; }

private:
    //! Copy constructor - No copy allowed
    NoPageID(const NoPageID&);
    //! Overloaded << operator to copy - No copy allowed
    NoPageID& operator=(const NoPageID&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoPageID& p) {
        p.print(s);
        return s;
    }
};

class PageID : public NoPageID, public PageIDAttributes {
public:
    PageID();
    virtual ~PageID() override;
    virtual void set(const map<string, string>& map) override { PageIDAttributes::set(map); }
    virtual void set(const XmlNode& node) override { PageIDAttributes::set(node); }
    virtual bool accept(const string& node) override { return PageIDAttributes::accept(node); }
    virtual void visit(BasicGraphicsObjectContainer& list) override;
    virtual NoPageID* clone() const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    PageID(const PageID&);
    //! Overloaded << operator to copy - No copy allowed
    PageID& operator=(const PageID&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const PageID& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoPageID> {
public:
    NoPageID* operator()(const string& val) { return SimpleObjectMaker<NoPageID>::create(val); }

    NoPageID* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
