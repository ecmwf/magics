/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LogoPlotting.h
    \brief Definition of the Template class LogoPlotting.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jun-2005

*/

#ifndef LogoPlotting_H
#define LogoPlotting_H

#include "BasicGraphicsObject.h"
#include "LogoPlottingAttributes.h"
#include "MagTranslator.h"
#include "UserLogoPlottingAttributes.h"
#include "magics.h"
namespace magics {

class BasicSceneObject;

class NoLogoPlotting {
public:
    NoLogoPlotting() {}
    virtual ~NoLogoPlotting() {}

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}

    virtual bool accept(const string&) { return false; }

    virtual NoLogoPlotting* clone() const { return new NoLogoPlotting(); }
    virtual void toxml(ostream&, int = 0) const {}

    virtual void operator()(BasicGraphicsObjectContainer&) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "NoLogoPlotting[]"; }

private:
    //! Copy constructor - No copy allowed
    NoLogoPlotting(const NoLogoPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    NoLogoPlotting& operator=(const NoLogoPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoLogoPlotting& p) {
        p.print(s);
        return s;
    }
};

class LogoPlotting : public NoLogoPlotting, public LogoPlottingAttributes {
public:
    LogoPlotting();
    virtual ~LogoPlotting() override;
    virtual void set(const map<string, string>&) override {}
    virtual void set(const XmlNode&) override {}
    void toxml(ostream&, int) const override {}
    virtual NoLogoPlotting* clone() const override {
        LogoPlotting* object = new LogoPlotting();
        return object;
    }
    virtual void operator()(BasicGraphicsObjectContainer&) const override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    double x_;
    double y_;

private:
    //! Copy constructor - No copy allowed
    LogoPlotting(const LogoPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    LogoPlotting& operator=(const LogoPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LogoPlotting& p) {
        p.print(s);
        return s;
    }
};


class UserLogoPlotting : public NoLogoPlotting, public UserLogoPlottingAttributes {
public:
    UserLogoPlotting();
    virtual ~UserLogoPlotting() override;
    virtual void set(const map<string, string>& map) override { UserLogoPlottingAttributes::set(map); }
    virtual void set(const XmlNode& node) override { UserLogoPlottingAttributes::set(node); }
    bool accept(const string& node) override { return UserLogoPlottingAttributes::accept(node); }

    virtual NoLogoPlotting* clone() const override {
        UserLogoPlotting* object = new UserLogoPlotting();
        object->copy(*this);
        return object;
    }
    void operator()(BasicGraphicsObjectContainer&) const override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    UserLogoPlotting(const UserLogoPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    UserLogoPlotting& operator=(const UserLogoPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const UserLogoPlotting& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, NoLogoPlotting> {
public:
    NoLogoPlotting* operator()(const string& val) { return SimpleObjectMaker<NoLogoPlotting>::create(val); }

    NoLogoPlotting* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
