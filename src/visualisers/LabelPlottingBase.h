/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LabelPlottingBase.h
    \brief Definition of the Template class LabelPlottingBase.

    Magics Team - ECMWF 2006

    Started: Thu 9-Feb-2006

    Changes:

*/

#ifndef LabelPlottingBase_H
#define LabelPlottingBase_H

#include "BasicSceneObject.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class XmlNode;
class GridPlottingBase;

class LabelPlottingBase {
public:
    LabelPlottingBase() {}
    virtual ~LabelPlottingBase() {}

    virtual void set(const XmlNode&) {
        MagLog::dev() << "LabelPlottingBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "LabelPlottingBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual void toxml(ostream&, int = 0) const {
        MagLog::dev() << "LabelPlottingBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }
    virtual LabelPlottingBase* clone() const {
        MagLog::dev() << "LabelPlottingBase::set(const map<string, string&)---> to be checked!...\n";
        return new LabelPlottingBase();
    }
    virtual void operator()(const BasicSceneObject&, GraphicsList&) {
        MagLog::dev() << "LabelPlottingBase::preparePlot(Task&)---> to be checked!...\n";
    }

    virtual void prepare(GridPlottingBase&) {
        MagLog::dev() << "LabelPlottingBase::prepare(GridPlotting&)---> to be checked!...\n";
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "LabelPlottingBase\n"; }

private:
    //! Copy constructor - No copy allowed
    LabelPlottingBase(const LabelPlottingBase&);
    //! Overloaded << operator to copy - No copy allowed
    LabelPlottingBase& operator=(const LabelPlottingBase&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LabelPlottingBase& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, LabelPlottingBase> {
public:
    LabelPlottingBase* operator()(const string& val) { return SimpleObjectMaker<LabelPlottingBase>::create(val); }

    LabelPlottingBase* magics(const string& param) {
        LabelPlottingBase* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
