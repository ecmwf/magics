/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PlotManager.h
    \brief Definition of the Template class PlotManager.

    Magics Team - ECMWF 2004

    Started: Tue 17-Aug-2004

    Changes:

*/

#ifndef PlotManager_H
#define PlotManager_H

#include "Factory.h"
#include "MagTranslator.h"
#include "MagicsManager.h"
#include "Node.h"
#include "magics.h"

namespace magics {

class XmlNode;

class PlotManager : public stack<BaseSceneObject*> {
public:
    PlotManager();
    virtual ~PlotManager();

    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual PlotManager* clone() { return new PlotManager(); }
    void toxml(ostream&, int) const {}

    virtual void superpage(MagicsManager&);
    virtual void page(MagicsManager&);
    virtual void subpage(MagicsManager&);
    virtual void check(MagicsManager&);
    virtual void addpage(MagicsManager&);
    void addNode(MagicsManager&, BaseSceneObject* object);
    void add(BaseSceneObject* object);

    virtual void addRoot(MagicsManager&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    bool page_;

private:
    //! Copy constructor - No copy allowed
    PlotManager(const PlotManager&);
    //! Overloaded << operator to copy - No copy allowed
    PlotManager& operator=(const PlotManager&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const PlotManager& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, PlotManager> {
public:
    PlotManager* operator()(const string& val) { return SimpleObjectMaker<PlotManager>::create(val); }

    PlotManager* magics(const string& param) {
        PlotManager* object;
        ParameterManager::update(param, object);
        return object;
    }
};


}  // namespace magics
#endif
