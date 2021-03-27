/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LevelSelection.h
    \brief Definition of the Template class LevelSelection.

    Magics Team - ECMWF 2004

    Started: Tue 9-Mar-2004

    Changes:

*/

#ifndef LevelSelection_H
#define LevelSelection_H

#include "Data.h"
#include "Factory.h"
#include "LevelSelectionAttributes.h"
#include "MagTranslator.h"
#include "magics.h"

namespace magics {

class UserPoint;
class PointsHandler;

class LevelSelectionInterface {
public:
    virtual int getCount() const        = 0;
    virtual int getTolerance() const    = 0;
    virtual double getReference() const = 0;
    virtual double getInterval() const  = 0;
    virtual doublearray getList() const = 0;
    virtual double getMin() const       = 0;
    virtual double getMax() const       = 0;
};

class LevelSelection : public LevelSelectionAttributes, public doublearray {
public:
    LevelSelection();
    virtual ~LevelSelection();

    virtual LevelSelection* clone() const { return 0; }
    virtual void set(const XmlNode& node) { LevelSelectionAttributes::set(node); }
    virtual void set(const map<string, string>& map) { LevelSelectionAttributes::set(map); }
    virtual void set(const LevelSelectionInterface&) {}

    virtual void calculate(double, double, bool){};
    virtual double reference(int) const;
    virtual void thinLevels(int frequency, vector<double>&) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    LevelSelection(const LevelSelection&);
    //! Overloaded << operator to copy - No copy allowed
    LevelSelection& operator=(const LevelSelection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LevelSelection& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, LevelSelection> {
public:
    LevelSelection* operator()(const string& val) { return SimpleObjectMaker<LevelSelection>::create(val); }
    LevelSelection* magics(const string& param) {
        LevelSelection* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics
#endif
