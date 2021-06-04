/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BinningObject.h
    \brief Definition of the Template class BinningObject.

    Magics Team - ECMWF 2011

    Started: Thu 7-Apr-2011

    Changes:

*/

#ifndef BinningObject_H
#define BinningObject_H

#include "BasePointsHandler.h"
#include "BinningObjectAttributes.h"
#include "Factory.h"
#include "IntervalMap.h"
#include "MagTranslator.h"
#include "Matrix.h"
#include "magics.h"

namespace magics {


class BinningObject : public BinningObjectAttributes {
public:
    BinningObject();
    ~BinningObject() override;

    virtual BinningObject* clone() const { return new BinningObject(); }

    Matrix* operator()(PointsList& points);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    void print(ostream&) const override;
    typedef void (BinningObject::*binner)(vector<double>&, double, double);
    map<string, binner> binners_x_;
    map<string, binner> binners_y_;

    void build(vector<double>& vals, IntervalMap<int>& binns);
    void countx(vector<double>&, double, double);
    void listx(vector<double>&, double, double);
    void intervalx(vector<double>&, double, double);

    void county(vector<double>&, double, double);
    void listy(vector<double>&, double, double);
    void intervaly(vector<double>&, double, double);

private:
    //! Copy constructor - No copy allowed
    BinningObject(const BinningObject&);
    //! Overloaded << operator to copy - No copy allowed
    BinningObject& operator=(const BinningObject&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BinningObject& p) {
        p.print(s);
        return s;
    }
};


class NoBinningObject : public BinningObject {
public:
    NoBinningObject() {}
    ~NoBinningObject() override {}
    BinningObject* clone() const override { return new NoBinningObject(); }
};


template <>
class MagTranslator<string, BinningObject> {
public:
    BinningObject* operator()(const string& val) { return SimpleObjectMaker<BinningObject>::create(val); }

    BinningObject* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};
}  // namespace magics
#endif
