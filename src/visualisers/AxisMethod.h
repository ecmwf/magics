/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisMethod.h
    \brief Definition of the Template class AxisMethod.

    Magics Team - ECMWF 2005

    Started: Fri 7-Oct-2005

    Changes:

*/

#ifndef AxisMethod_H
#define AxisMethod_H

#include <cmath>
#include <list>

#include "AxisItem.h"
#include "AxisMethodAttributes.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "Transformation.h"
#include "magics.h"


namespace magics {

class Box;
class Axis;

class AxisMethod : public AxisMethodAttributes {
public:
    AxisMethod();
    virtual ~AxisMethod() override;

    AxisMethod* clone() {
        NOTIMP;
        return 0;
    }
    virtual void set(const map<string, string>& map) override { AxisMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) override { AxisMethodAttributes::set(node); }
    virtual void updateX(const Transformation&);
    virtual void updateY(const Transformation&);
    virtual void prepare(const Axis&, AxisItems&);


    double min() const { return min_; }
    double max() const { return max_; }

    virtual void addItem(AxisItems& items, double val, const string& format) const {
        items.push_back(new AxisItem(val, format));
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    virtual double value(double val) const { return val; }
    void prepare(list<double>&, double, double, double, double);
    double min_;
    double max_;

private:
    //! Copy constructor - No copy allowed
    AxisMethod(const AxisMethod&);
    //! Overloaded << operator to copy - No copy allowed
    AxisMethod& operator=(const AxisMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisMethod& p) {
        p.print(s);
        return s;
    }
};

class LogarithmicAxisMethod : public AxisMethod {
public:
    LogarithmicAxisMethod() {}
    virtual ~LogarithmicAxisMethod() override {}
    void prepare(const Axis&, AxisItems&) override;

    double value(double val) const override { return ::pow(10., val); }
};
class HyperAxisMethod : public AxisMethod {
public:
    HyperAxisMethod() {}
    virtual ~HyperAxisMethod() override {}
    void prepare(const Axis&, AxisItems&) override;


    void updateX(const Transformation&) override;
    void updateY(const Transformation&) override;

protected:
    vector<double> hyperMin_;
    vector<double> hyperMax_;
};
class PositionListAxisMethod : public AxisMethod {
public:
    PositionListAxisMethod() {}
    virtual ~PositionListAxisMethod() override {}
    void prepare(const Axis&, AxisItems&) override;
};


template <>
class MagTranslator<string, AxisMethod> {
public:
    AxisMethod* operator()(const string& val) { return SimpleObjectMaker<AxisMethod>::create(val); }

    AxisMethod* magics(const string& param) {
        AxisMethod* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
