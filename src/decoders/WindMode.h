/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file WindMode.h
    \brief Definition of the Template class WindMode.

    Magics Team - ECMWF 2006

    Started: Wed 9-Aug-2006

    Changes:

*/

#ifndef WindMode_H
#define WindMode_H

#include "Factory.h"
#include "Layer.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class WindMode {
public:
    WindMode();
    virtual ~WindMode();

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }

    virtual bool accept(const string&) { return false; }
    virtual WindMode* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new WindMode();
    }

    virtual void toxml(ostream&) {}

    virtual void x(Matrix&, Matrix&) {}
    virtual pair<double, double> operator()(double x, double y) { return std::make_pair(x, y); }

    virtual ValuesCollectorData* values(double lon, double lat, double x, double y, double dist) {
        return new ValuesCollectorData(lon, lat, x, dist);
    }

    virtual double norm(double x, double y) const { return x; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    WindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
    WindMode& operator=(const WindMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const WindMode& p) {
        p.print(s);
        return s;
    }
};

class UVWindMode : public WindMode {
public:
    UVWindMode() {}
    virtual ~UVWindMode() override {}
    WindMode* clone() const override { return new UVWindMode(); }
    virtual void x(Matrix&, Matrix&) override;
    virtual pair<double, double> operator()(double x, double y) override { return std::make_pair(x, y); }
    ValuesCollectorData* values(double lon, double lat, double x, double y, double dist) override {
        return new ValuesCollectorUVData(lon, lat, x, y, dist);
    }
    double norm(double x, double y) const override { return sqrt(x * x + y * y); }

private:
    //! Copy constructor - No copy allowed
    UVWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
    UVWindMode& operator=(const WindMode&);
};


class SDWindMode : public WindMode {
public:
    SDWindMode() {}
    virtual ~SDWindMode() override {}
    WindMode* clone() const override { return new SDWindMode(); }
    virtual void x(Matrix&, Matrix&) override;
    virtual pair<double, double> operator()(double x, double y) override;
    ValuesCollectorData* values(double lon, double lat, double x, double y, double dist) override {
        return new ValuesCollectorSDData(lon, lat, x, y, dist);
    }
    double norm(double x, double y) const override { return x; }

private:
    //! Copy constructor - No copy allowed
    SDWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
    SDWindMode& operator=(const SDWindMode&);
};

class VDWindMode : public WindMode {
public:
    VDWindMode() {}
    virtual ~VDWindMode() override {}
    WindMode* clone() const override { return new VDWindMode(); }
    virtual void x(Matrix&, Matrix&) override;
    virtual void y(Matrix&, Matrix&);

private:
    //! Copy constructor - No copy allowed
    VDWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
    VDWindMode& operator=(const SDWindMode&);
};

template <>
class MagTranslator<string, WindMode> {
public:
    WindMode* operator()(const string& val) { return SimpleObjectMaker<WindMode>::create(val); }

    WindMode* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
