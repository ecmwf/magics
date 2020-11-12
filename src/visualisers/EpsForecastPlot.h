/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsForecastPlot.h
    \brief Definition of the Template class EpsForecastPlot.

    Magics Team - ECMWF 2005

    Started: Thu 15-Dec-2005

    Changes:

*/

#ifndef EpsForecastPlot_H
#define EpsForecastPlot_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class EpsForecastPlot {
public:
    EpsForecastPlot() {}
    virtual ~EpsForecastPlot() override {}

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual EpsForecastPlot* clone() const override { return new EpsForecastPlot(); }

    virtual bool forecast() override { return true; }
    virtual bool control() override { return true; }

    virtual void toxml(ostream&, int) const {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "EpsForecastPlot[]"; }

private:
    //! Copy constructor - No copy allowed
    EpsForecastPlot(const EpsForecastPlot&);
    //! Overloaded << operator to copy - No copy allowed
    EpsForecastPlot& operator=(const EpsForecastPlot&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsForecastPlot& p) {
        p.print(s);
        return s;
    }
};

class EpsForecastOnly : public EpsForecastPlot {
public:
    EpsForecastOnly() {}
    virtual ~EpsForecastOnly() override {}

    virtual EpsForecastPlot* clone() const override { return new EpsForecastOnly(); }

    virtual bool forecast() override { return true; }
    virtual bool control() override { return false; }
};

class EpsControlOnly : public EpsForecastPlot {
public:
    EpsControlOnly() {}
    virtual ~EpsControlOnly() override {}

    virtual EpsForecastPlot* clone() const override { return new EpsControlOnly(); }

    virtual bool forecast() override { return false; }
    virtual bool control() override { return true; }
};

class EpsNoForecast : public EpsForecastPlot {
public:
    EpsNoForecast() {}
    virtual ~EpsNoForecast() override {}

    virtual EpsForecastPlot* clone() const override { return new EpsNoForecast(); }

    virtual bool forecast() override { return false; }
    virtual bool control() override { return false; }
};

template <>
class MagTranslator<string, EpsForecastPlot> {
public:
    EpsForecastPlot* operator()(const string& val) { return SimpleObjectMaker<EpsForecastPlot>::create(val); }

    EpsForecastPlot* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
