/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfGeopointsInterpretor.h
    \brief Definition of the Template class NetcdfGeopointsInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#ifndef NetcdfGeopointsInterpretor_H
#define NetcdfGeopointsInterpretor_H

#include "magics.h"


#include "Matrix.h"
#include "NetcdfInterpretor.h"
#include "XmlNode.h"
namespace magics {

class NetcdfGeopointsInterpretor : public NetcdfInterpretor {
public:
    NetcdfGeopointsInterpretor();
    virtual ~NetcdfGeopointsInterpretor() override;

    void set(const map<string, string>& params) override {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)"
                        << "\n";
        NetcdfInterpretor::set(params);
    }
    void set(const XmlNode& node) override {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)"
                        << "\n";
        XmlNode netcdf = node;
        NetcdfInterpretor::set(node);
        netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf);
    }
    virtual NetcdfInterpretor* clone() const override {
        NetcdfGeopointsInterpretor* object = new NetcdfGeopointsInterpretor();
        object->clone(*this);
        return object;
    }
    void clone(const NetcdfGeopointsInterpretor& other) { copy(other); }
    bool interpretAsPoints(PointsList&) override;
    bool interpretAsPoints(PointsList&, const Transformation&) override;
    bool interpretAsMatrix(Matrix**) override { return false; }
    virtual void visit(MetaDataCollector&) override;
    virtual void visit(ValuesCollector&, PointsList&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    NetcdfGeopointsInterpretor(const NetcdfGeopointsInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfGeopointsInterpretor& operator=(const NetcdfGeopointsInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfGeopointsInterpretor& p) {
        p.print(s);
        return s;
    }
};
class NetcdfXYpointsInterpretor : public NetcdfInterpretor {
public:
    NetcdfXYpointsInterpretor();
    virtual ~NetcdfXYpointsInterpretor() override;

    void set(const map<string, string>& params) override {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)"
                        << "\n";
        NetcdfInterpretor::set(params);
    }
    void set(const XmlNode& node) override {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)"
                        << "\n";
        XmlNode netcdf = node;
        NetcdfInterpretor::set(node);
        netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf);
    }
    virtual NetcdfInterpretor* clone() const override {
        NetcdfXYpointsInterpretor* object = new NetcdfXYpointsInterpretor();
        object->clone(*this);
        return object;
    }
    void clone(const NetcdfXYpointsInterpretor& other) { copy(other); }
    bool interpretAsPoints(PointsList&, const std::set<string>&);
    bool interpretAsPoints(PointsList&) override;
    bool interpretAsPoints(PointsList&, const Transformation&) override;
    bool interpretAsMatrix(Matrix**) override { return false; }
    virtual void visit(Transformation&) override;
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&,
                                  int s) override;
    virtual void visit(MetaDataCollector&) override;
    virtual void visit(ValuesCollector&, PointsList&) override;
    void visit(TextVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    NetcdfXYpointsInterpretor(const NetcdfXYpointsInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfXYpointsInterpretor& operator=(const NetcdfXYpointsInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfXYpointsInterpretor& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
