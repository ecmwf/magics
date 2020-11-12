/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file NetcdfGeoMatrixInterpretor.h
    \brief Definition of the Template class NetcdfGeoMatrixInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#ifndef NetcdfGeoMatrixInterpretor_H
#define NetcdfGeoMatrixInterpretor_H

#include "ProjP.h"

#include "Matrix.h"
#include "NetcdfInterpretor.h"
#include "XmlNode.h"

namespace magics {

class NetcdfGeoMatrixInterpretor : public NetcdfInterpretor {
public:
    NetcdfGeoMatrixInterpretor();
    virtual ~NetcdfGeoMatrixInterpretor() override;

    static NetcdfInterpretor* guess(const NetcdfInterpretor&);
    void visit(Transformation& transformation) override;

    void set(const XmlNode& node) override {
        // FIXME: Infinit recursion
        MagLog::debug() << "NetcdfGeoMatrixInterpretor::set(params)"
                        << "\n";
        set(node);
        XmlNode netcdf = node;
        netcdf.name("netcdf");
        set(netcdf);
    }
    virtual NetcdfInterpretor* clone() const override {
        NetcdfGeoMatrixInterpretor* object = new NetcdfGeoMatrixInterpretor();
        object->clone(*this);
        return object;
    }
    void clone(const NetcdfGeoMatrixInterpretor& other) { copy(other); }
    bool interpretAsMatrix(Matrix**) override;
    bool interpretAsPoints(PointsList&) override;
    UserPoint* newPoint(const string&, double, double, double);
    virtual void statsData(map<string, vector<double> >&) override;
    virtual void visit(MetaDataCollector&) override;
    virtual void visit(ValuesCollector&, PointsList&) override;
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int) override;
    string proj4Detected(Netcdf& netcdf);
    void checkProj4Units(Netcdf& netcdf, const string& variable, vector<double>& data);

protected:
    //! Method to print string about this class on to a stream of type ostream
    //! (virtual).
    virtual void print(ostream&) const override;
    std::unique_ptr<Matrix> matrix_;
    LatLonProjP projection_;

private:
    //! Copy constructor - No copy allowed
    NetcdfGeoMatrixInterpretor(const NetcdfGeoMatrixInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfGeoMatrixInterpretor& operator=(const NetcdfGeoMatrixInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfGeoMatrixInterpretor& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
