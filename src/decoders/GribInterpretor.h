/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribInterpretor.h
    \brief Definition of the Template class GribInterpretor.

    Magics Team - ECMWF 2005

    Started: Mon 18-Apr-2005

    Changes:

*/

#ifndef GribInterpretor_H
#define GribInterpretor_H

#include "Factory.h"
#include "MagTranslator.h"
#include "UserPoint.h"
#include "CustomisedPoint.h"
#include "magics.h"


namespace magics {

class GribDecoder;
class Matrix;
class Transformation;


class RasterData;

struct Index {
    int index_;
    double lat_;
    double lon_;
    bool used_;
    Index(int index, double lat, double lon) : index_(index), lat_(lat), lon_(lon), used_(false) {}
    void print() { cout << "[" << lat_ << ", " << lon_ << "]" << endl; }
};

class GribInterpretor {
public:
    GribInterpretor() {}
    virtual ~GribInterpretor() {}
    virtual double XResolution(const GribDecoder&) const { return 0; }
    virtual void raw(GribDecoder&, const Transformation&, const string&,
                     map<double, map<double, CustomisedPoint*> >&) const;
    virtual void raw(GribDecoder&, const Transformation&,
                     vector<pair<double, vector<pair<double, CustomisedPoint*> > > >&, double&, double&) const;

    virtual void interpretAsMatrix(const GribDecoder&, Matrix** matrix, Matrix** matrix2 = NULL) const { *matrix = 0; }
    virtual void interpretAsMatrix(const GribDecoder& grib, Matrix** matrix, const Transformation&) const {
        interpretAsMatrix(grib, matrix);
    }


    virtual void interpret2D(double&, double&, double&, double&) const {}
    virtual void keepOriginal(bool) {}
    virtual PaperPoint reference(const GribDecoder&, const Transformation&);
    virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const {}
    virtual void scaling(GribDecoder&, Matrix**) const;
    virtual void scaling(GribDecoder& grib, double& scaling, double& offset) const;
    virtual void scaling(GribDecoder& grib, double& scaling, double& offset, string& originalUnits,
                         string& derivedUnits) const;
    void longitudesSanityCheck(double&, double&) const;

    void interpolate(const GribDecoder& grib, Matrix& matrix) const;

    virtual void index(const GribDecoder& grib);
    virtual void new_index(const GribDecoder& grib);

    virtual int nearest(double, double, double&, double&);
    Index nearest(double, double);
    double west_;
    double east_;

    vector<vector<Index> > helper_;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "GribInterpretor" << endl; }
    map<double, map<double, int> > index_;
    int indexLon_;
    int indexLat_;
    double indexStep_;
    double minlat_;
    double maxlat_;
    double minlon_;
    double maxlon_;


private:
    //! Copy constructor - No copy allowed
    GribInterpretor(const GribInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    GribInterpretor& operator=(const GribInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribInterpretor& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, GribInterpretor> {
public:
    MAGICS_NO_EXPORT GribInterpretor* operator()(const string& val) {
        return SimpleObjectMaker<GribInterpretor>::create(val);
    }
    MAGICS_NO_EXPORT GribInterpretor* magics(const string& param) {
        GribInterpretor* object;
        ParameterManager::update(param, object);
        return object;
    }
};


}  // namespace magics
#endif
