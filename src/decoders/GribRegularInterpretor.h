/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribRegularInterpretor.h
    \brief Definition of the Template class GribRegularInterpretor.

    Magics Team - ECMWF 2005

    Started: Mon 18-Apr-2005

    Changes:

*/

#ifndef GribRegularInterpretor_H
#define GribRegularInterpretor_H

#include "magics.h"

#include "GribInterpretor.h"

namespace magics {

class GribRegularInterpretor : public GribInterpretor {
public:
    GribRegularInterpretor();
    virtual ~GribRegularInterpretor() override;

    void interpretAsMatrix(GribDecoder&) const override;
    virtual double XResolution(const GribDecoder& grib) const override { return longitudeIncrement(grib); }
    void interpretAsMatrix(GribDecoder&, const Transformation&) const override;
    virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const override;
    virtual void latitudes(const GribDecoder&, vector<double>&) const;
    virtual double longitudeIncrement(const GribDecoder&) const;
    void index(const GribDecoder& grib) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;


private:
    //! Copy constructor - No copy allowed
    GribRegularInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    GribRegularInterpretor& operator=(const GribRegularInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribRegularInterpretor& p) {
        p.print(s);
        return s;
    }
};

class GribReducedGaussianInterpretor : public GribInterpretor {
public:
    GribReducedGaussianInterpretor() {}
    virtual ~GribReducedGaussianInterpretor() override {}

    virtual double XResolution(const GribDecoder& grib) const override;
    void interpretAsMatrix(GribDecoder&) const override;
    void interpretAsMatrix(GribDecoder&, const Transformation&) const override;
    void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const override;
    void index(const GribDecoder& grib) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    GribReducedGaussianInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    GribReducedGaussianInterpretor& operator=(const GribRegularInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribReducedGaussianInterpretor& p) {
        p.print(s);
        return s;
    }
};

class GribReducedLatLonInterpretor : public GribInterpretor {
public:
    GribReducedLatLonInterpretor() {}
    virtual ~GribReducedLatLonInterpretor() override {}

    void interpretAsMatrix(GribDecoder&) const override;
    virtual double XResolution(const GribDecoder& grib) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    GribReducedLatLonInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    GribReducedLatLonInterpretor& operator=(const GribRegularInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribReducedLatLonInterpretor& p) {
        p.print(s);
        return s;
    }
};

class GribRotatedInterpretor : public GribRegularInterpretor {
public:
    GribRotatedInterpretor() : original_(false) {}
    virtual ~GribRotatedInterpretor() override {}

    std::pair<double, double> unrotate(double lat, double lon) const;
    std::pair<double, double> rotate(double lat, double lon) const;
    PaperPoint reference(const GribDecoder&, const Transformation&) override;
    void raw(GribDecoder&, const Transformation&, const string&,
             map<double, map<double, CustomisedPoint*> >&) const override;
    void keepOriginal(bool original) override { original_ = original; }
    void interpretAsMatrix(GribDecoder&) const override;
    void interpret2D(double&, double&, double&, double&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    mutable double southPoleLat_;
    mutable double southPoleLon_;
    mutable double angle_;
    mutable bool uvRelativeToGrid_;

    bool original_;
};

class GribLambertAzimutalInterpretor : public GribRegularInterpretor {
public:
    GribLambertAzimutalInterpretor() {}
    virtual ~GribLambertAzimutalInterpretor() override {}

    UserPoint unrotate(double lat, double lon) const;
    void interpretAsMatrix(GribDecoder&) const override;
    PaperPoint reference(const GribDecoder&, const Transformation&) override;
    double XResolution(const GribDecoder& grib) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    mutable double standardParallel_;
    mutable double centralLongitude_;
};

class GribLambertInterpretor : public GribLambertAzimutalInterpretor {
public:
    GribLambertInterpretor() {}
    virtual ~GribLambertInterpretor() override {}


    void interpretAsMatrix(GribDecoder&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};


class GribPolarStereoInterpretor : public GribRegularInterpretor {
public:
    GribPolarStereoInterpretor() {}
    virtual ~GribPolarStereoInterpretor() override {}


    void interpretAsMatrix(GribDecoder&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};

class GribProjInterpretor : public GribRegularInterpretor {
public:
    GribProjInterpretor() {}
    virtual ~GribProjInterpretor() override {}


    void interpretAsMatrix(GribDecoder&) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
};


class GribRegularGaussianInterpretor : public GribRegularInterpretor {
public:
    GribRegularGaussianInterpretor() {}
    virtual ~GribRegularGaussianInterpretor() override {}

    void latitudes(const GribDecoder&, vector<double>&) const override;

protected:
};


}  // namespace magics
#endif
