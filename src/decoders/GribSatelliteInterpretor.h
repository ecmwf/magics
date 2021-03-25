/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribSatelliteInterpretor.h
    \brief Definition of the Template class GribSatelliteInterpretor.

    Magics Team - ECMWF 2005

    Started: Mon 18-Apr-2005

    Changes:

*/

#ifndef GribSatelliteInterpretor_H
#define GribSatelliteInterpretor_H

#include "magics.h"

#include "GribInterpretor.h"

namespace magics {

class GribSatelliteInterpretor : public GribInterpretor {
public:
    GribSatelliteInterpretor();
    virtual ~GribSatelliteInterpretor();

    //  virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const;
    virtual void interpretAsVectors(const GribDecoder&, Matrix&, Matrix&) const {
        throw NotYetImplemented("Satellite representation", " Vectors");
    }
    virtual void interpretAsMatrix(GribDecoder&) const;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    GribSatelliteInterpretor(const GribSatelliteInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    void AdjustBadlyEncodedGribs(int satId, int chanId, long& nx, long& ny, long& dx, long& dy, long& xp, long& yp,
                                 double& slon, long& functionCode) const;


    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribSatelliteInterpretor& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
