/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MatrixTestDecoder.h
    \brief Definition of the Template class MatrixTestDecoder.

    Magics Team - ECMWF 2004

    Started: Thu 25-Mar-2004

    Changes:

*/

#ifndef MatrixTestDecoder_H
#define MatrixTestDecoder_H

#include "MagException.h"
#include "magics.h"

#include "Data.h"
#include "MagicsDecoder.h"


namespace magics {

class MatrixTestDecoder : public Decoder, public Data {
public:
    MatrixTestDecoder();
    virtual ~MatrixTestDecoder();
    // implements Decoder interface
    void decode() {}

    virtual void set(const map<string, string>&) {}

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all) {
        NOTIMP;
    }

    PointsHandler& points(const Transformation& t, bool) { NOTIMP; }


    PointsHandler& points() { throw MethodNotYetImplemented("MatrixTestDecoder::points()"); }
    virtual MatrixHandler& matrix() {
        decode();
        matrixHandlers_.push_back(new MatrixHandler(matrix_));
        return *(matrixHandlers_.back());
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    Matrix matrix_;

private:
    //! Copy constructor - No copy allowed
    MatrixTestDecoder(const MatrixTestDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    MatrixTestDecoder& operator=(const MatrixTestDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MatrixTestDecoder& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
