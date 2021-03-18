/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TileDecoder.h
    \brief Definition of the Template class TileDecoder.

    Magics Team - ECMWF 2004

    Started: Thu 25-Mar-2004

    Changes:

*/

#ifndef TileDecoder_H
#define TileDecoder_H

#include "MagException.h"
#include "magics.h"

#include "Data.h"
#include "MagicsDecoder.h"
#include "TileDecoderAttributes.h"
#include "eccodes.h"

namespace magics {


class TileDecoder : public Decoder, public Data, public TileDecoderAttributes {
public:
    TileDecoder();
    virtual ~TileDecoder();
    // implements Decoder interface
    void decode();
    virtual void set(const XmlNode& node) { TileDecoderAttributes::set(node); }
    virtual void set(const map<string, string>& map) { TileDecoderAttributes::set(map); }

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all);

    PointsHandler& points(const Transformation& t, bool);

    PointsHandler& points() { throw MethodNotYetImplemented("TileDecoder::points()"); }
    virtual MatrixHandler& matrix() {
        decode();
        matrixHandlers_.push_back(new MatrixHandler(matrix_));
        matrixHandlers_.back()->setTile();
        return *(matrixHandlers_.back());
    }
    bool ok();

    string projection();
    string weights();
    string positions();
    string positions_symbols();

    Data* current();
    Data* next();

    vector<codes_handle*> entries_;
    vector<codes_handle*>::iterator entry_;
    codes_handle* handle_;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void scaling_offset(codes_handle*, double&, double&);
    Matrix matrix_;
    string grid_;
    PointsList points_;

private:
    //! Copy constructor - No copy allowed
    TileDecoder(const TileDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    TileDecoder& operator=(const TileDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TileDecoder& p) {
        p.print(s);
        return s;
    }
    ifstream file_;
};

}  // namespace magics
#endif
