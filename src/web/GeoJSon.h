/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#ifndef GeoJSon_H
#define GeoJSon_H

#include "magics.h"


#include "GeoJSonAttributes.h"

#include "Data.h"
#include "MagDateTime.h"
#include "MagicsDecoder.h"
#include "PointsHandler.h"
#include "UserPoint.h"
#include "json_spirit.h"

#include <limits>

namespace magics {

class GeoObject;

class GeoJSon : public Data, public PointsList, public GeoJSonAttributes {
public:
    GeoJSon();
    virtual ~GeoJSon();

    typedef void (GeoJSon::*Method)(const json_spirit::Value&);

    map<string, Method> methods_;

    void points(const Transformation&, vector<UserPoint>&);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    PointsHandler& points(const Transformation&, bool);
    MatrixHandler& matrix();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void decode();
    void dig(const json_spirit::Value&);
    void coordinates(const json_spirit::Value&);
    void properties(const json_spirit::Value&);
    void type(const json_spirit::Value&);
    void geometry(const json_spirit::Value&);
    void features(const json_spirit::Value&);
    string find(json_spirit::Object&, const string&);

    vector<CustomisedPoint*> points_;
    GeoObject* current_;
    GeoObject* parent_;
    Matrix* matrix_;

private:
    //! Copy constructor - No copy allowed
    GeoJSon(const GeoJSon&);
    //! Overloaded << operator to copy - No copy allowed
    GeoJSon& operator=(const GeoJSon&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GeoJSon& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
