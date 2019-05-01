/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsJSon.h
    \brief Definition of the Template classBufrJSon.

    Magics Team - ECMWF 2005

    Started: Mon 19-Sep-2011

    Changes:

*/

#ifndef ObsJSon_H
#define ObsJSon_H

#include "magics.h"


#include "Data.h"
#include "MagicsDecoder.h"
#include "ObsJSonAttributes.h"
#include "UserPoint.h"
#include "json_spirit.h"

namespace magics {


class ObsJSon : public ObsJSonAttributes, public Decoder, public Data, public PointsList {
public:
    ObsJSon();
    virtual ~ObsJSon();

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all);

    PointsHandler& points(const Transformation&, bool) { NOTIMP; }

    virtual void set(const map<string, string>& map) { ObsJSonAttributes::set(map); }
    virtual void set(const XmlNode& node) { ObsJSonAttributes::set(node); }

    void decode();
    void getInfo(const std::set<string>&, multimap<string, string>&);
    void visit(MetaDataVisitor&);
    CustomisedPoint* decode(json_spirit::Object&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    vector<CustomisedPoint*> points_;
    std::set<string> types_;
    typedef void (ObsJSon::*Method)(const json_spirit::Value&, CustomisedPoint&);
    map<string, Method> methods_;

    void latitude(const json_spirit::Value&, CustomisedPoint&);
    void longitude(const json_spirit::Value&, CustomisedPoint&);
    void type(const json_spirit::Value&, CustomisedPoint&);
    void identifier(const json_spirit::Value&, CustomisedPoint&);

private:
    //! Copy constructor - No copy allowed
    ObsJSon(const ObsJSon&);
    //! Overloaded << operator to copy - No copy allowed
    ObsJSon& operator=(const ObsJSon&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ObsJSon& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics
#endif
