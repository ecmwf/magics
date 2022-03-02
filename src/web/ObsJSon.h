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

namespace magics {

class Value;
class ValueMap;

class ObsJSon : public ObsJSonAttributes, public Decoder, public Data, public PointsList {
public:
    ObsJSon();
    virtual ~ObsJSon() override;

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out,
                          bool all) override;

    PointsHandler& points(const Transformation&, bool) override { NOTIMP; }

    virtual void set(const map<string, string>& map) override { ObsJSonAttributes::set(map); }
    virtual void set(const XmlNode& node) override { ObsJSonAttributes::set(node); }

    void decode() override;
    void getInfo(const std::set<string>&, multimap<string, string>&) override;
    void visit(MetaDataVisitor&) override;
    CustomisedPoint* decode(ValueMap&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    vector<CustomisedPoint*> points_;
    std::set<string> types_;
    typedef void (ObsJSon::*Method)(const Value&, CustomisedPoint&);
    map<string, Method> methods_;

    void latitude(const Value&, CustomisedPoint&);
    void longitude(const Value&, CustomisedPoint&);
    void type(const Value&, CustomisedPoint&);
    void identifier(const Value&, CustomisedPoint&);
    void number(const Value&, CustomisedPoint&, const string&);


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
