/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsgramDecoder.h
    \brief Definition of the Template class EpsgramDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 19-Sep-2005

    Changes:

*/

#ifndef EpsBufr_H
#define EpsBufr_H

#include "magics.h"


#include "Data.h"
#include "DateTime.h"
#include "Decoder.h"
#include "EpsBufrAttributes.h"
#include "UserPoint.h"

#include <limits>

namespace magics {


class EpsBufr : public Decoder, public Data, public PointsList, public EpsBufrAttributes {
public:
    EpsBufr();
    virtual ~EpsBufr();

    virtual void set(const map<string, string>& map) { EpsBufrAttributes::set(map); }
    virtual void set(const XmlNode& node) { EpsBufrAttributes::set(node); }

    virtual void visit(Transformation&);
    void visit(const XmlNode& node);

    virtual void decode();

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    virtual PointsHandler& points();
    virtual void visit(TextVisitor&);
    virtual void visit(MetaDataVisitor&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool) {
        customisedPoints(n, out);
    }

    PointsHandler& points(const Transformation&, bool) { return points(); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    DateTime base_;
    vector<CustomisedPoint*> points_;
    double minstep_;
    double maxstep_;
    double miny_;
    double maxy_;
    double shift_;


private:
    //! Copy constructor - No copy allowed
    EpsBufr(const EpsBufr&);
    //! Overloaded << operator to copy - No copy allowed
    EpsBufr& operator=(const EpsBufr&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsBufr& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
