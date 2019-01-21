/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotDecoder.h
    \brief Definition of the Template class BoxPlotDecoder.

    Magics Team - ECMWF 2005

    Started: Thu 29-Sep-2005

    Changes:

*/

#ifndef BoxPlotDecoder_H
#define BoxPlotDecoder_H

#include "magics.h"

#include "BoxPlotDecoderAttributes.h"
#include "Data.h"
#include "Decoder.h"
#include "UserPoint.h"

namespace magics {

class BoxPlotDecoder : public BoxPlotDecoderAttributes, public Data, public PointsList {
public:
    BoxPlotDecoder();
    virtual ~BoxPlotDecoder();

    virtual void set(const map<string, string>& map) { BoxPlotDecoderAttributes::set(map); }

    virtual void set(const XmlNode& node) { BoxPlotDecoderAttributes::set(node); }

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all) {
        customisedPoints(n, out);
    }
    PointsHandler& points(const Transformation&, bool) { NOTIMP; }
    void getReady(const Transformation&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    BoxPlotDecoder(const BoxPlotDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    BoxPlotDecoder& operator=(const BoxPlotDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BoxPlotDecoder& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
