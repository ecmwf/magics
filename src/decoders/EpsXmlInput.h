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

#ifndef EpsXmlDecoder_H
#define EpsXmlDecoder_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "Data.h"
#include "EpsXmlInputAttributes.h"
#include "MagDateTime.h"
#include "MagicsDecoder.h"
#include "UserPoint.h"
#include "XmlReader.h"

#include <limits>

namespace magics {


class EpsXmlInput : public Decoder,
                    public Data,
                    public PointsList,
                    public EpsXmlInputAttributes,
                    public XmlNodeVisitor {
public:
    EpsXmlInput();
    virtual ~EpsXmlInput() override;

    virtual void set(const map<string, string>& map) override { EpsXmlInputAttributes::set(map); }
    virtual void set(const XmlNode& node) override { EpsXmlInputAttributes::set(node); }

    virtual void visit(Transformation&) override;
    void visit(const XmlNode& node) override;

    virtual void decode() override;

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out,
                          bool all) override {
        customisedPoints(n, out);
    }
    PointsHandler& points(const Transformation& t, bool) override { NOTIMP; }

    virtual void visit(TextVisitor&) override;
    virtual void visit(MetaDataVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    DateTime base_;
    vector<CustomisedPoint*> points_;
    double minstep_;
    double maxstep_;
    double miny_;
    double maxy_;
    string station_;
    double latitude_;
    double longitude_;
    string title_;
    string parameter_;
    int dateOffset_;


private:
    //! Copy constructor - No copy allowed
    EpsXmlInput(const EpsXmlInput&);
    //! Overloaded << operator to copy - No copy allowed
    EpsXmlInput& operator=(const EpsXmlInput&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsXmlInput& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
