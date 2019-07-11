/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsStatDecoder.h
    \brief Definition of the Template class ObsStatDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 17-Oct-2005

    Changes:

*/

#ifndef ObsStatDecoder_H
#define ObsStatDecoder_H

#include "magics.h"

#include "ObsStatDecoderAttributes.h"
//#include "BaseSceneObject.h"
#include "Data.h"
#include "MagicsDecoder.h"
#include "PaperPoint.h"


namespace magics {

class StatItem {
public:
    StatItem(const string&, ifstream&);
    virtual ~StatItem() {}
    map<string, int> columns_;
    vector<vector<double> > rows_;
    map<string, vector<string> > definitions_;
    string name_;
    virtual void print(ostream&) const;
    friend ostream& operator<<(ostream& s, const StatItem& p) {
        p.print(s);
        return s;
    }
};

class StatDef {
public:
    StatDef(const string&, ifstream&);
    virtual ~StatDef() {}
    map<string, vector<string> > definitions_;
    vector<StatItem> data_;
    string name_;
    virtual void print(ostream&) const;
    friend ostream& operator<<(ostream& s, const StatDef& p) {
        p.print(s);
        return s;
    }
};

class ObsStatDecoder : public ObsStatDecoderAttributes, public Data, public PointsList {
public:
    ObsStatDecoder();
    virtual ~ObsStatDecoder();

    void set(const map<string, string>& map) { ObsStatDecoderAttributes::set(map); }
    void set(const XmlNode& node) { ObsStatDecoderAttributes::set(node); }

    virtual void decode();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    virtual PointsHandler& points();
    void customisedPoints(const Transformation&, const std::set<string>& needs, CustomisedPointsList& out, bool) {
        customisedPoints(needs, out);
    }
    PointsHandler& points(const Transformation&, bool) { return points(); }


    string version_;
    map<string, StatDef> data_;

private:
    //! Copy constructor - No copy allowed
    ObsStatDecoder(const ObsStatDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    ObsStatDecoder& operator=(const ObsStatDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ObsStatDecoder& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
