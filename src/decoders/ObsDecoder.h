/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsDecoder.h
    \brief Definition of the Template class ObsDecoder.

    Magics Team - ECMWF 2005

    Started: Wed 23-Mar-2005

    Changes:

*/

#ifndef ObsDecoder_H
#define ObsDecoder_H

#include "magics.h"

#include "Data.h"
#include "MagicsDecoder.h"
#include "ObsDecoderAttributes.h"

namespace magics {

class BufrIdentifiers : public map<string, string> {
public:
    BufrIdentifiers(int centre);
    ~BufrIdentifiers() {}
    void set(const map<string, string>&);
    int ident(const string&) const;  // Return -1 if ident does not exist.
protected:
    int centre_;
};

class BufrIdentTable : public map<int, BufrIdentifiers*> {
public:
    BufrIdentTable() {}
    static const BufrIdentifiers& get(int centre);

protected:
    static BufrIdentTable table_;
};


class TitleNode;

/*! \brief Reader for obs data in BUFR

*/
class ObsDecoder : public ObsDecoderAttributes, public Decoder, public Data, public PointsList {
public:
    ObsDecoder();
    virtual ~ObsDecoder();
    virtual void set(const map<string, string>& map) { ObsDecoderAttributes::set(map); }

    virtual void decode();

    bool defined() { return file_name_.empty() == false; }
    void getInfo(const std::set<string>&, multimap<string, string>&);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool) {
        customisedPoints(t, n, out);
    }
    virtual PointsHandler& points();
    PointsHandler& points(const Transformation&, bool) { return points(); }
    void visit(TitleNode&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string title_;

    bool findInTypes(const string&);
    bool checkLevel(double);

private:
    //! Copy constructor - No copy allowed
    ObsDecoder(const ObsDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    ObsDecoder& operator=(const ObsDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ObsDecoder& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
