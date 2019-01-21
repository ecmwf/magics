/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDecoder.h
    \brief Definition of the Template class TableDecoder.

    Magics Team - ECMWF 2004

    Started: Thu 6-May-2004

    Changes:

*/

#ifndef TableDecoder_H
#define TableDecoder_H

#include "MagException.h"
#include "magics.h"

#include "Data.h"
#include "Matrix.h"
#include "TableDecoderAttributes.h"
#include "TableReader.h"
#include "UserPoint.h"

namespace magics {

class TableDecoder : public Data, public TableDecoderAttributes, public PointsList {
public:
    TableDecoder();
    virtual ~TableDecoder() {}

    void prepareXY();
    void prepareGeo();

    void set(const map<string, string>& map) { TableDecoderAttributes::set(map); }
    void set(const XmlNode& node) { TableDecoderAttributes::set(node); }
    void visit(Transformation& transformation);
    void visit(TextVisitor& title);

    PointsHandler& points(const Transformation& transformation, bool);

    PointsHandler& points() {
        this->pointsHandlers_.push_back(new PointsHandler(*this));
        return *(this->pointsHandlers_.back());
    }

    MatrixHandler& matrix();
    bool check(const Transformation&, UserPoint*, stack<UserPoint>&);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void getReady(const Transformation&);
    string legendText(double, double);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    void visit(ValuesCollector&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void dateSetting(vector<string>&, vector<double>&, DateTime&);
    void numberSetting(vector<double>&, vector<double>&);
    void indexGeoMode(TableReader&);
    void nameGeoMode(TableReader&);
    void indexXYMode(TableReader&);
    void nameXYMode(TableReader&);
    double indexToNumber(const string&);

    vector<double> x_values_;
    vector<double> y_values_;
    vector<double> xc_values_;
    vector<double> yc_values_;
    vector<double> v_values_;
    vector<string> x_date_values_;
    vector<string> y_date_values_;

    string x_name_;
    string y_name_;
    string xc_name_;
    string yc_name_;
    string v_name_;

    DateTime baseDateX_;
    DateTime baseDateY_;

    Matrix* matrix_;
    Transformation::CoordinateType coordinate_;

private:
    //! Copy constructor - No copy allowed
    TableDecoder(const TableDecoder&);
    //! Overloaded << operator to copy - No copy allowed
    TableDecoder& operator=(const TableDecoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TableDecoder& p) {
        p.print(s);
        return s;
    }
};
}  // namespace magics


#endif
