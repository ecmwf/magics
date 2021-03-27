/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file InputData.h
    \brief Definition of the Template class InputData.

    Magics Team - ECMWF 2004

    Started: Thu 6-May-2004

    Changes:

*/

#ifndef InputData_H
#define InputData_H

#include "MagException.h"
#include "magics.h"

#include "Data.h"
#include "InputDataAttributes.h"
#include "Matrix.h"
#include "UserPoint.h"

namespace magics {


class InputData : public Data, public InputDataAttributes, public PointsList {
public:
    InputData() : matrix_(0) {}
    virtual ~InputData() {}

    void prepare();

    void set(const map<string, string>& map) { InputDataAttributes::set(map); }
    void set(const XmlNode& node) { InputDataAttributes::set(node); }
    void visit(Transformation& transformation);


    MatrixHandler& matrix();
    virtual PointsHandler& points(const Transformation&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void getReady(const Transformation&);
    void visit(ValuesCollector&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void dateSetting(vector<string>&, vector<double>&, DateTime&, bool);
    void numberSetting(vector<double>&, vector<double>&);

    vector<double> x_values_;
    vector<double> y_values_;
    vector<double> x2_values_;
    vector<double> y2_values_;

    DateTime baseDateX_;
    DateTime baseDateY_;

    Matrix* matrix_;

private:
    //! Copy constructor - No copy allowed
    InputData(const InputData&);
    //! Overloaded << operator to copy - No copy allowed
    InputData& operator=(const InputData&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const InputData& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
