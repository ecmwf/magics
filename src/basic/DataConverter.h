/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DataConverter.h
    \brief Definition of the Template class DataConverter.

    Magics Team - ECMWF 2009

    Started: Wed 22-Jul-2009

    Changes:

*/

#ifndef DataConverter_H
#define DataConverter_H

#include "Data.h"
#include "magics.h"


namespace magics {

class DataConverter : public Data {
public:
    DataConverter(Data*);
    virtual ~DataConverter() override;
    virtual MatrixHandler& matrix() override { throw MethodNotYetImplemented("DataConverter::matrix"); }

    //! Methods to access the data as a 2Dmatrix Used by pwind action routine
    virtual MatrixHandler& xComponent() override {
        throw MethodNotYetImplemented("DataConverter::yComponent") override;
    }
    virtual MatrixHandler& yComponent() override {
        throw MethodNotYetImplemented("DataConverter::yComponent") override;
    }

    //! Method to access the data as a list of points : Used by psymb.
    virtual PointsHandler& points() override;


    virtual void getReady(const Transformation&) {}
    virtual void visit(Transformation&) override {}


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    Data* data_;
    PointsList points_;

private:
    //! Copy constructor - No copy allowed
    DataConverter(const DataConverter&);
    //! Overloaded << operator to copy - No copy allowed
    DataConverter& operator=(const DataConverter&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const DataConverter& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
