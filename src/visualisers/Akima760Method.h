/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima760Method.h
    \brief Definition of the Template class Akima760Method.

    Magics Team - ECMWF 2004

    Started: Thu 11-Mar-2004

    Changes:

*/

#ifndef Akima760Method_H
#define Akima760Method_H

#include "magics.h"

#include "Akima760MethodAttributes.h"
#include "ContourMethod.h"

namespace magics {


class Akima760 : public MatrixHandler {
public:
    Akima760(const AbstractMatrix& matrix, const Akima760MethodAttributes&);

    ~Akima760() override;

    double operator()(int i, int j) const override;
    int rowIndex(double r) const override;
    int columnIndex(double r) const override;
    void boundRow(double r, double& row1, int& index1, double& row2, int& index2) const override;

    void boundColumn(double r, double& column1, int& index1, double& column2, int& index2) const override;
    /*
    double interpolate(double  i, double  j) const {
        return this->matrix_.interpolate(i,j);
   }
   */

    int rows() const override { return nrows_; }
    int columns() const override { return ncols_; }

    double regular_row(int i) const override;
    double regular_column(int j) const override;

    double column(int, int) const override;
    double row(int, int) const override;

    double missing() const override { return this->matrix_.missing(); }

    // Compute partial derivatives of a bivariate function
    // on a rectangular grid
    void rgpd3p();

    // Rectangular-grid bivariate interpolation
    double rgbi3p(double xi, double yi) const;

    // Location of the desired points in a rectangular grid
    // Remove  later
    void rglctn(double xi, double yi, int& inxi, int& inyi) const;

    // Polynomials for rectangular-grid bivariate interpolation
    // and surface fitting
    void rgplnl(double xi, double yi, int inxi, int inyi, double& zi) const;

    // Check missing values
    int CheckMissingValues(int col, int lin) const;

    // TEST, REMOVE LATER
    // void test_build_data();

private:
    //     MonotonicIncreasingMatrixHandler mono1_; //Akima needs indexes in the monotonic increasing order
    //     MatrixHandler mono_;
    // It is expected that the input Matrix is monotonic increasing and
    // already cached
    Akima760MethodAttributes attr_;
    int nrows_;       // number of rows
    int ncols_;       // number of columns
    double** WKZX_;   // working array
    double** WKZY_;   // working array
    double** WKZXY_;  // working array
    int monoRows_;
    int monoColumns_;
    bool missingValues_;  // True: data has missing values
    map<double, int> rowsMap_;
    map<double, int> columnsMap_;
    vector<double> rows_;
    vector<double> columns_;
};


class Akima760Method : public ContourMethod, public Akima760MethodAttributes {
public:
    Akima760Method() { MagLog::dev() << "Akima760Method::Akima760Method-->" << *this << "\n"; }
    virtual ~Akima760Method() override {}

    ContourMethod* clone() const override {
        Akima760Method* method = new Akima760Method();
        method->copy(*this);
        return method;
    }

    virtual void set(const map<string, string>& map) override { Akima760MethodAttributes::set(map); }

    virtual void set(const XmlNode& node) override { Akima760MethodAttributes::set(node); }
    virtual bool accept(const string& node) override {
        return Akima760MethodAttributes::accept(node);
        ;
    }

    virtual MatrixHandler* handler(const AbstractMatrix& matrix, const BasicGraphicsObjectContainer&) override {
        return new Akima760(matrix, *this);
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override {
        out << "Akima760Method[";
        Akima760MethodAttributes::print(out);
        out << "]";
    }

private:
    //! Copy constructor - No copy allowed
    Akima760Method(const Akima760Method&);
    //! Overloaded << operator to copy - No copy allowed
    Akima760Method& operator=(const Akima760Method&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Akima760Method& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
