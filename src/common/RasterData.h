/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file RasterData.h
    \brief Definition of the Template class RasterData.

    Magics Team - ECMWF 2005

    Started: Tue 12-Apr-2005

    Changes:

*/

#ifndef RasterData_H
#define RasterData_H

#include <algorithm>
#include "magics.h"

class TeProjection;


namespace magics {

class Raster : public vector<double> {
public:
    Raster() {}
    virtual ~Raster() {}
    double operator()(int row, int column) const { return (*this)[row * columns_ + column]; }
    double min() const {
        if (this->empty())
            return -1.;
        return *(std::min_element(begin(), end()));
    }
    double max() const {
        if (this->empty())
            return -1.;
        return *(std::max_element(begin(), end()));
    }
    inline int getRows() const { return rows_; }
    inline int getColumns() const { return columns_; }
    inline void setColumns(int columns) { columns_ = columns; }
    inline void setRows(int rows) { rows_ = rows; }

protected:
    virtual void print(ostream& out) const {
        out << "Raster[";
        out << size() << " points, ";
        out << min() << ", " << max() << "]";
    }
    int rows_;
    int columns_;


private:
    //! Copy constructor - No copy allowed
    Raster(const Raster&);
    //! Overloaded << operator to copy - No copy allowed
    Raster& operator=(const Raster&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Raster& p) {
        p.print(s);
        return s;
    }
};


class RasterData : public Raster {
public:
    RasterData() {}
    virtual ~RasterData() override {}

    TeProjection& getProjection() { return *projection_; }

    inline double getXResolution() const { return x_; }
    inline double getYResolution() const { return y_; }

    inline const UserPoint& getLowerLeftCorner() const { return lowerLeft_; }
    inline const UserPoint& getUpperRightCorner() const { return upperRight_; }

    inline void setXResolution(double x) { x_ = x; }
    inline void setYResolution(double y) { y_ = y; }

    inline void setUpperRightCorner(double lon, double lat) { upperRight_ = UserPoint(lon, lat); }
    inline void setLowerLeftCorner(double lon, double lat) { lowerLeft_ = UserPoint(lon, lat); }

    inline void setProjection(TeProjection* projection) { projection_ = projection; }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override { s << "RasterData[]"; }
    TeProjection* projection_;
    double x_;
    double y_;
    UserPoint lowerLeft_;
    UserPoint upperRight_;

private:
    //! Copy constructor - No copy allowed
    RasterData(const RasterData&);
    //! Overloaded << operator to copy - No copy allowed
    RasterData& operator=(const RasterData&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const RasterData& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
