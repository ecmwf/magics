/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Image.h
    \brief Definition of the Template class Image.

    Magics Team - ECMWF 2005

    Started: Wed 6-Apr-2005

    Changes:

*/

#ifndef Image_H
#define Image_H

#include "BaseDriver.h"
#include "ImageProperties.h"
#include "magics.h"

namespace magics {

class Image : public ImageProperties, public vector<short> {
public:
    Image() {}
    virtual ~Image() {}

    // Implement the BaseGraphics Interface
    virtual void redisplay(const BaseDriver& driver) const { driver.redisplay(*this); }

    void set(int rows, int columns) {
        rows_    = rows;
        columns_ = columns;
        reserve(rows_ * columns_);
    }

    int getNumberOfRows() const { return rows_; }
    int getNumberOfColumns() const { return columns_; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    int rows_;
    int columns_;

private:
    //! Copy constructor - No copy allowed
    Image(const Image&);
    //! Overloaded << operator to copy - No copy allowed
    Image& operator=(const Image&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Image& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
