/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeBox.h
    \brief This file contains structures and definitions to deal with a box
*/
#ifndef __TERRALIB_INTERNAL_BOX_H
#define __TERRALIB_INTERNAL_BOX_H

#include "TeCoord2D.h"
#include "TeDefines.h"
#include "float.h"

class TeGeometry;
class TePolygon;

//! Provides support for dealing with a rectangular a box. Used by all geometrical representations
/*!
 \sa
    TeCoord2D
 */
struct TL_DLL TeBox {
    enum TeBoxCorner
    {
        TeLOWERLEFT  = 0,
        TeUPPERLEFT  = 1,
        TeLOWERRIGHT = 2,
        TeUPPERRIGHT = 3
    };

    //! Default constructor
    /*!
        \param x1 X value of the lower left box coordinate
        \param y1 Y value of the lower left box coordinate
        \param x2 X value of the upper right box coordinate
        \param y2 Y value of the upper right box coordinate
    */
    TeBox(const double& x1 = TeMAXFLOAT, const double y1 = TeMAXFLOAT, const double& x2 = -TeMAXFLOAT,
          const double y2 = -TeMAXFLOAT) :
        x1_(x1),
        y1_(y1),
        x2_(x2),
        y2_(y2) {}

    //! Constructor from corners as TeCoord2D objects
    /*!
        \param p1 lower left box coordinate
        \param p2 upper right box coordinate
    */
    TeBox(const TeCoord2D& p1, const TeCoord2D& p2) : x1_(p1.x()), y1_(p1.y()), x2_(p2.x()), y2_(p2.y()) {}

    //! Copy constructor
    TeBox(const TeBox& rhs) {
        x1_ = rhs.x1_;
        x2_ = rhs.x2_;
        y1_ = rhs.y1_;
        y2_ = rhs.y2_;
    }

    //! Operator =
    TeBox& operator=(const TeBox& rhs) {
        if (this == &rhs)
            return *this;
        else {
            x1_ = rhs.x1_;
            x2_ = rhs.x2_;
            y1_ = rhs.y1_;
            y2_ = rhs.y2_;
        }
        return *this;
    }

    //!	Returns TRUE if current TeBox is equal to box; otherwise returns FALSE
    bool operator==(const TeBox& box) const {
        return ((fabs(y1_ - box.y1_) < TePrecision::instance().precision()) &&
                (fabs(x1_ - box.x1_) < TePrecision::instance().precision()) &&
                (fabs(y2_ - box.y2_) < TePrecision::instance().precision()) &&
                (fabs(x2_ - box.x2_) < TePrecision::instance().precision()));
    }

    //! Returns FALSE if current TeBox is equal to box; otherwise returns TRUE
    bool operator!=(const TeBox& box) const { return (!operator==(box)); }

    //! Returns the coordinate corresponding to the geometric center of the  box
    TeCoord2D center() const {
        TeCoord2D p(((x1_ + x2_) / 2.), ((y1_ + y2_) / 2.));
        return p;
    }

    //!	Returns TRUE if current box is valid; otherwise returns FALSE
    bool isValid() const {
        if (x1_ == TeMAXFLOAT || y1_ == TeMAXFLOAT || x2_ == -TeMAXFLOAT || y2_ == -TeMAXFLOAT || x1_ > x2_ ||
            y1_ > y2_)
            return false;
        if (TeISNAN(x1_) || TeISNAN(y1_) || TeISNAN(x2_) || TeISNAN(y2_))
            return false;
        return true;
    }

    //! Returns the x component of the lower left corner
    const double& x1() const { return x1_; }

    //! Returns the y component of the lower left corner
    const double& y1() const { return y1_; }

    //! Returns the x component of the upper right corner
    const double& x2() const { return x2_; }

    //! Returns the y component of the upper right corner
    const double& y2() const { return y2_; }

    //! Returns the box width
    double width() const { return x2_ - x1_; }

    //! Returns the box height
    double height() const { return y2_ - y1_; }

    //! Returns the lower left coordinate of the box
    TeCoord2D lowerLeft() const { return TeCoord2D(x1_, y1_); }

    //! Returns the upper right coordinate of the box
    TeCoord2D upperRight() const { return TeCoord2D(x2_, y2_); }

    //! Destructor
    ~TeBox() {}

    double x1_,  //!< x coordinate of the lower left corner
        y1_,     //!< y coordinate of the lower left corner
        x2_,     //!< x coordinate of the upper right corner
        y2_;     //!< y coordinate of the upper right corner
};

// ALGORITHMS THAT USE A BOX

/*! \fn void updateBox void updateBox ( TeBox& box, const TeCoord2D& pt )
   \brief update a box to include a coordinate
 */
TL_DLL void updateBox(TeBox& box, const TeCoord2D& pt);

/*! \fn void updateBox void updateBox ( TeBox& box, const TeBox& other )
   \brief update a box to include another box
 */
TL_DLL void updateBox(TeBox& box, const TeBox& other);

/*! \fn void updateBox ( TeBox& box, const TeGeometry& geo )
   \brief update a box to include a geometry
 */
TL_DLL void updateBox(TeBox& box, const TeGeometry& geo);


// ZOOMING FUNCTIONS

/*! \fn void zoomIn void zoomIn ( TeBox& box, double t = .8)
   \brief increases the box by a factor t
 */
TL_DLL void zoomIn(TeBox& box, double t = .8);

/*! \fn  void zoomOut void zoomOut ( TeBox& box, double t = .8)
   \brief decreases box by a factor of t
 */
TL_DLL void zoomOut(TeBox& box, double t = .8);

/*! \fn  TeBox makeBox(double x1, double y1, double x2, double y2, const double& tol = 0.0)
   \brief builds a box
 */
TL_DLL TeBox makeBox(double x1, double y1, double x2, double y2, const double& tol = 0.0);

/*! \fn  TeBox adjustToCut(TeBox& box, double bWidth, double bHeight)
   \brief finds the correspondent smallest box that allows a box to be cut in blocks of a given size
 */
TL_DLL TeBox adjustToCut(TeBox& box, double bWidth, double bHeight);

/*! \fn TePolygon polygonFromBox(TeBox& bb)
   \brief builds a TePolygon geometry from a box
 */
TL_DLL TePolygon polygonFromBox(TeBox& bb);

/*! \fn  TeBox adjust()
\brief Expands the box when has not width or height. Returns the precision level needed.
*/
TL_DLL int adjustBox(TeBox& bb);

#endif
