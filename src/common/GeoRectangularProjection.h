/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file GeoRectangularProjection.h
    \brief Definition of GeoRectangularProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Jan 11 11:08:24 2008
*/

#ifndef _GeoRectangularProjection_H
#define _GeoRectangularProjection_H

#include "GeoRectangularProjectionAttributes.h"
#include "Transformation.h"
#include "XmlNode.h"

namespace magics {

/*! \class GeoRectangularProjection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class GeoRectangularProjection : public Transformation, public GeoRectangularProjectionAttributes {
public:
    GeoRectangularProjection();
    ~GeoRectangularProjection() override;

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node) override {
        Transformation::set(node);
        GeoRectangularProjectionAttributes::set(node);
        init();
    }
    virtual bool addSouth() const override { return true; }
    bool verifyDef(const string&) const override;
    /*!
       \brief sets  from a map
     */
    void set(const map<string, string>& map) override {
        Transformation::set(map);
        GeoRectangularProjectionAttributes::set(map);
        init();
    }

    double ratio() const override;
    virtual Transformation* clone() const override {
        GeoRectangularProjection* transformation = new GeoRectangularProjection();
        transformation->copy(*this);
        return transformation;
    }
    void setNewPCBox(double, double, double, double) override;

    void coastSetting(map<string, string>&, double, double) const override;

    Polyline& getPCBoundingBox() const override;
    Polyline& getUserBoundingBox() const override;
    void populate(double lon, double lat, double value, vector<UserPoint>& out) const override;

    double patchDistance(double) const override;
    /*!
    \\brief
    */
    virtual PaperPoint operator()(const UserPoint&) const override;
    /*!
    \\brief
    */
    virtual PaperPoint operator()(const PaperPoint&) const override;
    /*!
    \\brief
    */
    virtual void revert(const PaperPoint&, UserPoint&) const override;

    void revert(const vector<std::pair<double, double> >&, vector<std::pair<double, double> >&) const override;
    /*!
    \\brief Does the projection needs the coastalines to be shifted!
    */
    virtual bool needShiftedCoastlines() const override;
    /*!
    \\brief set the aspect ratio!
    */
    virtual void aspectRatio(double&, double&) override;
    /*!
    \\brief set the bounding box!
    */
    virtual void boundingBox(double&, double&, double&, double&) const override;
    virtual void smallestBoundingBox(double&, double&, double&, double&) const override;
    /*!
    \\brief return the xmin in user coordinates!
    */
    virtual double getMinX() const override;
    /*!
    \\brief return the ymin in user coordinates!
    */
    virtual double getMinY() const override;
    /*!
    \\brief return the xmax in user coordinates!
    */
    virtual double getMaxX() const override;
    /*!
    \\brief return the ymax in user coordinates!
    */
    virtual double getMaxY() const override;
    /*!
    \\brief set the xmin in user coordinates!
    */
    virtual void setMinX(double);
    /*!
    \\brief return the ymin in user coordinates!
    */
    virtual void setMinY(double);
    /*!
    \\brief return the xmax in user coordinates!
    */
    virtual void setMaxX(double);
    /*!
    \\brief return the ymax in user coordinates!
    */
    virtual void setMaxY(double);
    /*!
    \\brief return the xmin in projection coordinates!
    */
    virtual double getMinPCX() const override;
    /*!
    \\brief return the ymin in projection coordinates!
    */
    virtual double getMinPCY() const override;
    /*!
    \\brief return the xmax in projection coordinates!
    */
    virtual double getMaxPCX() const override;
    /*!
    \\brief return the ymax in projection coordinates!
    */
    virtual double getMaxPCY() const override;
    /*!
    \\brief create the grid for the longitudes!!
    */
    virtual void gridLongitudes(const GridPlotting&) const override;
    /*!
    \\brief create the grid for the latitudes!!
    */
    virtual void gridLatitudes(const GridPlotting&) const override;
    /*!
    \\brief calculate the top labels
    */
    virtual void labels(const LabelPlotting&, DrawingVisitor&) const override;
    virtual void labels(const LabelPlotting&, LeftAxisVisitor&) const override;
    virtual void labels(const LabelPlotting&, RightAxisVisitor&) const override;
    virtual void labels(const LabelPlotting&, TopAxisVisitor&) const override;
    virtual void labels(const LabelPlotting&, BottomAxisVisitor&) const override;


    virtual double getExtendedMinPCX() const override;
    virtual double getExtendedMaxPCX() const override;
    virtual double getExtendedMinPCY() const override;
    virtual double getExtendedMaxPCY() const override;

    MatrixHandler* prepareData(const AbstractMatrix& matrix) const override;
    void wraparound(const UserPoint&, stack<UserPoint>&) const override;
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const override;
    void setDefinition(const string&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    virtual void init() override;

    mutable TeProjection* projection_;
    double xpcmin_;
    double ypcmin_;
    double xpcmax_;
    double ypcmax_;

    double xgutter_;
    double ygutter_;
    


private:
    //! Copy constructor - No copy allowed
    GeoRectangularProjection(const GeoRectangularProjection&);
    //! Overloaded << operator to copy - No copy allowed
    GeoRectangularProjection& operator=(const GeoRectangularProjection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GeoRectangularProjection& p) {
        p.print(s);
        return s;
    }
};

class MercatorProjection : public GeoRectangularProjection {
public:
    MercatorProjection();
    ~MercatorProjection() override;
    bool fast_reproject(double& x, double& y) const override;
    double patchDistance(double) const override;

protected:
    void print(ostream&) const override;
    void init() override;
};


}  // namespace magics
#endif
