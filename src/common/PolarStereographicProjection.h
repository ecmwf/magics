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
    \file PolarStereographicProjection.h
    \brief Definition of PolarStereographicProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Jan 11 15:01:45 2008
*/

#ifndef _PolarStereographicProjection_H
#define _PolarStereographicProjection_H

#include "PolarStereographicProjectionAttributes.h"
#include "Transformation.h"
#include "XmlNode.h"

namespace magics {

/*! \class PolarStereographicProjection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class PolarStereographicProjection : public Transformation, public PolarStereographicProjectionAttributes {
public:
    PolarStereographicProjection();
    ~PolarStereographicProjection() override;

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node) override {
        Transformation::set(node);
        PolarStereographicProjectionAttributes::set(node);
    }
    /*!
      \brief sets  from a map
    */
    void set(const map<string, string>& map) override {
        Transformation::set(map);
        PolarStereographicProjectionAttributes::set(map);
    }

    virtual Transformation* clone() const override {
        PolarStereographicProjection* transformation = new PolarStereographicProjection();
        transformation->copy(*this);
        return transformation;
    }
    void fill(double&, double&) override;
    void tile(double&, double&) override;

    void setNewPCBox(double, double, double, double) override;

    double patchDistance(double) const override;
    bool fast_reproject(double& x, double& y) const override;
    /*!
    \\brief Initialise the projection
    */
    virtual void init(double, double);

    /*!
    \\brief
    */
    virtual PaperPoint operator()(const UserPoint&) const override;
    virtual void operator()(const UserPoint&, vector<PaperPoint>& out) const override;
    virtual double unitToCm(double, double) const override;
    virtual double height() const override;
    /*!
    \\brief
    */
    virtual PaperPoint operator()(const PaperPoint&) const override;
    void revert(const vector<std::pair<double, double> >&, vector<std::pair<double, double> >&) const override;
    /*!
    \\brief
    */
    virtual void revert(const PaperPoint&, UserPoint&) const override;
    /*!
    \\brief Does the projection needs the coastalines to be shifted!
    */
    virtual bool needShiftedCoastlines() const override;

    // virtual bool concatenate(vector<Polyline* >& lines, Polyline* poly) const override;
    Polyline& getPCBoundingBox() const override;
    Polyline& getUserBoundingBox() const override;
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

    virtual void thin(MatrixHandler&, double x, double y, vector<UserPoint>&) const override;
    /*!
            \\brief prepare the javascript for thes particular instance of projection
    */
    virtual void visit(MetaDataVisitor&, double, double, double, double, double, double) override;

    // Needed for Image processing!
    virtual TeProjection& getProjection() override { return *projection_; }
    double dimension(BasicGraphicsObjectContainer& parent) const override { return parent.absoluteHeight(); }
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const override;
    void setDefinition(const string&) override;
    UserPoint reference() const override;
    virtual void geoProjection(int& geo) const override { geo = 1; }  // Useful for Streamlines !
protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    void corners();
    void centre(double, double);
    void verticalLabels(const LabelPlotting&, double, double, bool) const;
    void horizontalLabels(const LabelPlotting&, double, double, bool) const;

    mutable TeProjection* projection_;

    double xmin_;
    double ymin_;
    double xmax_;
    double ymax_;

    double xpcmin_;
    double ypcmin_;
    double xpcmax_;
    double ypcmax_;
    void reprojectComponents(double& x, double& y, pair<double, double>&) const override;
    void reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const override;

    void coastSetting(map<string, string>&, double width, double height) const override;

    MatrixHandler* prepareData(const AbstractMatrix& matrix) const override;
    void wraparound(const UserPoint&, stack<UserPoint>&) const override;


private:
    //! Copy constructor - No copy allowed
    PolarStereographicProjection(const PolarStereographicProjection&);
    //! Overloaded << operator to copy - No copy allowed
    PolarStereographicProjection& operator=(const PolarStereographicProjection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const PolarStereographicProjection& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics
#endif
