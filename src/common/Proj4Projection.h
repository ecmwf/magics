/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*!
    \file Proj4Projection.h
    \brief Definition of Proj4Projection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue May 18 17:39:58 2010
*/

#ifndef _Proj4Projection_H
#define _Proj4Projection_H

#include "Proj4ProjectionAttributes.h"
#include "Transformation.h"
#include "XmlNode.h"


#include "ProjP.h"

namespace magics {

class MetaDataVisistor;
class Epsg;
/*! \class Proj4Projection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class Proj4Projection : public Transformation, public Proj4ProjectionAttributes {
public:
    Proj4Projection();
    Proj4Projection(const string& definition);
    ~Proj4Projection() override;

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node) override {
        Transformation::set(node);
        Proj4ProjectionAttributes::set(node);
        init();
    }
    /*!
       \brief sets  from a map
     */
    void set(const map<string, string>& map) override {
        Transformation::set(map);
        Proj4ProjectionAttributes::set(map);
        init();
    }

    virtual Transformation* clone() const override {
        Proj4Projection* transformation = new Proj4Projection(definition_);
        transformation->copy(*this);
        return transformation;
    }

    bool addSouth() const override;
    void populate(double lon, double lat, double value, vector<UserPoint>& out) const override;

    /*!
    \\brief Initialise the projection
    */
    virtual void init() override;
    /*!
    \\brief
    */
    void fill(double&, double&) override;

    virtual PaperPoint operator()(const UserPoint&) const override;
    /*!
    \\brief
    */
    virtual bool fast_reproject(double& x, double& y) const override;

    virtual PaperPoint operator()(const PaperPoint&) const override;
    /*!
    \\brief
    */
    void collect(MetaDataCollector&) const override;
    virtual void revert(const PaperPoint&, UserPoint&) const override;

    void revert(const vector<std::pair<double, double> >& in, vector<std::pair<double, double> >& out) const override;

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
    virtual void setExtend();

    virtual double getExtendedMinPCX() const override;
    virtual double getExtendedMaxPCX() const override;
    virtual double getExtendedMinPCY() const override;
    virtual double getExtendedMaxPCY() const override;

    virtual void setMinMaxX(double, double) override;
    virtual void setMinMaxY(double, double) override;

    /*!
    \\brief create the grid for the longitudes!!
    */
    virtual void gridLongitudes(const GridPlotting&) const override;
    /*!
    \\brief create the grid for the latitudes!!
    */
    virtual void gridLatitudes(const GridPlotting&) const override;
    /*!
    \\brief calculate the labels
    */
    virtual void labels(const LabelPlotting&, DrawingVisitor&) const override;
    /*!
    \\brief calculate the left labels
    */
    virtual void labels(const LabelPlotting&, LeftAxisVisitor&) const override;
    /*!
    \\brief calculate the right labels
    */
    virtual void labels(const LabelPlotting&, RightAxisVisitor&) const override;
    /*!
    \\brief calculate the bottom labels
    */
    virtual void labels(const LabelPlotting&, BottomAxisVisitor&) const override;
    /*!
    \\brief calculate the top labels
    */
    virtual void labels(const LabelPlotting&, TopAxisVisitor&) const override;
    virtual void coastSetting(map<string, string>&, double, double) const override;

    void visit(MetaDataVisitor& visitor, double left, double top, double width, double height, double imgw,
               double imgh) override;
    void setNewPCBox(double minx, double miny, double maxx, double maxy) override;
    void verticalLabels(const LabelPlotting& label, double x, double pos, Justification justif) const;
    void horizontalLabels(const LabelPlotting& label, double y, double pos, VerticalAlign align) const;
    MatrixHandler* prepareData(const AbstractMatrix&) const override;
    Polyline& getPCBoundingBox() const override;
    Polyline& getSimplePCBoundingBox() const override;
    Polyline& getUserBoundingBox() const override;

    void setPCBoundingBox();

    typedef void (Proj4Projection::*InitMethod)();
    map<string, InitMethod> methods_;

    void conic();
    void geos();
    void tpers();
    void simple();
    void projectionSimple();
    void cleaninit() override { init(); }

    void add(double, double);

    double patchDistance(double) const override;
    const string& name() const { return definition_; }
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const override;
    void setDefinition(const string&) override;
    void reprojectComponents(double&, double&, pair<double, double>&) const override;
    void reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const override;
    virtual void geoProjection(int& geo) const override { geo = 1; }  // Useful for Streamlines !

    void wrap(double&, double&);

protected:
    //! Method to print string about this class on to a stream of type ostream
    //! (virtual).
    typedef void (Proj4Projection::*SettingHelper)();
    map<string, SettingHelper> helpers_;

    void full();
    void corners();
    void centre();
    void data();

    virtual void print(ostream&) const override;
    mutable LatLonProjP* helper_;
    double xpcmin_;
    double xpcmax_;
    double ypcmin_;
    double ypcmax_;
    double xgutter_;
    double ygutter_;
    
    mutable Epsg* projection_;
    string definition_;
    mutable double gridMinLon_;
    mutable double gridMinLat_;
    mutable double gridMaxLon_;
    mutable double gridMaxLat_;

    bool wraparound_;
    bool useData_;

    double pwidth_;

    double data_min_longitude_;
    double data_max_longitude_;
    double data_min_latitude_;
    double data_max_latitude_;
   

private:
    //! Copy constructor - No copy allowed
    Proj4Projection(const Proj4Projection&);
    //! Overloaded << operator to copy - No copy allowed
    Proj4Projection& operator=(const Proj4Projection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Proj4Projection& p) {
        p.print(s);
        return s;
    }
};

class Proj4PolarNorth : public Proj4Projection {
public:
    Proj4PolarNorth() : Proj4Projection("polar_north") {}
};

class Proj4PolarSouth : public Proj4Projection {
public:
    Proj4PolarSouth() : Proj4Projection("polar_south") {}
};

class Proj4Lambert : public Proj4Projection {
public:
    Proj4Lambert() : Proj4Projection("lambert") {}
};
class Proj4EPSG3857 : public Proj4Projection {
public:
    Proj4EPSG3857() : Proj4Projection("EPSG:3857") {}
};
class Proj4EPSG900913 : public Proj4Projection {
public:
    Proj4EPSG900913() : Proj4Projection("EPSG:3857") {}
};
class Proj4EPSG3035 : public Proj4Projection {
public:
    Proj4EPSG3035() : Proj4Projection("EPSG:3035") {}
};
class Proj4Geos : public Proj4Projection {
public:
    Proj4Geos() : Proj4Projection("geos") {}
};
class Proj4TPers : public Proj4Projection {
public:
    Proj4TPers() : Proj4Projection("tpers") {}
};
class Proj4Meteosat0 : public Proj4Projection {
public:
    Proj4Meteosat0() : Proj4Projection("meteosat_0") {}
};
class Proj4Meteosat57 : public Proj4Projection {
public:
    Proj4Meteosat57() : Proj4Projection("meteosat_575") {}
};
class Proj4Meteosat145 : public Proj4Projection {
public:
    Proj4Meteosat145() : Proj4Projection("meteosat_145") {}
};
class Proj4Geose : public Proj4Projection {
public:
    Proj4Geose() : Proj4Projection("goes-east") {}
};
class Proj4Geosw : public Proj4Projection {
public:
    Proj4Geosw() : Proj4Projection("goes-w") {}
};
class Proj4Goode : public Proj4Projection {
public:
    Proj4Goode() : Proj4Projection("goode") {}
};
class Proj4Mercator : public Proj4Projection {
public:
    Proj4Mercator() : Proj4Projection("mercator") {}
};
class Proj4Collignon : public Proj4Projection {
public:
    Proj4Collignon() : Proj4Projection("collignon") {}
};

class Proj4Mollweide : public Proj4Projection {
public:
    Proj4Mollweide() : Proj4Projection("mollweide") {}
};
class Proj4Robinson : public Proj4Projection {
public:
    Proj4Robinson() : Proj4Projection("robinson") {}
};
class Proj4Bonne : public Proj4Projection {
public:
    Proj4Bonne() : Proj4Projection("bonne") {}
};
class Proj4Google : public Proj4Projection {
public:
    Proj4Google() : Proj4Projection("google") {}
};
class Proj4Efas : public Proj4Projection {
public:
    Proj4Efas() : Proj4Projection("efas") {}
};

class Proj4LambertNorthAtlantic : public Proj4Projection {
public:
    Proj4LambertNorthAtlantic() : Proj4Projection("lambert_north_atlantic") {}
};
class Proj4EPSG32661 : public Proj4Projection {
public:
    Proj4EPSG32661() : Proj4Projection("EPSG:32661") {}
};
class Proj4EPSG32761 : public Proj4Projection {
public:
    Proj4EPSG32761() : Proj4Projection("EPSG:32761") {}
};
class Proj4EPSG4326 : public Proj4Projection {
public:
    Proj4EPSG4326() : Proj4Projection("EPSG:4326") {}
};

class Proj4Automatic : public Proj4Projection {
public:
    Proj4Automatic();
    void aspectRatio(double&, double&);

    void init();
    
    
    void setNewPCBox(double minx, double miny, double maxx, double maxy);
    virtual void setExtend() override;
    

protected:
    bool init_;

    
   
};

}  // namespace magics
#endif
