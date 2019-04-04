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
    \file Proj4Projection.h
    \brief Definition of Proj4Projection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue May 18 17:39:58 2010
*/

#ifndef _Proj4Projection_H
#define _Proj4Projection_H

#include <Proj4ProjectionAttributes.h>
#include <Transformation.h>
#include <XmlNode.h>
#define ACCEPT_USE_OF_DEPRECATED_PROJ_API_H 1
#include <proj_api.h>

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
    ~Proj4Projection();

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node) {
        Transformation::set(node);
        Proj4ProjectionAttributes::set(node);
        init();
    }
    /*!
       \brief sets  from a map
     */
    void set(const map<string, string>& map) {
        Transformation::set(map);
        Proj4ProjectionAttributes::set(map);
        init();
    }

    virtual Transformation* clone() const {
        Proj4Projection* transformation = new Proj4Projection(definition_);
        transformation->copy(*this);
        return transformation;
    }

    bool addSouth() const;

    /*!
    \\brief Initialise the projection
    */
    virtual void init();
    /*!
    \\brief
    */


    virtual PaperPoint operator()(const UserPoint&) const;
    /*!
    \\brief
    */
    virtual bool fast_reproject(double& x, double& y) const;

    virtual PaperPoint operator()(const PaperPoint&) const;
    /*!
    \\brief
    */
    void collect(MetaDataCollector&) const;
    virtual void revert(const PaperPoint&, UserPoint&) const;

    void revert(const vector<std::pair<double, double> >& in, vector<std::pair<double, double> >& out) const;

    /*!
    \\brief Does the projection needs the coastalines to be shifted!
    */
    virtual bool needShiftedCoastlines() const;
    /*!
    \\brief set the aspect ratio!
    */
    virtual void aspectRatio(double&, double&);
    /*!
    \\brief set the bounding box!
    */
    virtual void boundingBox(double&, double&, double&, double&) const;
    /*!
    \\brief return the xmin in user coordinates!
    */
    virtual double getMinX() const;
    /*!
    \\brief return the ymin in user coordinates!
    */
    virtual double getMinY() const;
    /*!
    \\brief return the xmax in user coordinates!
    */
    virtual double getMaxX() const;
    /*!
    \\brief return the ymax in user coordinates!
    */
    virtual double getMaxY() const;
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
    virtual double getMinPCX() const;
    /*!
    \\brief return the ymin in projection coordinates!
    */
    virtual double getMinPCY() const;
    /*!
    \\brief return the xmax in projection coordinates!
    */
    virtual double getMaxPCX() const;
    /*!
    \\brief return the ymax in projection coordinates!
    */
    virtual double getMaxPCY() const;
    /*!
    \\brief create the grid for the longitudes!!
    */
    virtual void gridLongitudes(const GridPlotting&) const;
    /*!
    \\brief create the grid for the latitudes!!
    */
    virtual void gridLatitudes(const GridPlotting&) const;
    /*!
    \\brief calculate the labels
    */
    virtual void labels(const LabelPlotting&, DrawingVisitor&) const;
    /*!
    \\brief calculate the left labels
    */
    virtual void labels(const LabelPlotting&, LeftAxisVisitor&) const;
    /*!
    \\brief calculate the right labels
    */
    virtual void labels(const LabelPlotting&, RightAxisVisitor&) const;
    /*!
    \\brief calculate the bottom labels
    */
    virtual void labels(const LabelPlotting&, BottomAxisVisitor&) const;
    /*!
    \\brief calculate the top labels
    */
    virtual void labels(const LabelPlotting&, TopAxisVisitor&) const;
    virtual void coastSetting(map<string, string>&, double, double) const;

    void visit(MetaDataVisitor& visitor, double left, double top, double width, double height, double imgw,
               double imgh);
    void setNewPCBox(double minx, double miny, double maxx, double maxy);
    void verticalLabels(const LabelPlotting& label, double x, double pos, Justification justif) const;
    void horizontalLabels(const LabelPlotting& label, double y, double pos, VerticalAlign align) const;
    MatrixHandler* prepareData(const AbstractMatrix&) const;
    Polyline& getPCBoundingBox() const;
    Polyline& getUserBoundingBox() const;

    typedef void (Proj4Projection::*InitMethod)();
    map<string, InitMethod> methods_;

    void conic();
    void geos();
    void simple();
    void projectionSimple();

    void add(double, double);

    double patchDistance(double) const;
    const string& name() const { return definition_; }
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const;
    void setDefinition(const string&);
    void reprojectComponents(double&, double&, pair<double, double>&) const;
    void reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const;
    virtual void geoProjection(int& geo) const { geo = 1; }  // Useful for Streamlines !

    void wrap(double&, double&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    typedef void (Proj4Projection::*SettingHelper)();
    map<string, SettingHelper> helpers_;

    void full();
    void corners();
    void centre();

    virtual void print(ostream&) const;
    mutable projPJ from_;
    mutable projPJ to_;
    double min_pcx_;
    double max_pcx_;
    double min_pcy_;
    double max_pcy_;
    mutable Epsg* projection_;
    string definition_;
    mutable double gridMinLon_;
    mutable double gridMinLat_;
    mutable double gridMaxLon_;
    mutable double gridMaxLat_;

    bool wraparound_;
    double width_;
    double pwidth_;

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
    void setMinMaxX(double, double);
    void setMinMaxY(double, double);
    void setNewPCBox(double minx, double miny, double maxx, double maxy);

protected:
    bool init_;
    double width_;
    double height_;
};

}  // namespace magics
#endif
