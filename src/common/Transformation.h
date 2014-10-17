/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \defgroup projections Geographical projections

\section supportedProjection Supported projections

Currently Magics++ supports following projections:

 - Cylindrical
 - Polar-stereographic
 - Satellite

 You can select the projection by setting the parameter SUBPAGE_MAP_PROJECTION .

\section addProjection How to add a new projection

 - add your <i>NewProjection.xml</i> in <i>src/xml</i>

 - add this file in <i>src/xml/Makefile.am</i>

 - add <i>NewProjection.cc/.h</i> in <i>src/drivers/common/</i>,
   inheriting from Transformation, NewProjectionParameters and a relevant Terralib class

 - add the new files in <i>src/common/Makefile.am</i>
 
 - add new projection in src/common/SubPageAttributes.cc:\n
    static SimpleObjectMaker<NewProjection, Transformation> new_NewProjection("new_name");

 - add new projection in src/common/ViewAttributes.cc:\n
    static SimpleObjectMaker<NewProjection, Transformation> new_NewProjection("new_name");
    
 - Last but not least: add the new projection to the documentation!
*/

/*! \file Transformation.h

 Magics Team - ECMWF 2004
*/

#ifndef Transformation_H
#define Transformation_H

#include "magics.h"

#include "PaperPoint.h"
#include "UserPoint.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "TeProjection.h"
#include "BasicGraphicsObject.h"
#include "SceneVisitor.h"
#include "CustomisedPoint.h"




namespace magics {

class Layout;
class GridPlotting;
class LabelPlotting;
class BasicSceneObject;
class MetaDataCollector;
class AbstractMatrix;
class MatrixHandler;
class PointsHandler;
class XmlNode;


class Polyline;

class ViewFilter
{
public:
	ViewFilter() {}
	ViewFilter(double xmin, double xmax, double ymin, double ymax, double xres, double yres);
	~ViewFilter() {}
	bool in(const PaperPoint& xy);

    vector<bool> done;
    double xmin_;
    double xmax_;
    double ymin_;
    double ymax_;
    double xres_;
    double yres_;
    int xdim_;
    int ydim_;
};


class Transformation
{
public:
	Transformation();
	virtual ~Transformation();
	virtual void init();
	void cleaninit();
	enum CoordinateType {GeoType,XyType};
	CoordinateType coordinateType() const {return coordinateType_;}	
	virtual void toxml(ostream&) const {}
	// Xml Methods ! 
	virtual void set(const map<string, string>& ) {}
	virtual void set(const XmlNode& ) {}
	virtual bool accept(const string& ) { return false; }
	void toxml(ostream&, int)  const {}
	virtual Transformation* clone() const {
		Transformation* object = new Transformation();
		return object;
	}
	
	virtual Polyline& getUserBoundingBox() const  { assert(false); }
	virtual Polyline& getPCBoundingBox()   const  { assert(false); }
	
	bool needTopAxis() const   { return topAxis_; }
	void needTopAxis(bool top) { topAxis_ = top; }

	virtual void operator()(const Polyline& poly,  BasicGraphicsObjectContainer& out) const;

	string writeLongitude(const UserPoint&) const;
	string writeLatitude(const UserPoint&) const;

	virtual bool wrapAround(const Polyline&) const { return false; }
	virtual void coastSetting(map<string, string>&, double, double) const { assert(false); }
	virtual bool verifyDef(const string&) const { return false; }   



	virtual double x(double x) const { return x; }
	virtual double y(double y) const { return y; }
	virtual double rx(double x) const { return x; }
	virtual double ry(double y) const { return y; }

	/*
	// is the polyline out the projection!
	virtual bool out(const Polyline&) const;
	// is the polyline in the projection?
	virtual bool in(const Polyline&) const;
	*/
    // is the point in projected area?
	virtual bool in(const UserPoint&) const;
	virtual bool in(const PaperPoint&) const;


	// is the point in PC in the projected area?
	bool in(double x, double y) const;

    bool inX(double x) const
       { return ( getAbsoluteMinX() <= x && x <=  getAbsoluteMaxX()); }
    bool inY(double y) const
        { return ( getAbsoluteMinY() <= y && y <= getAbsoluteMaxY()); }

	// Needed for Image processing!
	virtual TeProjection&  getProjection() 
		{ static TeDatum datum; static TeLatLong dummy(datum); return dummy; }
	
	
	
	virtual void gridLongitudes(const GridPlotting&) const {}
	virtual void gridLatitudes(const GridPlotting&) const  {}

	virtual void labels(const LabelPlotting&, DrawingVisitor&) const {}
	virtual void labels(const LabelPlotting&, LeftAxisVisitor&) const {}
	virtual void labels(const LabelPlotting&, RightAxisVisitor&) const {}
	virtual void labels(const LabelPlotting&, TopAxisVisitor&) const {}
	virtual void labels(const LabelPlotting&, BottomAxisVisitor&) const {}

	
	
	virtual void aspectRatio(double&, double&);
	virtual void fill(double&, double&); // fill the space , can adapt the coordiantes to return the biggest area..
	virtual void tile(double&, double&); // fill the space , can adapt the coordiantes to return the biggest area..
    virtual void forceNewArea(double, double, double, double, double&, double&);
	virtual UserPoint reference() const;
	
// Basic reprojection method! 
	virtual PaperPoint operator()(const UserPoint& xy) const 
		{ return PaperPoint(xy.x(), xy.y()); }


	virtual void fast_reproject(double& x, double& y) const
			{ }

	virtual double patchDistance(double) const { assert(false); }

	virtual PaperPoint operator()(const PaperPoint& xy) const 
		{ return xy; }
	virtual void operator()(const UserPoint& geo, Polyline& out) const;
	virtual void operator()(const UserPoint& xy, vector<PaperPoint>& out) const;

	virtual void revert(const vector< std::pair<double, double> > &, vector< std::pair<double, double> > &) const;
	
	virtual void revert(const PaperPoint& xy, UserPoint& point) const 
		{ point = UserPoint(xy.x(), xy.y()); }		


// Does the projection needs the coastalines to be shifted!
	virtual bool needShiftedCoastlines() const { return false; }

// Set the bounding box in user coordinates 
	virtual void boundingBox(double&, double&, double&, double&) const;
	virtual void smallestBoundingBox(double& x1, double& y1, double& x2, double& y2) const
	{
		boundingBox(x1, y1, x2, y2);
	}
	

	virtual void setDataMinMaxX(double minx, double maxx) const
		{ dataMinX_ = std::min(minx, dataMinX_);  dataMaxX_ = std::max(maxx, dataMaxX_);}
	virtual void setDataMinMaxY(double miny, double maxy) const
		{ dataMinY_ = std::min(miny, dataMinY_);  dataMaxY_ = std::max(maxy, dataMaxY_);}

	
	virtual void setAutomaticX(bool) {}
	virtual void setAutomaticY(bool) {}
	virtual bool getAutomaticX() const { return false; } 
	virtual bool getAutomaticY() const { return false; }
	
    virtual void setDataMinMaxX(double minx,double maxx, const string& ref) const;
	virtual void setDataMinMaxY(double miny, double maxy, const string& ref) const;

	
	virtual vector<double> getDataVectorMinX() const  { return vector<double>(); }
	virtual vector<double> getDataVectorMaxX() const  { return vector<double>(); }
	virtual vector<double> getDataVectorMinY() const  { return vector<double>(); }
	virtual vector<double> getDataVectorMaxY() const  { return vector<double>(); }
	
	
	
    virtual const string& getReferenceX() const  { return referenceX_; }
	virtual const string& getReferenceY() const  { return referenceY_; }
	void setReferenceX(const string& ref) const  { referenceX_ = ref; }
	void setReferenceY(const string& ref) const  { referenceY_ = ref; }
	
	
	
	virtual void adjustXAxis(Layout&) const {}
	virtual void adjustYAxis(Layout&) const {}

	virtual double getWidth() { return -1; }
	virtual double getHeight() { return -1; }
	
	virtual double x(const string& val) const { return tonumber(val); }
	virtual double y(const string& val) const { return tonumber(val); }


	virtual double getMinX() const { return -1; }
	virtual double getMaxX() const { return -1; }
	virtual double getMinY() const { return -1; }
	virtual double getMaxY() const { return -1; }
	
	virtual void setMinMaxX(double, double) {}
	virtual void setMinMaxY(double, double) {}


	virtual double getMinPCX() const { return -1; }
	virtual double getMaxPCX() const { return -1; }
	virtual double getMinPCY() const { return -1; }
	virtual double getMaxPCY() const { return -1; }
	virtual double dimension(BasicGraphicsObjectContainer& parent) const 
	{ return parent.absoluteWidth(); }
	
	double getAbsoluteMinPCX() const { return std::min(getMinPCX(), getMaxPCX()); }
	double getAbsoluteMaxPCX() const { return std::max(getMinPCX(), getMaxPCX()); }
	double getAbsoluteMinPCY() const  { return std::min(getMinPCY(), getMaxPCY()); }
	double getAbsoluteMaxPCY() const  { return std::max(getMinPCY(), getMaxPCY()); }
	
	double getAbsoluteMinX() const { return std::min(getMinX(), getMaxX()); }
	double getAbsoluteMaxX() const { return std::max(getMinX(), getMaxX()); }
	double getAbsoluteMinY() const  { return std::min(getMinY(), getMaxY()); }
	double getAbsoluteMaxY() const  { return std::max(getMinY(), getMaxY()); }
	
	virtual void setNewPCBox(double, double, double, double) {  }
	virtual double unitToCm(double, double) const;
	
	virtual double height() const { return getMaxX() - getMinX(); }
	

	virtual void thin(double, PaperPoint&, vector<pair<double, double> >&) const;
	virtual void getNewDefinition(const UserPoint&, const UserPoint&, string&) const
		{ assert(false); }
	virtual void setDefinition(const string&)
		{ assert(false); }

   virtual void thin(MatrixHandler&, double x, double y, vector<UserPoint>&) const;
   
   virtual void reprojectComponents(double&, double&, pair<double, double>&) const;
   virtual void reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const;
 
   void thin(PointsHandler& points, vector<PaperPoint>& out, vector<PaperPoint>&) const;
   void thin(MatrixHandler& points, vector<PaperPoint>& out, vector<PaperPoint>&) const;

   // The view is set in Projection coordinates!
  virtual void filterView(double xmin, double xmax, double ymin, double ymax, double x, double y)  const {
	  	view_ = ViewFilter(xmin, xmax, ymin, ymax, x, y);
  }
  virtual  void  collect(MetaDataCollector&) const {}
   virtual  void  visit(MetaDataVisitor&, double, double, double, double, double, double);
   virtual string xAxisType() const { return "regular"; }
   virtual string yAxisType() const { return "regular"; }

   virtual MatrixHandler* prepareData(const AbstractMatrix& matrix) const;
   virtual void wraparound(const UserPoint&, stack<UserPoint>&) const;
   virtual void populate(double lon, double lat, double val, vector<UserPoint>& out) const {
	   if (in(lon, lat) )
		   out.push_back(UserPoint(lon, lat, val));
   }
   double distance(UserPoint&, UserPoint&) const;
protected:
	virtual void print(ostream&) const;    
    
    CoordinateType coordinateType_;	
    
    mutable double areaMinX_;
    mutable double areaMaxX_;
    mutable double areaMinY_;
    mutable double areaMaxY_;
    
    mutable double  dataMinX_;
    mutable double  dataMaxX_;
    mutable double  dataMinY_;
    mutable double  dataMaxY_;
    
    mutable string dataReferenceX_;
    mutable string dataReferenceY_;
    mutable string referenceX_;
    mutable string referenceY_;
    
    mutable ViewFilter view_;

    bool topAxis_;

    mutable Polyline* userEnveloppe_;
    mutable Polyline* PCEnveloppe_;

    // For tiling Mode keep the position in pixel of the asked topleft corner!
    int xTile_;
    int yTile_;
    double askedxmin_;
    double askedxmax_;
    double askedymin_;
    double askedymax_;
    double askedWidth_;
    double askedHeight_;
    double originX_;
    double originY_;
    int tile_;
    int zoomLevel_;
    double unit_;
    double unitEpsilon_;
private:
	// No copy allowed
	Transformation(const Transformation&);
	Transformation& operator=(const Transformation&);

// -- Friends
	friend ostream& operator<<(ostream& s,const Transformation& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, Transformation>
{ 
public:
	Transformation* operator()(const string& val )
	{
		 return SimpleObjectMaker<Transformation>::create(val);
	}     
	Transformation* magics(const string& param)
	{
		Transformation* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif

