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

/*!
    \file PolarStereographicProjection.h
    \brief Definition of PolarStereographicProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Jan 11 15:01:45 2008
*/

#ifndef _PolarStereographicProjection_H
#define _PolarStereographicProjection_H

#include <Transformation.h>
#include <PolarStereographicProjectionAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class PolarStereographicProjection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class PolarStereographicProjection: public Transformation, public PolarStereographicProjectionAttributes
{
public:
	PolarStereographicProjection();
	~PolarStereographicProjection();

	/*!
	  \brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{
		Transformation::set(node);
		PolarStereographicProjectionAttributes::set(node);
	}
	/*!
	  \brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{
		Transformation::set(map);
		PolarStereographicProjectionAttributes::set(map);
	}
    
	virtual Transformation* clone() const {
		PolarStereographicProjection* transformation = new PolarStereographicProjection();
		transformation->copy(*this);
		return transformation;
	}
	void fill(double&, double&) ;
	void tile(double&, double&) ;

	void setNewPCBox(double, double, double, double);

	 double patchDistance(double) const;
	 void fast_reproject(double& x, double& y) const;
	/*!
	\\brief Initialise the projection
	*/
	virtual void init(double, double) ;

	/*!
	\\brief 
	*/
	virtual PaperPoint operator()(const UserPoint&) const;
	virtual void operator()(const UserPoint&, vector<PaperPoint>& out) const;
	virtual double unitToCm(double, double) const;
	virtual double height() const;
	/*!
	\\brief 
	*/
	virtual PaperPoint operator()(const PaperPoint&) const;
	void revert(const vector< std::pair<double, double> > &, vector< std::pair<double, double> > &) const;
	/*!
	\\brief 
	*/
	virtual void revert(const PaperPoint&, UserPoint&) const;
	/*!
	\\brief Does the projection needs the coastalines to be shifted!
	*/
	virtual bool needShiftedCoastlines() const;

	//virtual bool concatenate(vector<Polyline* >& lines, Polyline* poly) const;
	Polyline& getPCBoundingBox() const;
	Polyline& getUserBoundingBox() const;
	/*!
	\\brief set the aspect ratio!
	*/
	virtual void aspectRatio(double&, double&) ;
	/*!
	\\brief set the bounding box!
	*/
	virtual void boundingBox(double&, double&, double&, double&) const;
	virtual void smallestBoundingBox(double&, double&, double&, double&) const;
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
	\\brief calculate the top labels
	*/
	virtual void labels(const LabelPlotting&, DrawingVisitor&) const;
	virtual void labels(const LabelPlotting&, LeftAxisVisitor&) const;
	virtual void labels(const LabelPlotting&, RightAxisVisitor&) const;
	virtual void labels(const LabelPlotting&, TopAxisVisitor&) const;
	virtual void labels(const LabelPlotting&, BottomAxisVisitor&) const;
	
	virtual void thin(MatrixHandler&, double x, double y, vector<UserPoint>&) const;
	/*!
	        \\brief prepare the javascript for thes particular instance of projection
	*/
	virtual void  visit(MetaDataVisitor&,double, double, double, double, double, double) ;
	
	// Needed for Image processing!
	virtual TeProjection&  getProjection() 
			{ return *projection_ ; }
	double dimension(BasicGraphicsObjectContainer& parent) const
        { return parent.absoluteHeight(); }
	void getNewDefinition(const UserPoint&, const UserPoint&, string&) const;
	void setDefinition(const string&);
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	 
	void corners();
	void centre(double, double);
	void verticalLabels(const LabelPlotting&, double, double, bool)  const;
	void horizontalLabels(const LabelPlotting&, double, double, bool)  const;

	mutable TeProjection* projection_;
	 
	double xmin_;
	double ymin_;
	double xmax_;
	double ymax_;

	double xpcmin_;
	double ypcmin_;
	double xpcmax_;
	double ypcmax_;
	void reprojectComponents(double& x, double& y, pair<double, double>&) const;
	void reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const;

	void coastSetting(map<string, string>&, double width, double height) const;

	MatrixHandler* prepareData(const AbstractMatrix& matrix) const;
	void wraparound(const UserPoint&, stack<UserPoint>&) const;


private:
	//! Copy constructor - No copy allowed
	PolarStereographicProjection(const PolarStereographicProjection&);
	//! Overloaded << operator to copy - No copy allowed
	PolarStereographicProjection& operator=(const PolarStereographicProjection&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PolarStereographicProjection& p)
		{ p.print(s); return s; }
};
    

} // namespace magics
#endif
