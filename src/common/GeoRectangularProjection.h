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

#include <Transformation.h>
#include <GeoRectangularProjectionAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class GeoRectangularProjection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class GeoRectangularProjection: public Transformation, public GeoRectangularProjectionAttributes
{

public:
	GeoRectangularProjection();
	~GeoRectangularProjection();

	/*!
	  \brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{
        Transformation::set(node);
        GeoRectangularProjectionAttributes::set(node);
        init();
	}
	bool verifyDef(const string&) const;
   /*!
	  \brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{
        Transformation::set(map);
        GeoRectangularProjectionAttributes::set(map);
        init();
	}
    
    double ratio() const;
    virtual Transformation* clone() const {
		GeoRectangularProjection* transformation = new GeoRectangularProjection();
        transformation->copy(*this);
		return transformation;
	}
    void setNewPCBox(double, double, double, double);
    
    void coastSetting(map<string, string>&, double, double) const;

    Polyline& getPCBoundingBox() const;
    Polyline& getUserBoundingBox() const;
    void populate(double lon, double lat, double value, vector<UserPoint>& out) const;

    double patchDistance(double) const;
	/*!
	\\brief 
	*/
	virtual PaperPoint operator()(const UserPoint&) const;
	/*!
	\\brief 
	*/
	virtual PaperPoint operator()(const PaperPoint&) const;
	/*!
	\\brief 
	*/
	virtual void revert(const PaperPoint&, UserPoint&) const;

	void revert(const vector< std::pair<double, double> > &, vector< std::pair<double, double> > &) const;
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
	\\brief set the xmin in user coordinates!
	*/
	virtual void setMinX(double) ;
	/*!
	\\brief return the ymin in user coordinates!
	*/
	virtual void setMinY(double) ;
	/*!
	\\brief return the xmax in user coordinates!
	*/
	virtual void setMaxX(double) ;
	/*!
	\\brief return the ymax in user coordinates!
	*/
	virtual void setMaxY(double) ;
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

	MatrixHandler* prepareData(const AbstractMatrix& matrix) const;
	void wraparound(const UserPoint&, stack<UserPoint>&) const;
	void getNewDefinition(const UserPoint&, const UserPoint&, string&) const;
	void setDefinition(const string&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 virtual void init(); 
	 
	 mutable TeProjection* projection_;
	 double 	       xpcmin_;
	 double                ypcmin_;
	 double                xpcmax_;
	 double                ypcmax_;

private:
    //! Copy constructor - No copy allowed
	GeoRectangularProjection(const GeoRectangularProjection&);
    //! Overloaded << operator to copy - No copy allowed
	GeoRectangularProjection& operator=(const GeoRectangularProjection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GeoRectangularProjection& p)
		{ p.print(s); return s; }

};

class MercatorProjection : public GeoRectangularProjection
{
public:
	MercatorProjection();
	~MercatorProjection();
	bool fast_reproject(double& x, double& y) const;
	double patchDistance(double) const;

protected :
	void print(ostream&) const; 
	void init();
};
    

} // namespace magics
#endif
