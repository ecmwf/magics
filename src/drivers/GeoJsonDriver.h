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
    \file GeoJsonDriver.h
    \brief Definition of GeoJsonDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 09 18:41:52 2015
*/

#ifndef _GeoJsonDriver_H
#define _GeoJsonDriver_H

#include <BaseDriver.h>
#include <GeoJsonDriverAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class GeoJsonDriver
    \brief This driver produces output for GeoJson
    \ingroup drivers

    The driver produces Keyhole XML (GeoJson) output for usage with 
    Google Earth (http://earth.google.com/) and Google Maps (http://maps.google.com/).
*/
class GeoJsonDriver: public BaseDriver, public GeoJsonDriverAttributes
{

public:
	GeoJsonDriver();
	~GeoJsonDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(),"GeoJson"))
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("GeoJson");
			GeoJsonDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		GeoJsonDriverAttributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void redisplay(const StaticLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const NoDataLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const StepLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const SceneLayer&) const;
	MAGICS_NO_EXPORT void newLayer() const;
	MAGICS_NO_EXPORT void closeLayer() const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT void writeColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const {}
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const {}
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int, bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol& symbol) const;
	MAGICS_NO_EXPORT void renderWindArrow(const Arrow &arrow) const;
	MAGICS_NO_EXPORT void renderWindFlag(const Flag &flag) const;

	//! Method to print string about this class on to a stream of type ostream (virtual).
	MAGICS_NO_EXPORT void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	/* K M L specific */
	MAGICS_NO_EXPORT void redisplay(const LegendLayout& legend) const;
	MAGICS_NO_EXPORT void closePlacemark() const;
	mutable ofstream	pFile_;    //!< Output stream to file.
	mutable Layout*		layout_;
	mutable string		currentLayerPath_;
	mutable string		currentTimeBegin_;
	mutable string		currentTimeEnd_;
	mutable string		currentTimeStamp_;
	mutable bool		GeoJson_placemark_;
	mutable bool		polyline_begin_;
	mutable bool		polygon_begin_;
	mutable bool		MultiGeometrySet_;
	mutable bool		layer_;
	mutable bool		render_;
	mutable bool		ecmwf_logo_;

	mutable stringarray GeoJson_output_resource_list_;

	//! Copy constructor - No copy allowed
	GeoJsonDriver(const GeoJsonDriver&);
	//! Overloaded << operator to copy - No copy allowed
	GeoJsonDriver& operator=(const GeoJsonDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GeoJsonDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
