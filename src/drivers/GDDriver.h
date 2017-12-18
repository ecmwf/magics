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
    \file GDDriver.h
    \brief Definition of GDDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Oct 29 16:05:47 2007
*/

#ifndef MPP_GDDriver_H
#define MPP_GDDriver_H

#include <BaseDriver.h>
#include <GDDriverAttributes.h>
#include <XmlNode.h>

#include <gd.h>

namespace magics
{

/*! \class GDDriver
    \brief This driver produces output in various raster image formats.
    \ingroup drivers

    The raster image format driver produces output in various raster image formats
    thanks to GD ( http://www.libgd.org ).
*/
class GDDriver: public BaseDriver, public GDDriverAttributes
{
public:
	GDDriver();
	~GDDriver();
	void open();
	void close();

	void setJPG(bool b = true) const {jpg_ = b;}
	void setGIF(bool b = true) const {gif_ = b;}
	void setPNG(bool b = true) const {png_ = b;}
	void setAnimation(bool b = true) const {animated_ = b; if(b) gif_ = true;}

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "gif") ||
		     magCompare(node.name(), "gif_animation") ||
		     magCompare(node.name(), "gd_png") ||
		     magCompare(node.name(), "png") ||
		     magCompare(node.name(), "jpeg") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("gd");
			GDDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		GDDriverAttributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;

	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int, bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;

	MAGICS_NO_EXPORT MFloat setY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT void renderMagLogo(MFloat x, MFloat y) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol& symbol) const;
	MAGICS_NO_EXPORT MFloat projectX(const MFloat x) const {return offsetX_+(x*coordRatioX_);}
	MAGICS_NO_EXPORT MFloat projectY(const MFloat y) const {return offsetY_+(y*coordRatioY_);}

	mutable ofstream	xmlFile_;
	mutable FILE*		outFile_;
	mutable string	currentFont_;
	mutable int		currentFontSize_;
	mutable int		currentColourIndex_;
	mutable int		currentLineStyleIndex_;
	mutable int		currentPageCount_;
	mutable int		lastPage_;    //!< for layers
	mutable MFloat		scaleFactor_; //!< needed because GD uses different sizes as CM
	mutable Layout*		currentLayout_;

	mutable vector<gdImagePtr> Images_;
	mutable gdImagePtr	currentImage_;
	mutable gdImagePtr	Brush_;
	mutable bool		animated_;
	mutable bool		jpg_;
	mutable bool		gif_;
	mutable bool		png_;
	mutable bool		clipping_;

	mutable int dimensionXglobal_;
	mutable int dimensionYglobal_;
	mutable MFloat offsetX_;
	mutable MFloat offsetY_;
	mutable stack<MFloat> offsetsX_;
	mutable stack<MFloat> offsetsY_;
	mutable stack<MFloat> boxoffsetsX_;
	mutable stack<MFloat> boxoffsetsY_;


	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	//! Copy constructor - No copy allowed
	GDDriver(const GDDriver&);
	//! Overloaded << operator to copy - No copy allowed
	GDDriver& operator=(const GDDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GDDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
