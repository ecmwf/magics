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
    \file CairoDriver.h
    \brief Definition of CairoDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Oct 15 20:49:32 2007
*/

#ifndef _CairoDriver_H
#define _CairoDriver_H

#include <BaseDriver.h>
#include <CairoDriverAttributes.h>
#include <XmlNode.h>

#ifdef _AIX
#undef HAVE_STDLIB_H
#endif
#include <cairo.h>

namespace magics
{

/*! \class CairoDriver
    \brief This driver produces output for Cairo
    \ingroup drivers

    This driver ...
*/
class CairoDriver: public BaseDriver, public CairoDriverAttributes
{

public:
	CairoDriver();
	~CairoDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "png") ||
		     magCompare(node.name(), "pdf") ||
		     magCompare(node.name(), "cairo_ps") ||
		     magCompare(node.name(), "cairo_svg") ||
		     magCompare(node.name(), "x") ||
		     magCompare(node.name(), "cairo_eps") ||
		     magCompare(node.name(), "geotiff") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("cairo");
			CairoDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		CairoDriverAttributes::set(map);
	}

	void setPNG() const {backend_ = "png";}
	void setPDF() const {backend_ = "pdf";}
	void setPS () const {backend_ = "ps";}
	void setEPS() const {backend_ = "eps";}
	void setSVG() const {backend_ = "svg";}
	void setX()   const {backend_ = "x";}
	void setGEOTIFF() const {backend_ = "geotiff";}
	void setCairo() const {backend_ = "cairo";}

private:
	MAGICS_NO_EXPORT void startPage() const;
        MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;
	MAGICS_NO_EXPORT void setupNewSurface() const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT void setColour(cairo_t* ct, const Colour &col) const;
	MAGICS_NO_EXPORT int  setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderSimplePolygon() const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void renderImage(const ImportObject& obj) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol& symbol) const;
	MAGICS_NO_EXPORT bool convertToPixmap(const string &fname, const GraphicsFormat format, const int reso,
			     const MFloat wx0, const MFloat wy0,const MFloat wx1,const MFloat wy1) const;
	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	mutable string	filename_;

	mutable cairo_t*	cr_;
	mutable cairo_t*	tmp_cr_;
	mutable cairo_surface_t* surface_;
	mutable cairo_surface_t* tmp_surface_;

	MAGICS_NO_EXPORT MFloat projectX(const MFloat x) const {return offsetX_+(x*coordRatioX_);}
	MAGICS_NO_EXPORT MFloat projectY(const MFloat y) const {return offsetY_+(y*coordRatioY_);}
	MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setSymbolY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setFlagY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setY(const MFloat y) const {return y;}
#ifdef MAGICS_GEOTIFF
	MAGICS_NO_EXPORT void write_tiff() const;
#endif
	MAGICS_NO_EXPORT void write_8bit_png() const;
	mutable MFloat offsetX_;
	mutable MFloat offsetY_;
	mutable stack<MFloat> offsetsX_;
	mutable stack<MFloat> offsetsY_;

	mutable string backend_;
	mutable int dimensionXglobal_;
	mutable int dimensionYglobal_;

	//! Copy constructor - No copy allowed
	CairoDriver(const CairoDriver&);
	//! Overloaded << operator to copy - No copy allowed
	CairoDriver& operator=(const CairoDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CairoDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
