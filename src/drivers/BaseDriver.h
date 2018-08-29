/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \defgroup drivers Output drivers

   \section generalDriver Overview

   Magics++ supports various vector and raster output formats. It
   produces a generic descriptions of the output plot which
   than gets converted by a chosen driver to the requested format(s).

   \sa DriverManager::DriverManager(), OutputHandler(), OutputFactory()

   \section usingDrivers Using drivers

   In MagML the \<drivers\> Tag is used to define all drivers being used. Each output
   format

   Example test/MagML/drivers.magml test/C/drivers.c

   \section addDriver How to add a new output format / driver

   It is assumed you try to add a driver called <i>NewDriver</i>.

   - go into the <i>tools</i> directory and run <i>perl newdriver New</i>
   - edit your <i>NewDriver.xml</i> and copy it to <i>src/xml</i>
   - add this file in <i>src/xml/Makefile.am</i>
   - edit <i>NewDriver.cc/.h</i> and copy them to <i>src/drivers/</i>
   - add the new files in <i>src/drivers/Makefile.am</i>
   - look at <i>NewDriver.todo</i> and add factories in:
		-# <i>src/common/OutputHandler.cc</i>
		-# <i>src/common/OutputFactory.cc/.h</i>


\example drivers.c Example how mulitiples drivers can be used in C.
\example drivers.magml Example how drivers can be used in MagML.
*/

/*! \file BaseDriver.h
    \brief Definition of driver base class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/
#ifndef MPP_BaseDriver_H
#define MPP_BaseDriver_H

#include <stack>

#include <magics.h>
#include <MagTranslator.h>
#include <Colour.h>
#include <BaseDriverAttributes.h>
#include "MagicsObserver.h"

namespace magics{

class Layer;
class SceneLayer;
class StaticLayer;
class NoDataLayer;
class StepLayer;

class Layout;
class SceneLayout;
class LegendLayout;
class StartPage;
class EndPage;
class RootLayout;
class BinaryObject;
class Polyline;

class Text;
class Symbol;
class TextSymbol;
class ClearObject;
class ComplexSymbol;
class TextItem;
class FlagItem;
class SymbolItem;

class Image;
class Arrow;
class Flag;
class ImportObject;
class PaperPoint;

struct ShadingProperties;
struct FillShadingProperties;
struct DotShadingProperties;
struct HatchShadingProperties;

class SelectionMode;
class MtInputEvent;
class PreviewLayout;
class HistoLayout;
class MagnifierLayout;

/*!
 Codes for Brinary reading and writing
*/
#define BINARY_VERSION 2               // last updated with version 2.15.0

typedef double MFloat;


/*!
 \brief helper compare function for string maps

 The function follows the descriptionn of Josuttis's STL book at page 213.
*/
class RuntimeStringCompare
{
	static bool nocase_compare(char c1, char c2) {return toupper(c1)<toupper(c2);}
public:
	RuntimeStringCompare(){}
	bool operator() (const string& s1, const string& s2) const 
	{
		return lexicographical_compare(s1.begin(),s1.end(),s2.begin(),s2.end(),nocase_compare);
	}
};

/*!
 \brief Method to extract points of Polylines
 
 First all possible commas are replaced by spaces and than all points are read through a stream 
*/
inline void parsePoints(vector<PaperPoint> &vPP, string &points)
{
	string::size_type pos = points.find_first_of(",");

	while(pos != string::npos)
	{
		points.replace(pos,1," ",1);
		pos = points.find_first_of(",",pos);
	}

	istringstream totalString(points);
	MFloat x,y;

	while(!totalString.eof())
	{
		totalString >> x >> y;
		PaperPoint pp;
		pp.x(x);
		pp.y(y);
		vPP.push_back(pp);
	}
}

/*!
 \brief struct describing an XML element

 Helper construct for symbol plotting.
*/
struct xmlElement
{
	string name;
	std::map<string, string> attributes;
	xmlElement() : name("") {}
};

/*!
 \brief struct describing an SVG element

 Helper construct for symbol plotting.
*/
struct svgSymbol
{
	string id;
	vector<xmlElement> elements;
	svgSymbol() : id("") {}
};

typedef vector<svgSymbol> vSymbols;

/*!
 \brief struct describing a entry in the font table

 All fonts read in by readFonts() are stored in this way.

 \sa readFonts()
*/
struct magFont
{
	int	id;
	string	magics_name;
	string	ps_name;
	string	ps_filename;
	string	ttf_filename;
	string	css_name;
};


/*! \class BaseDriver
    \brief Base class for all drivers of Magics++.
    \ingroup drivers

    This abstract class provides an interface for Magics
    to call various device drivers.

    Every new driver has to inherit this class and has
    to provide own implementation of abstract members.
*/
class BaseDriver : public BaseDriverAttributes, public MagicsObserver
{
public:
	BaseDriver();
	virtual ~BaseDriver();

	virtual void set(const XmlNode& node)
	{
		BaseDriverAttributes::set(node);
	}
	void setWidth(double width) { width_ = width; }

	virtual void set(const std::map<string, string>& map)
	{
		BaseDriverAttributes::set(map);
	}

	virtual void open() {}
	virtual void close() {}
	void printOutputName(const std::string & str) const; 

	virtual void project(const Layout& ) const {}
	virtual void unproject() const {}

	virtual void newLayer() const { debugOutput("BaseDriver::newLayer");}
	virtual void closeLayer() const { debugOutput("BaseDriver::closeLayer");}
	virtual void newLayer(Layer&) const {debugOutput("BaseDriver::newLayer()");}
	virtual void closeLayer(Layer&) const {debugOutput("BaseDriver::closeLayer()");}
	virtual void newLayer(StaticLayer&) const {debugOutput("BaseDriver::newStaticLayer");}
	virtual void closeLayer(StaticLayer&) const {debugOutput("BaseDriver::closeStaticLayer");}
	virtual void newLayer(StepLayer&) const {debugOutput("BaseDriver::newStepLayer");}
	virtual void closeLayer(StepLayer&) const {debugOutput("BaseDriver::closeStepLayer");}

	intarray frames() const  { return frame_list_; }

	MAGICS_NO_EXPORT void redisplay(const Layout&) const;
	MAGICS_NO_EXPORT void redisplay(const RootLayout&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const LegendLayout&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const SceneLayout&) const;
	MAGICS_NO_EXPORT void redisplay(const StartPage&) const;
	MAGICS_NO_EXPORT void redisplay(const EndPage&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const Layer&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const SceneLayer&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const StaticLayer&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const NoDataLayer&) const;
	virtual MAGICS_NO_EXPORT void redisplay(const StepLayer&) const;
	
	virtual MAGICS_NO_EXPORT void redisplay(const Polyline&) const;
	

#ifdef MAGICS_QT
	virtual MAGICS_NO_EXPORT void redisplay(const PreviewLayout&) const {};
	virtual MAGICS_NO_EXPORT void redisplay(const HistoLayout&) const {};
	virtual MAGICS_NO_EXPORT void redisplay(const MagnifierLayout&) const {};
#endif
	typedef void (BaseDriver::*ModeFunction)(const SelectionMode&);
	typedef void (BaseDriver::*ControlFunction)(bool);
	typedef void (BaseDriver::*InputEventFunction)(MtInputEvent*);

	virtual void redisplay(const BinaryObject&) const;
	void setDimensionsFromBinary(string mbg_tmpl,MFloat &ratio,int &width) const;

	//! Method to redisplay a Text.
	MAGICS_NO_EXPORT void redisplay(const Text&) const;
	//! Method to redisplay a Symbol.
	MAGICS_NO_EXPORT void redisplay(const Symbol&) const;
	//! Method to redisplay a TextSymbol.
	MAGICS_NO_EXPORT void redisplay(const TextSymbol&) const;
	//! Method to redisplay a ComlpexSymbol.
	void redisplay(const ComplexSymbol&) const;
	void redisplay(const ClearObject&) const;
	void redisplay(const TextItem&, const ComplexSymbol&) const;
	void redisplay(const FlagItem&, const ComplexSymbol&) const;
	void redisplay(const SymbolItem&, const ComplexSymbol&) const;
	
	//! Method to redisplay an Arrow.
	virtual MAGICS_NO_EXPORT void redisplay(const Arrow&) const;
	//! Method to redisplay an Flag.
	virtual MAGICS_NO_EXPORT void redisplay(const Flag&) const;
	//! Method to redisplay a external file.
	void redisplay(const ImportObject&) const;
	//! Method to redisplay a external file.
	MAGICS_NO_EXPORT void redisplay(const Image&) const;

	void shade(const FillShadingProperties&) const;
	void shade(const DotShadingProperties&) const;
	void shade(const HatchShadingProperties&) const;

	bool disable() const { return disabled_; }
	void disable(bool disabled) { disabled_ = disabled; }


	/*!
	 SUPER_PAGE_X_LENGTH Default: 29.7
	*/
	MAGICS_NO_EXPORT void setXDeviceLength(MFloat xdevicelength)
		{ xDeviceLength_ =  xdevicelength; }
	MAGICS_NO_EXPORT MFloat getXDeviceLength() const
		{ return xDeviceLength_; }

	/*!
	 SUPER_PAGE_Y_LENGTH Default: 21.0
	*/
	MAGICS_NO_EXPORT void setYDeviceLength(MFloat ydevicelength)
		{ yDeviceLength_ =  ydevicelength; }
	MAGICS_NO_EXPORT MFloat getYDeviceLength() const
		{ return yDeviceLength_; }


protected:
	MFloat convertCM(const MFloat cm) const {return cm*cmScale_;}
	void setCMscale(const MFloat scale) const {cmScale_ = scale;}

	/*!
	  \brief set Y values positive or negative

	  Some drivers hava a Y axis which is positive towards the bottom of the plot
	  and these need to set this method negative.
	*/
	virtual MAGICS_NO_EXPORT MFloat setY(const MFloat y) const {return y;}

	/*!
	  \brief set Y values positive or negative dependent how symbols are interpreted

	*/
	virtual MAGICS_NO_EXPORT MFloat setSymbolY(const MFloat y) const {return y;}

	/*!
	  \brief set Y values positive or negative dependent how angles are interpreted

	  Some drivers hava a angle orientation different to PS
	  and these need to set this method negative.
	*/
        virtual MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const {return y;}
        virtual MAGICS_NO_EXPORT MFloat setFlagY(const MFloat y) const {return y;}

	string getFileName(const string &extension, const unsigned int no = 0) const;

	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;
        virtual void startPage() const{};
        virtual void endPage() const{};
	virtual void setNewColour(const Colour &) const {};
	virtual void printLine(const Polyline &line) const;

	virtual void renderText(const Text &) const {}
	virtual void debugOutput(const string &s) const 
	{
		if(debug_) MagLog::debug() <<" DRIVERS: "<<s<< "\n";
	}

	virtual MFloat projectX(const MFloat x) const {return coordRatioX_*x;}
	virtual MFloat projectY(const MFloat y) const {return coordRatioY_*y;}
		
	string getTmpName() const;

	double LSF(MFloat *x,MFloat *y, int i0) const;

	mutable int	currentPage_;
	mutable string	fileName_;
	mutable string	currentLayer_;  // from Layer (open & close)

	mutable LineStyle currentLineType_;
	mutable MFloat	currentLineWidth_;
	mutable int	currentLineStyle_;
	mutable Colour	currentColour_;

	mutable MFloat	coordRatioX_;
	mutable MFloat	coordRatioY_;
	mutable MFloat	dimensionX_;
        mutable MFloat	dimensionY_;
        mutable MFloat	offsetX_;
        mutable MFloat	offsetY_;

	mutable std::stack<MFloat>  dimensionStack_;
	mutable std::stack<MFloat>  scalesX_;
	mutable std::stack<MFloat>  scalesY_;

//	mutable MFloat	lastAreaHeightPercentage_;
//	mutable MFloat	lastAreaWidthPercentage_;
	mutable bool	newPage_;
//	mutable bool	firstPage_;
//	mutable	bool	newLayout_;
//	mutable	bool    external_;
//	mutable bool    polylineAntialiasing_;

	bool    disabled_;
	bool    alphaEnabled_;
	mutable int       applyGaussianBlur_;
	mutable vSymbols  sym_;

	virtual MAGICS_NO_EXPORT void setNewLineWidth(const MFloat w) const {currentLineWidth_ = w;}
	MAGICS_NO_EXPORT MFloat getNewLineWidth() const {return currentLineWidth_;}

	//! Load svg symbols from file
	void loadSymbols() const;

	virtual MAGICS_NO_EXPORT void renderTextSymbols(const TextSymbol& symbol) const;
	
	virtual void renderComplexSymbols(const ComplexSymbol& symbol) const;
	virtual void renderTextItem(const TextItem&, const ComplexSymbol&) const;
	virtual void renderFlagItem(const FlagItem&, const ComplexSymbol& symbol) const;
	virtual void renderSymbolItem(const SymbolItem&, const ComplexSymbol& symbol) const;
	virtual void renderSymbols(const Symbol& symbol) const;
	virtual void renderPolyline(const int, MFloat *, MFloat *) const {};
	virtual void renderPolyline2(const int, MFloat *, MFloat *) const {}
	void renderPolyline(vector<PaperPoint> &vP) const;
	void renderPolyline2(vector<PaperPoint> &vP) const;

	virtual MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const = 0;
	virtual MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const = 0;
	virtual void circle(const MFloat, const MFloat, const MFloat, const int) const {}
	virtual MAGICS_NO_EXPORT int setLineParameters(const LineStyle st, const MFloat w) const {currentLineType_=st; setNewLineWidth(w);return 0;}

	// Filling calculating methods
	// PolylineSets rendering methods
	//MAGICS_NO_EXPORT void renderPolylineSets(const PolylineSet<PaperPoint>&) const;
	mutable unsigned int indexHatch_;
	mutable Shading   currentShading_;
	mutable const ShadingProperties *currentShadingProperties_;

        // wind calculating methods (overridden in Binary)
        virtual MAGICS_NO_EXPORT void renderWindArrow(const Arrow &arrow) const;
        virtual MAGICS_NO_EXPORT void renderWindFlag(const Flag &flag) const;

	// images + bitmap methods
	virtual void renderImage(const ImportObject& object) const;
	MAGICS_NO_EXPORT void renderImage(const Image& obj) const {renderCellArray(obj);}
        virtual MAGICS_NO_EXPORT bool convertToPixmap(const string &fname, const GraphicsFormat format, const int reso,
	             const MFloat wx0, const MFloat wy0,const MFloat wx1,const MFloat wy1) const;
	virtual MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool hasAlpha=false) const;
	virtual MAGICS_NO_EXPORT bool renderCellArray(const Image& ) const;

	mutable std::map<string, magFont, RuntimeStringCompare> FontMap_;
	typedef std::map<string, magFont, RuntimeStringCompare>::const_iterator fontMapIter;

	void readFonts() const;
	void relieveFonts() const {FontMap_.clear();}

private:
	mutable MFloat cmScale_;
	MFloat xDeviceLength_;
	MFloat yDeviceLength_;
	static int numFiles_;
	mutable double obs_distance_;
	mutable stack<const Layout *>  staLayouts_;
	mutable vector<const PaperPoint *>  vecPoints_;
	bool checkDistanceMoreThan(const PaperPoint *pp, double distance) const;
	void renderSimplePolygon(vector<PaperPoint> &vP) const;
	void snowflake(const MFloat, const MFloat, const MFloat) const;
	void drizzle(const MFloat, const MFloat, const MFloat) const;
	void triangle(const MFloat, const MFloat, const MFloat, const int, const int) const;
	void lightning(const MFloat x, const MFloat y, const MFloat size) const;

	//! Copy constructor - No copy allowed
	BaseDriver(const BaseDriver&);
	//! Overloaded << operator to copy - No copy allowed
	BaseDriver& operator=(const BaseDriver&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BaseDriver& p) { p.print(s); return s; }
};

/*!
 \brief translates a string to a driver
*/
template<>
class MagTranslator<string, BaseDriver>
{
public:
	MAGICS_NO_EXPORT BaseDriver* operator()(const string& val )
	{
		 return SimpleObjectMaker<BaseDriver>::create(val);
	}
	MAGICS_NO_EXPORT BaseDriver* magics(const string& param)
	{
		BaseDriver* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
