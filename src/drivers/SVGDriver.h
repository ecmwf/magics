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
    \file SVGDriver.h
    \brief Definition of SVGDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Oct 26 20:58:21 2007
*/

#ifndef _SVGDriver_H
#define _SVGDriver_H

#include <BaseDriver.h>
#include <SVGDriverAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class SVGDriver
    \brief This driver produces output for SVG
    \ingroup drivers

    This driver provides output in the SVG format:
    
    \sa http://www.w3.org/Graphics/SVG/
*/
class SVGDriver: public BaseDriver, public SVGDriverAttributes
{

public:
	SVGDriver();
	~SVGDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		XmlNode basic = node;
		basic.name("driver");
		BaseDriver::set(basic);
		basic.name("svg");
		SVGDriverAttributes::set(basic);
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		SVGDriverAttributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;
	MAGICS_NO_EXPORT void redisplay(const StaticLayer& layer) const;
	MAGICS_NO_EXPORT void redisplay(const StepLayer& layer) const;
	MAGICS_NO_EXPORT void redisplay(const NoDataLayer& layer) const;
	MAGICS_NO_EXPORT void newLayer() const;
	MAGICS_NO_EXPORT void closeLayer() const;
//	MAGICS_NO_EXPORT void renderInteractiveBegin(const InteractiveBegin&) const;
//	MAGICS_NO_EXPORT void renderInteractiveEnd(const InteractiveEnd&) const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;
//	MAGICS_NO_EXPORT void printLine(const Polyline &line) const;

//	MAGICS_NO_EXPORT void redisplay(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol& symbol) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void renderImage(const ImportObject& obj) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;

	//! Method to print string about this class on to a stream of type ostream (virtual).
	MAGICS_NO_EXPORT void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	MAGICS_NO_EXPORT MFloat setY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setSymbolY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const {return y;}
	MAGICS_NO_EXPORT MFloat setFlagY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT double getRatio() const {return getYDeviceLength() / getXDeviceLength();}

	MAGICS_NO_EXPORT void openGroup(string g) const;
	MAGICS_NO_EXPORT void closeGroup() const;
	MAGICS_NO_EXPORT bool isGroup() const {return groupString_.empty();}

	mutable ofstream	pFile_;        //!< Output stream to file.
	mutable string		tmp_pFile_;    //!< Tmp to hold output stream to file.
	mutable string		groupString_;
	mutable int		group_counter_;
//	mutable bool		showCoordinates_;
	mutable bool		inkscape_;
//	mutable bool		interactive_;
//	mutable unsigned short	interactiveCounter_;

	mutable std::vector<string> layers_;
	mutable stringarray svg_output_resource_list_;

	//! Copy constructor - No copy allowed
	SVGDriver(const SVGDriver&);
	//! Overloaded << operator to copy - No copy allowed
	SVGDriver& operator=(const SVGDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SVGDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
