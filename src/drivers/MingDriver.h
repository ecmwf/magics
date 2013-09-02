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
    \file MingDriver.h
    \brief Definition of MingDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue Nov 20 12:26:52 2007
*/

#ifndef _MingDriver_H
#define _MingDriver_H

#include <BaseDriver.h>
#include <MingDriverAttributes.h>
#include <XmlNode.h>

#include <mingpp.h>

namespace magics
{

/*! \class MingDriver
    \brief This driver produces output for Ming
    \ingroup drivers

    This driver ...
*/
class MingDriver: public BaseDriver, public MingDriverAttributes
{

public:
	MingDriver();
	~MingDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "swf") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("swf");
			MingDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		MingDriverAttributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject(const magics::UnLayout&) const;
	MAGICS_NO_EXPORT void projectBox(const string &) const;
	MAGICS_NO_EXPORT void unprojectBox() const;
	MAGICS_NO_EXPORT void newLayer(const PhysicalLayer& ) const;
	MAGICS_NO_EXPORT void setNewLineWidth(const float) const;
	MAGICS_NO_EXPORT void closeLayer(const UnPhysicalLayer& ) const;

	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const float w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, float *, float *) const;
	MAGICS_NO_EXPORT void renderPolyline2x(const int, int*, const int) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, float *x, float *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, float *, float *) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const float x, const float y, const float r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(float,float,float,float,int,int,unsigned char*,int, bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void endPolygon() const;

	MAGICS_NO_EXPORT float setY(const float y) const {return -y;}
	MAGICS_NO_EXPORT float projectX(const float x) const {return offsetX_+(x*coordRatioX_);}
	MAGICS_NO_EXPORT float projectY(const float y) const {return offsetY_+(y*coordRatioY_);}
	mutable SWFMovie*     movie_;
	mutable SWFFillStyle* currentStyle_;
	mutable float offsetX_;
	mutable float offsetY_;
	mutable stack<float> offsetsX_;
	mutable stack<float> offsetsY_;
	mutable stack<float> boxoffsetsX_;
	mutable stack<float> boxoffsetsY_;

	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	//! Copy constructor - No copy allowed
	MingDriver(const MingDriver&);
	//! Overloaded << operator to copy - No copy allowed
	MingDriver& operator=(const MingDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MingDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
