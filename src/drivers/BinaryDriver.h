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
    \file BinaryDriver.h
    \brief Definition of BinaryDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Sun Oct  4 20:28:15 2009
*/

#ifndef _MPP_BinaryDriver_H
#define _MPP_BinaryDriver_H

#include <BaseDriver.h>
#include <BinaryDriverAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class BinaryDriver
    \brief This driver produces output for Binary
    \ingroup drivers

    This driver ...
*/
class BinaryDriver: public BaseDriver, public BinaryDriverAttributes
{

public:
	BinaryDriver();
	~BinaryDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "mgb") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("binary");
			BinaryDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<std::string, std::string>& map)
	{
		BaseDriver::set(map);
		BinaryDriverAttributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;

	// BinaryDriver specific member functions BEGIN
        MAGICS_NO_EXPORT void renderWindArrow(const Arrow &arrow) const;
        MAGICS_NO_EXPORT void renderWindFlag(const Flag &flag) const;
	
	MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setSymbolY(const MFloat y) const {return -y;}
	MAGICS_NO_EXPORT MFloat setFlagY(const MFloat y) const {return y;}
	MAGICS_NO_EXPORT MFloat setY(const MFloat y) const {return y;}

	mutable ofstream out_;

	// BinaryDriver specific member functions END

	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const {MagLog::debug() << s << endl;}

	//! Copy constructor - No copy allowed
	BinaryDriver(const BinaryDriver&);
	//! Overloaded << operator to copy - No copy allowed
	BinaryDriver& operator=(const BinaryDriver&);

	mutable stack<MFloat> offsetsX_;
	mutable stack<MFloat> offsetsY_;
	mutable int dimensionYglobal_;

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BinaryDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
