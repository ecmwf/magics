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
    \file PostScriptDriver.h
    \brief Definition of PostScriptDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: March 2004
*/

#ifndef MPP_PostScriptDriver_H
#define MPP_PostScriptDriver_H

#include <BaseDriver.h>
#include <PostScriptDriverAttributes.h>
#include <XmlNode.h>

namespace magics
{
/*! \class PostScriptDriver
    \brief This driver produces output in PostScript, EPS and PDF.
    \ingroup drivers

    This driver produces output in one or multiple PostScript file(s) (*.ps), multiple
    EPS (*.eps) and/or one PDF (*.pdf).

    \todo DRIVERS: PDF is still raster and needs to be vector!
    \note PDF is currently produced throgh <b>ghostscript</b>. This means it uses raster instead of
    vector information to describe the graphics.
*/
class PostScriptDriver: public BaseDriver, public PostScriptDriverAttributes
{

public:
	PostScriptDriver();
	~PostScriptDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "ps") ||
		     magCompare(node.name(), "eps") ||
		     magCompare(node.name(), "pdf") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("ps");
			PostScriptDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<string, string>& map)
	{
		BaseDriver::set(map);
		PostScriptDriverAttributes::set(map);
	}

	void setEPS(bool b) const	{eps_=b;}
	void setPDF(bool b) const	{pdf_=b;}
	void setPS(bool b) const	{ps_=b;}
	bool isEPS() const	{return eps_;}
	bool isPDF() const	{return pdf_;}
	bool isPS() const	{return ps_;}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT void writeColour() const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol& ) const;

	// P O S T S C I P T specific member functions BEGIN

	MAGICS_NO_EXPORT void writePSFileHeader() const;
	MAGICS_NO_EXPORT void writePSFileEnd() const;
	MAGICS_NO_EXPORT void copyMacro(fstream *m, const string &file) const;
	MAGICS_NO_EXPORT void setDeviceColourModel(const string &m) const;
	MAGICS_NO_EXPORT fstream* getStream() const {return &PSOut_;}
	MAGICS_NO_EXPORT int getDeviceColourModel() const {return deviceColourModel_;}

	//! Should the output be splited?
	MAGICS_NO_EXPORT bool isSplit() const {return ((isEPS()||split_) && !isPDF());}
	MAGICS_NO_EXPORT void openFile() const;
	MAGICS_NO_EXPORT void closeFile() const;

	mutable fstream   PSOut_;
	mutable bool      ps_;
	mutable bool      pdf_;
	mutable bool      eps_;
	mutable bool      old_;
	mutable	unsigned int maxPathSize_;
	mutable	unsigned int deviceColourModel_;
	mutable Colour currentWrittenColour_;

	// P O S T S C I P T specific member functions END

	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	//! Copy constructor - No copy allowed
	PostScriptDriver(const PostScriptDriver&);
	//! Overloaded << operator to copy - No copy allowed
	PostScriptDriver& operator=(const PostScriptDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PostScriptDriver& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
