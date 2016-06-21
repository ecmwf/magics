/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XmlMagics.h
    \brief Definition of the Template class XmlMagics.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 3-Apr-2007
    
    Changes:
    
*/

#ifndef XmlMagics_H
#define XmlMagics_H

#include "magics.h"
#include "XmlNode.h"
#include "DriverManager.h"
#include "OutputHandler.h"
#include "RootSceneNode.h"
#include "MagicsObserver.h"



namespace magics {

class GribLoop;
class XmlTree;


class XmlMagics : public XmlNodeVisitor, 
	public stack<BasicSceneObject*>, 
	public vector<MagicsObserver*>
{
public:
	XmlMagics(); 
	~XmlMagics();
	
	void execute(const string&, std::map<string, string>& metadata);
	void execute(const string& magml)
	{
		std::map<string, string> data;
		execute(magml, data);
	}
	void execute(XmlTree& magml);
	
	void visit(const XmlNode& node);

	void magics(const XmlNode&);

	void page(const XmlNode&);
	void binary(const XmlNode&);
	void import(const XmlNode&);
	void text(const XmlNode&);
	void map(const XmlNode&);
	void coastlines(const XmlNode&);
	void tephigrid(const XmlNode&);
	void thermo(const XmlNode&);
	void taylor(const XmlNode&);
	void layer(const XmlNode&);
	void grib(const XmlNode&);
	void table(const XmlNode&);
	void geopoints(const XmlNode&);
	void netcdf(const XmlNode&);
	void xyinput(const XmlNode&);
	void input(const XmlNode&);
	void odb(const XmlNode&);
	void oda(const XmlNode&);
	void contour(const XmlNode&);
	void histogram(const XmlNode&);
	void wind(const XmlNode&);
	void symbol(const XmlNode&);
	void matrix(const XmlNode&);
	void cartesian(const XmlNode&);
	void geographical(const XmlNode&);

	void driver(const XmlNode&);

	void legend(const XmlNode&);
	void image(const XmlNode&);

	void horizontalAxis(const XmlNode&);
	void verticalAxis(const XmlNode&);

	void efigraph(const XmlNode&);
	void cdfgraph(const XmlNode&);
	void efigram(const XmlNode&);

	void metgraph(const XmlNode&);
	void metgram(const XmlNode&);

	void epsgraph(const XmlNode&);
	void epsgram(const XmlNode&);
	void epsbufr(const XmlNode&);
	void epsxml(const XmlNode&);
	void wrepjson(const XmlNode&);
	void geojson(const XmlNode&);
	void epswind(const XmlNode&);
	void epscloud(const XmlNode&);
	void epsbar(const XmlNode&);
	void epswave(const XmlNode&);
	void epsshading(const XmlNode&);
	void epsplume(const XmlNode&);
	void epsdirection(const XmlNode&);

	void polyinput(const XmlNode&);
	void polyline(const XmlNode&);

	void mapgen(const XmlNode&);
	
	void graph(const XmlNode&);
	
	void gribloop(const XmlNode&);
	
	void gribinloop(const XmlNode&);
	void splitinloop(const XmlNode&);
	
	void metadata(const XmlNode&);
	void thread(const XmlNode&);
	void split(const XmlNode&);

	void display(const string&);

protected:
	//! Method to print string about this class on to a stream of type ostream.
	void print(ostream&) const; 
	DriverManager		 drivers_;
	XmlRootSceneNode*        root_;
	OutputHandler            output_;
	GribLoop*                gribloop_;
	bool                     geographical_;
	double                width_;

	typedef void (XmlMagics::*Action)(const XmlNode&);

	std::map<string, Action> actions_;
	bool driversToSet_;

private:
	//! Copy constructor - No copy allowed
	XmlMagics(const XmlMagics&);
	//! Overloaded << operator to copy - No copy allowed
	XmlMagics& operator=(const XmlMagics&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XmlMagics& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
