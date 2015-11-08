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

/*! \file CoastPlotting.h
    \brief Definition of the Template class CoastPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/


/*!
  \defgroup coastlines Coastlines
  
  \section overviewCoast Overview
  
  Coastlines are stored in SHP files and are stored in <i>share/magics/??m</i> directories.
  The shape files are downloaded from <a href="http://www.naturalearthdata.com/">http://www.naturalearthdata.com/</a>.
  
  \section sourceCode Coding
  
  - Parameters are defined in src/xml/CoastPlotting.xml
  
  - Generated files src/visualisers/BoundariesAttributes.cc src/visualisers/CoastPlottingAttributes.cc src/visualisers/CitiesAttributes.cc
  
  - Code for shape file reading is in src/decoders/ShapeDecoder.cc
  
  - Code for coastline processing src/visualisers/CoastPlotting.cc
  
  \section Boundaries Boundaries

-   src/visualisers/Boundaries.cc
  
  \section readingOfShape Pre-processing and reading of Shape files

  Only the following extensions are required for shape files: [ dbf, shp, shx, prj ]. Extensions .sbn and sbx are undocumented formats used only by ESRI software.

  To extract subsets of shape files into other shape files use the ogr2ogr command line utility (from gdal) as follows:
 
      ogr2ogr <output_folder> <input_shape_file>.shp -where "<condition>"

  For example, to ignore minor feature data in the 10m shape files we did the following:

      ogr2ogr output_folder 10m_lakes.shp -where "ScaleRank < 7"

  Where 'ScaleRank' is one of the fields in the shape file. Information on which fields are present are given in the .dbf file (which can be opened with Open Office Spreadsheet).

  Conditions can be combined, for example

      ogr2ogr output_folder 10m_lakes.shp -where "ScaleRank = 1 AND Name2 != 'Great Lakes'"

*/


#ifndef CoastPlotting_H
#define CoastPlotting_H

#include "magics.h"

#include "NoCoastPlottingAttributes.h"
#include "CoastPlottingAttributes.h"
#include "BasicSceneObject.h"
#include "BasicGraphicsObject.h"
#include "UserPoint.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "Polyline.h"
#include "Layer.h"
namespace magics {

class PreviewVisitor;

class NoCoastPlotting: public NoCoastPlottingAttributes{
public :
	NoCoastPlotting();
	virtual ~NoCoastPlotting() {}

	void set(const XmlNode& node)
	{
		NoCoastPlottingAttributes::set(node);
	}

	virtual void set(const map<string, string>& map)
	{
		NoCoastPlottingAttributes::set(map);
	}
	bool accept(const string& node) { return NoCoastPlottingAttributes::accept(node); }
	virtual NoCoastPlotting* clone() const
	{
		return new NoCoastPlotting();
	}
	virtual void operator()(DrawingVisitor& parent);
	virtual void operator()(PreviewVisitor&) {}
	virtual void layer(BasicGraphicsObjectContainer*) {}
	virtual void visit(LegendVisitor&);
	virtual void visit(MetaDataCollector&);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "NoCoastPlotting\n"; }
		typedef void (NoCoastPlotting::*Action)(DrawingVisitor&);
		std::map<string, Action> riversMethods_;
		
		void rivers(DrawingVisitor&);
		void ignore(DrawingVisitor&) {}
		
		void layers(map<string, Action>&, const string&, DrawingVisitor&);
		map<string, string> coastSet_;
		MetaDataCollector meta_;
private:
    //! Copy constructor - No copy allowed
	NoCoastPlotting(const NoCoastPlotting&);
    //! Overloaded << operator to copy - No copy allowed
	NoCoastPlotting& operator=(const NoCoastPlotting&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoCoastPlotting& p)
		{ p.print(s); return s; }
};



class CoastPlotting: public NoCoastPlotting, public CoastPlottingAttributes {

public:

	CoastPlotting();
	virtual ~CoastPlotting();
	void set(const XmlNode& node) {
			CoastPlottingAttributes::set(node);
			NoCoastPlottingAttributes::set(node);
	}

	virtual void set(const map<string, string>& map)
	{
			NoCoastPlottingAttributes::set(map);
			CoastPlottingAttributes::set(map);
	}
	bool accept(const string& node) { return NoCoastPlottingAttributes::accept(node); }

	virtual NoCoastPlotting* clone() const {
		CoastPlotting* object = new CoastPlotting();
		//object->copy(*this);
		return object;
	}

	virtual void visit(LegendVisitor&);
	virtual void operator()(DrawingVisitor&);
	virtual void operator()(PreviewVisitor&);


protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;

	void decode(const Layout&);
	
	void landsea(Layout&);
	void landonly(Layout&);
	void seaonly(Layout&);
	void nolandsea(Layout&);

	void setLine(Polyline&);
	void setSeaShading(Polyline&);
	void setLandShading(Polyline&);
	Polyline* ocean(Layout&);

	string coast_resolution_;

	vector<Polyline> coast_;
//	vector<Polyline> coastlines_;
	vector<Polyline> lake_;
	vector<Polyline> ocean_;
};


template <>
class MagTranslator<string, NoCoastPlotting> {
public:
	NoCoastPlotting* operator()(const string& val )
	{
		return SimpleObjectMaker<NoCoastPlotting>::create(val);
	}

	NoCoastPlotting* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
