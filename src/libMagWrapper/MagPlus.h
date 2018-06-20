/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagPlus_H
#define MagPlus_H

#include <magics.h>
#include "DriverManager.h"

// undefine the macro ABS because it is defined in mars.h and will be redefined
// by Terralib (only an big issue on AIX)

#undef ABS

#include "OutputHandler.h"
#include "RootSceneNode.h"
#include "MagicsEvent.h"
#include "MagRequest.h"




#ifdef MAGICS_QT
class QGraphicsScene;
#endif

namespace magics {

#ifdef MAGICS_QT
class QtDriver;
#endif

class FortranViewNodeWrapper;
class MultiVisdef;

class MagPlus : public stack<BasicSceneObject*>
{
public:
	MagPlus();
	~MagPlus() {}

	void execute(MagRequest&);

	static void decode(MagRequest&, const string&);

	void registerObserver(MagicsObserver* observer) { observers_.push_back(observer); }
	void unregisterObserver(MagicsObserver* observer);
	//! Notify drivers about an event (GUI -> Qt)
	void notifyDrivers(MagicsEvent& event) { drivers_.dispatch(&MagicsEvent::notify, event); }

	//! Notify the magicsobservers about an event in the QtDriverObserver (Qt -> GUI)
	void notify(MagicsEvent&);

#ifdef MAGICS_QT
	//! Set the QGraphicsScene for the Qt driver
	void setQtScene(QGraphicsScene*);
#endif


protected:
	vector<MagicsObserver*> observers_;
	vector<BasicSceneObject*> foreground_;
	
	typedef bool (MagPlus::*ObjectCreator)(magics::MagRequest&);
	typedef Data* (MagPlus::*DataCreator)(magics::MagRequest&);
	static map<string,  ObjectCreator > driverCreators_;
	static map<string,  DataCreator > dataCreators_;
	static map<string,  ObjectCreator > sceneCreators_;
	static map<string,  ObjectCreator > sceneUpdators_;
	map<string,  ObjectCreator >* sceneHandler_;
	map<int,  FortranViewNodeWrapper* > pages_;

	void setIconInfo(magics::MagRequest&, MetviewIcon&);

	bool page(magics::MagRequest&);
	bool newpage(magics::MagRequest&);
	bool page_update(magics::MagRequest&);
	bool superpage(magics::MagRequest&);
	bool cartesian(magics::MagRequest&);

	bool coastlines(magics::MagRequest&);

	bool cartesianGrid(magics::MagRequest&);
    bool tephiGrid(magics::MagRequest&);
    bool skewtGrid(magics::MagRequest&);
    bool emagramGrid(magics::MagRequest&);
	bool taylorGrid(magics::MagRequest&);
	bool tephigrid(magics::MagRequest&);
	bool oldcoastlines(magics::MagRequest&);
	bool axis(magics::MagRequest&);
	bool grib(magics::MagRequest&);
	bool geojson(magics::MagRequest&);
	bool gribloop(magics::MagRequest&);
	bool rasterloop(magics::MagRequest&);
	bool dataloop(magics::MagRequest&);
	bool bufr(magics::MagRequest&);
	bool visdef(magics::MagRequest&);
	bool layer(magics::MagRequest&);
	
#ifdef HAVE_ODB
	bool geoodb(magics::MagRequest&);
	bool xyodb(magics::MagRequest&);
#endif
	bool netcdf(magics::MagRequest&);
	Data* createnetcdf(magics::MagRequest&);


	bool geopoints(magics::MagRequest&);
	Data* createGeopoints(magics::MagRequest&);

	bool raster(magics::MagRequest&);
	bool input(magics::MagRequest&);
	bool table(magics::MagRequest&);
	bool binning(magics::MagRequest&);

	bool contour(magics::MagRequest&);
	bool symbol(magics::MagRequest&);
	bool wind(magics::MagRequest&);
	bool import(magics::MagRequest&);
	bool obs(magics::MagRequest&);
	bool graph(magics::MagRequest&);
	bool multi(magics::MagRequest&);
	
	bool ptext(magics::MagRequest&);
	bool text(magics::MagRequest&);
	bool legend(magics::MagRequest&);
	bool ignore(magics::MagRequest&);
	bool device(magics::MagRequest&);

	bool qtdriver(magics::MagRequest&);
	bool pdfdriver(magics::MagRequest&);
	bool pngdriver(magics::MagRequest&);
	bool svgdriver(magics::MagRequest&);
	bool kmldriver(magics::MagRequest&);  
	bool geojsondriver(magics::MagRequest&);  
	bool psdriver(magics::MagRequest&);
	bool epsdriver(magics::MagRequest&);
	bool epscairodriver(magics::MagRequest&);
	bool pscairodriver(magics::MagRequest&);
    
	RootSceneNode* root_;
	DriverManager  drivers_;
	OutputHandler  output_;
	int  superpage_;
	bool geographical_;
	MagicsMode mode_;
	enum MetviewMode {creation, update };
	MetviewMode mvMode_;
	FortranViewNodeWrapper* page_;
	vector<Visdef*>* currentMulti_;
	
	// Information related to the current layer
	bool visibility_;
	int zindex_;
	int transparency_;
	string id_;
	string layer_;

#ifdef MAGICS_QT
	//! Keep the instance of the QtDrver!
	QtDriver* qtDriver_;
    QGraphicsScene * qtScene_;
#endif
};

} // namespace magics

#endif
