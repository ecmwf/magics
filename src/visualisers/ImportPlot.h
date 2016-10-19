/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportPlot.h
    \brief Definition of the Template class ImportPlot.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/

#ifndef ImportPlot_H
#define ImportPlot_H

#include "magics.h"

#include "ImportPlotAttributes.h"
#include "Visdef.h"
#include "ImportObject.h"
#include "LegendVisitor.h"

namespace magics {


class OverlayPlot: public ImportPlotAttributes, public Visdef {

public:
	OverlayPlot() {}
	virtual ~OverlayPlot() {}
	void set(const map<string, string>& map) { ImportPlotAttributes::set(map); }

	virtual void operator()(Data& data, BasicGraphicsObjectContainer& visitor)
	{
		// Here we work on projection coordinates!
		ImportObject* object = new ImportObject();
	    object->setPath(data.path());
	    object->setOrigin(PaperPoint(x_, y_));
	    object->setWidth(width_);
	    object->setHeight(height_);
	    object->setFormat(format_);
	    object->setOriginReference(ImportObject::bottom_left);
	   
	    if ( crs_.empty() ) {
	    	    	visitor.push_back(object);
	    	    	return;
	    	    }
	    	    // here we are doing a bit of cheking for metview 
	    	    if ( crs_ != "EPSG:4326" ) {
	    	    	MagLog::warning() << " Magics canot handle crs:" << crs_ << endl;
	    	    	return;
	    	    }
	    	    
	    
	    
	    
	}
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const 
	 	{ out << "OverlayPlot["; ImportPlotAttributes::print(out); out << "]";  }
 
private:
    //! Copy constructor - No copy allowed
	OverlayPlot(const OverlayPlot&);
    //! Overloaded << operator to copy - No copy allowed
	OverlayPlot& operator=(const OverlayPlot&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const OverlayPlot& p)
		{ p.print(s); return s; }

};


class ImportPlot: public ImportPlotAttributes, public Visdef {

public:
	ImportPlot() {}
	virtual ~ImportPlot() {}
	void set(const map<string, string>& map) { ImportPlotAttributes::set(map); }

	virtual void operator()(Data& data, BasicGraphicsObjectContainer& visitor)
	{				
		ImportObject* object = new ImportObject();
	    object->setPath(data.path());
	    object->setOrigin(PaperPoint(x_, y_));
	    object->setWidth(width_);
	    object->setHeight(height_);
	    object->setFormat(format_);
	    object->setOriginReference(ImportObject::bottom_left);

	    if ( crs_.empty() ) {
	    	visitor.push_back(object);
	    	return;
	    } 
	    // here we are doing a bit of cheking for metview 
	    if ( !visitor.transformation().verifyDef(crs_) ) {
	    	MagLog::warning() << " incompatible projections ( in " << crs_  << " and out) " << crs_ << endl;
	    	return;
	    }
	    
	   
	    object->setOrigin(PaperPoint(crs_minx_, crs_miny_));
	    object->setWidth(crs_maxx_ - crs_minx_);
	    object->setHeight(crs_maxy_ - crs_miny_);
	    visitor.push_back(object);
	}
	void visit(LegendVisitor& legend) {
		legend.add(new EmptyEntry());
	}
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const 
	 	{ out << "ImportPlot["; ImportPlotAttributes::print(out); out << "]";  }
 
private:
    //! Copy constructor - No copy allowed
	ImportPlot(const ImportPlot&);
    //! Overloaded << operator to copy - No copy allowed
	ImportPlot& operator=(const ImportPlot&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImportPlot& p)
		{ p.print(s); return s; }

};


} // namespace magics


#endif
