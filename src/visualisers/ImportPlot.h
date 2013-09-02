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
