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

/*! \file CellShading.cc
    \brief Implementation of the Template class CellShading.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 30-Aug-2005
    
    Changes:
    
*/

#include "CellShading.h"
#include "Image.h"
#include "IsoPlot.h"
#include "Symbol.h"
#include <algorithm>
#include <limits>

using namespace magics;


CellShading::CellShading()
{
}


CellShading::~CellShading()
{
}


void CellShading::operator()( MatrixHandler& data, BasicGraphicsObjectContainer& parent)
{
	// Here we have to work using the parentput projection.
	
	const Transformation& projection = parent.transformation();
	
	double minc = projection.getMinPCX();
	double maxc = projection.getMaxPCX();
	double minr = projection.getMinPCY();
	double maxr = projection.getMaxPCY();
	
	double width =  parent.absoluteWidth();
	double height =  parent.absoluteHeight();
	
	MagLog::debug() << "minx="   << minc << endl;
	MagLog::debug() << "maxx="   << maxc << endl;
	MagLog::debug() << "miny="   << minr << endl;
	MagLog::debug() << "maxy="   << maxr << endl;
	MagLog::debug() << "width="  << width << endl;
	MagLog::debug() << "height=" << height << endl;
	MagLog::debug() << "resolution=" << resolution_ << endl;
	
	int rows    = int(height * resolution_);
	int columns = int(width * resolution_);
	double stepr = (maxr-minr)/(rows-1);
	double stepc = (maxc-minc)/(columns-1);
	MagLog::debug() << "stepy=" << stepr << endl;
	MagLog::debug() << "stepx=" << stepc << endl;
	MagLog::debug() << "rows=" << rows << endl;
	MagLog::debug() << "columns=" << columns << endl;
	
	UserPoint point;
	double value;
	
	Image* image = new Image();
	image->set(rows, columns);
	
	double lat = maxr;
	double lon = minc;
	for ( int row = 0; row < rows; row++) {
		lon = minc;
		for ( int column = 0; column < columns; column++) {
			projection.revert(PaperPoint(lon, lat), point);			
			value = (magCompare(method_, "nearest") ) ?
					data.nearest(point.y(), point.x()) :  data.interpolate(point.y(), point.x());
		
			image->push_back(map_.find(value,0));
			lon += stepc;
		}
		lat -= stepr;
	}
	
	
    ColourTable table;
    vector<Colour>::const_iterator colour = colours_.begin();
    for (int i = 0; i <= *max_element(image->begin(), image->end()); i++)
    {
    	table.push_back(*colour);
    	if ( ++colour == colours_.end() ) colour = colours_.begin();
	}
	

	PaperPoint pp(minc, maxr);
	image->setOrigin(pp);
	MagLog::debug() << "origin--->" << pp << endl;
	image->setWidth(maxc-minc);
	image->setHeight(maxr-minr);
	image->setColourTable(table);

	
	parent.push_back(image);
}
DumpShading::DumpShading()
{
}


DumpShading::~DumpShading()
{
}

void DumpShading::operator()( MatrixHandler& data, BasicGraphicsObjectContainer& parent)
{
	// Here we have to work using the parentput projection.

	const Transformation& projection = parent.transformation();

	double minc = projection.getMinPCX();
	double maxc = projection.getMaxPCX();
	double minr = projection.getMinPCY();
	double maxr = projection.getMaxPCY();

	Image* image = new Image();
	image->set(data.rows(), data.columns());



	for ( int row = 0; row < data.rows(); row++) {

		for ( int column = 0; column < data.columns(); column++) {

			image->push_back(map_.find(data(row, column),0));

		}

	}


    ColourTable table;
    vector<Colour>::const_iterator colour = colours_.begin();
    for (int i = 0; i <= *max_element(image->begin(), image->end()); i++)
    {
    	table.push_back(*colour);
    	if ( ++colour == colours_.end() ) colour = colours_.begin();
	}


	PaperPoint pp(minc, maxr);
	image->setOrigin(pp);
	MagLog::debug() << "origin--->" << pp << endl;

	image->setColourTable(table);
	image->setWidth(maxc-minc);
	image->setHeight(maxr-minr);

	parent.push_back(image);
}

bool CellShading::prepare(const LevelSelection& levels, const ColourTechnique& technique)
{
	// First Interval ...
	
	map_.clear();
	colours_.clear();
	map_[Interval(INT_MIN, levels.front())] = 0;
	colours_.push_back(Colour("none"));
	for (unsigned int i = 0; i < levels.size() -1; i++)
	{  
		map_[Interval(levels[i], levels[i+1])] = i+1;
		colours_.push_back(technique.right(levels[i]));
	}
	map_[Interval(levels.back(), std::numeric_limits<double>::max())] = levels.size();
	colours_.push_back(Colour("none"));
	return false;
}


void  CellShading::visit(LegendVisitor& node, const ColourTechnique&)
{
	
	for ( IntervalMap<int>::const_iterator interval = map_.begin(); interval != map_.end(); ++interval) {
	   
	   Polyline* box = new Polyline();
			      
	   double min = interval->first.min_;
	   double max = interval->first.max_;
	   // We ignore the first and the last entries: no interest in the legend!  
	   if (interval->second == 0) continue;
	   if (interval->second == int(map_.size()-1)) continue;
	   
	   
	   box->setFilled(true);
	   box->setFillColour(colours_[interval->second]);
	   		
	   FillShadingProperties* shading = new FillShadingProperties();
	    	    
	  
	          
	   box->setShading(shading);


	   
	   node.add(new BoxEntry(min, max, box));	        
	}

	node.last();

}


/*!
 Class information are given to the parentput-stream.
*/	

void CellShading::print(ostream& out)  const
{
	out << "CellShading";
}




void  CellShading::colour(double val, Colour& colour)
{
	colour = this->colours_[this->map_.find(val, 0)];
}
CellArray* CellShading::array(MatrixHandler& matrix, IntervalMap<int>& range,
		const Transformation& transformation, int width, int height,
		float resolution, const string& technique)
{
	return new CellArray(matrix, range, transformation, width, height, resolution, technique);

}
