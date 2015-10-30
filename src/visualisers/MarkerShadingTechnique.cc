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

/*! \file MarkerShadingTechnique.cc
    \brief Implementation of the Template class MarkerShadingTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 26-Aug-2004
    
    Changes:
    
*/

#include "MarkerShadingTechnique.h"
#include "LevelSelection.h"
#include "MatrixHandler.h"
#include "Symbol.h"
#include "LegendVisitor.h"
#include "IsoPlot.h"
using namespace magics;



MarkerShadingTechnique::MarkerShadingTechnique() 
{
}

MarkerShadingTechnique::~MarkerShadingTechnique() 
{
}

Symbol* MarkerShadingTechnique::operator()(double val)
{
     
    for (map<Interval, Symbol* >::const_iterator interval = map_.begin(); interval != map_.end(); ++interval)
    {
            if (interval->first.between(val)) { 
                
                return interval->second;
            }
    }
    return 0;
}

void MarkerShadingTechnique::operator()(const PaperPoint& point)
{
    Symbol* symbol = (*this)(point.value());
    symbol->push_back(point);
}

void MarkerShadingTechnique::operator()(MatrixHandler& data, BasicGraphicsObjectContainer& out)
{
    OriginalMatrixHandler original(data);
    int rows = original.rows();
    int columns = original.columns();
    const Transformation& transformation = out.transformation();
    
    for (int j = 0; j < rows ; j++) {
        for (int i = 0; i < columns; i++) {
            Symbol* symbol = (*this)(original(j,i));
            PaperPoint pos=transformation(UserPoint(original.column(j, i), original.row(j, i), original(j,i)));
            if ( transformation.in(pos) && symbol )
            	symbol->push_back(pos);
        } 
    }     
    
    // Now we feed the task
    
    for ( vector<BasicGraphicsObject*>::iterator object = begin(); object != end(); ++object)
    	out.push_back(*object);
    
}

void MarkerShadingTechnique::visit(LegendVisitor& legend, const ColourTechnique&)
{
       
     for (map<Interval, Symbol* >::const_iterator interval = legend_.begin(); interval != legend_.end(); ++interval)
     {
         Interval   range  = interval->first;
         Symbol* symbol = interval->second;
         Symbol* add = new Symbol();
         add->setColour(symbol->getColour());
         add->setMarker(symbol->getMarker());
         add->setHeight(symbol->getHeight());
                 
         legend.add(new SymbolEntry(range.min_, range.max_, add));
     }

}
CellArray* MarkerShadingTechnique::array(MatrixHandler& matrix, IntervalMap<int>& range,
		const Transformation& transformation, int width, int height,
		float resolution, const string& technique)
{
	return new CellArray(matrix, range, transformation, width, height, resolution, technique);

}

bool MarkerShadingTechnique::prepare(const LevelSelection& levels, const ColourTechnique& technique)
{
    if ( colour_.empty() ) {       
        technique.colours(colour_);
         if ( colour_.empty() ) 
         	colour_.push_back("blue");
    }
    if ( height_.empty() ) {
        height_.push_back(0.2);
    }

    if ( marker_.empty() ) {
        marker_.push_back(18);
    }
    if ( marker_.empty() ) {
        marker_.push_back(18);
    }

    if (symbol_.empty() ) {
    	for ( intarray::iterator marker = marker_.begin(); marker != marker_.end(); ++marker )
    		symbol_.push_back(Symbol::convert(*marker));
    }
     
    // Prepare the table ...
    stringarray::iterator colour = colour_.begin();
    doublearray::iterator height = height_.begin();
    stringarray::iterator name = symbol_.begin();


    for (unsigned int i = 0; i < levels.size() -1; i++)
    {
        
     
        Symbol* symbol =  new Symbol();
        symbol->setColour(Colour(*colour));

        symbol->setHeight(*height);
        Symbol* legend =  new Symbol();
        legend->setColour(Colour(*colour));
        legend->setSymbol(*name);
        legend->setHeight(*height);
        map_[Interval(levels[i], levels[i+1])] = symbol;
        legend_[Interval(levels[i], levels[i+1])] = legend;
        push_back(symbol);
        if ( i+1 <  levels.size() -1) {
	        if ( ++colour == colour_.end()) {
	            MagLog::warning() << "MarkerShading --> not enough colours defined!\n";
	            colour = colour_.begin();
	        }  
	        if ( ++height == height_.end())  {
	            MagLog::warning() << "MarkerShading --> not enough heights defined!\n";
	            height = height_.begin();
	        }  
	        if ( ++name == symbol_.end()) {
	            MagLog::warning() << "MarkerShading --> not enough markers defined!\n";
	            name = symbol_.begin();
	        }
        }
    }
    return false;
}

   
/*!
 Class information are given to the output-stream.
*/	
void MarkerShadingTechnique::print(ostream& out)  const
{
	out << "MarkerShadingTechnique";
}

