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

/*! \file ColourTable.cc
    \brief Implementation of the Template class ColourTable.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/



#include "ColourTable.h"
#include "PaperPoint.h"
#include "Polyline.h"

using namespace magics;

ColourTable::ColourTable() 
{
}


ColourTable::~ColourTable() 
{
}

void ColourTable::prepare() 
{
#if 0  
	for ( int i = 1; i < 256; i++) 
		push_back(Rgb(1./i, 1./i, 1./i)); 
#else
		push_back(Colour(1., 0., 0.)); 
		push_back(Colour(0., 1., 0)); 
		push_back(Colour(0., 0., 1.)); 
		push_back(Colour(1., 1., 0.)); 
		push_back(Colour(1., 0., 1.)); 
		push_back(Colour(0., 1., 1.)); 
#endif
}
/*!
 Class information are given to the output-stream.
*/		
void ColourTable::print(ostream& out)  const
{
	out << "ColourTable[\n";
	int i = 0;
	for ( ColourIterator colour = begin(); colour != end(); ++colour ) {
		out << "\t Colour " << i << ":" << *colour << "\n";
		i++; 
	}
	out << "]\n";
}
/*
void ColourTable::visit(LegendEntryList& list) const
{
	for ( ColourIterator colour = begin(); colour != end(); ++colour ) 
		colour->visit(list);
}

void ColourTableEntry::visit(LegendEntryList& list) const
{
	
	Polyline*  box = new Polyline(0, "legend");
	FillShadingProperties* shading = new FillShadingProperties();    
	box->setColour(colour_);
	
	shading->left_      = colour_;
	shading->right_     = colour_;
	shading->inColour_ = colour_;
	shading->outColour_ = Colour("NONE");    
	 
	box->setShading(shading);
	list.push_back(new BoxEntry(min_, max_, box));
	
}
*/
void ColourTableEntry::print(ostream& out)  const
{
	out << "[" << min_ << ", " << max_ << ", " << colour_ << "]";
}





