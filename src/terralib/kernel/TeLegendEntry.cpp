/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
#include <TeLegendEntry.h>

TeLegendEntry::TeLegendEntry(const TeSlice& slice) :
	slice_(slice),
	id_(-1),
	theme_(0),
	group_(0)
{
	if (slice_.from_ == "Missing Data")
		label_ = "Missing Data";
	else 
	{
		int i = slice_.from_.find("mean = ");
		if (i >= 0 || slice_.to_.empty())
			label_ = slice_.from_;
		else
			label_ = slice_.from_ + " ~ " + slice_.to_;
	}
}


TeLegendEntry::TeLegendEntry (const TeLegendEntry& leg)
{
	*this = leg;
}

TeLegendEntry&
TeLegendEntry::operator = (const TeLegendEntry& leg)
{
	if ( this != &leg )
	{	//clears the current map
		TeGeomRepVisualMap::iterator currMapIt = visualMap_.begin();
		while( currMapIt != visualMap_.end() )
		{
			if (currMapIt->second)
			{
				delete currMapIt->second;
				currMapIt->second = NULL;
			}				
			++currMapIt;
		}
		visualMap_.clear();
			
		//copy the new map
		TeGeomRepVisualMap::const_iterator it = leg.visualMap_.begin();
		while( it != leg.visualMap_.end() )
		{
			if (it->second)
			{
				TeVisual* visual = it->second->copy();
				visualMap_[ it->first ] = visual;
			}
			++it;
		}
		slice_ = leg.slice_;
		label_ = leg.label_;
		id_ = leg.id_;
		theme_ = leg.theme_;
		group_ = leg.group_;
	}
	return *this;
}

void
TeLegendEntry::clear()
{
	slice_.from_.clear();
	slice_.to_.clear();
	label_.clear();

	//clears the current map
	TeGeomRepVisualMap::iterator currMapIt = visualMap_.begin();
	while( currMapIt != visualMap_.end() )
	{
		if (currMapIt->second)
		{
			delete currMapIt->second;
			currMapIt->second = NULL;
		}
		++currMapIt;
	}
	visualMap_.clear();
}

string 
TeLegendEntry::label()
{
    if (label_.empty())
    {
        int npos = slice_.from_.find("mean = ");
        if (npos >= 0 || slice_.to_.empty())
            label_ = slice_.from_;
        else 
            label_ = slice_.from_ + " ~ " + slice_.to_;
    }
    return label_;
} 

TeVisual*
TeLegendEntry::visual(TeGeomRep rep, const string& visualType)
{ 
	if(visualMap_.find(rep) == visualMap_.end())
	{
		TeVisual* visual = TeVisualFactory::make(visualType);
        visualMap_[rep] = visual; 
	}
	return visualMap_[rep];
}

void 
TeLegendEntry::color( TeColor& color )
{
	if(visualMap_.find(TePOLYGONS) != visualMap_.end())
	{
		TeVisual* tmpVisual = visualMap_[TePOLYGONS];
		if (tmpVisual)
		{
			tmpVisual->color( color );
			if ( tmpVisual->style() != 1 ) 
				tmpVisual->contourColor( color );
		}		
	}
	if(visualMap_.find(TeLINES) != visualMap_.end())
	{
		TeVisual* tmpVisual = visualMap_[TeLINES];
		if (tmpVisual)
			tmpVisual->color( color );
	}
	if(visualMap_.find(TePOINTS) != visualMap_.end())	
	{
		TeVisual* tmpVisual = visualMap_[TePOINTS];
		if (tmpVisual)
			tmpVisual->color( color );
	}
}

void 
TeLegendEntry::setVisual(TeVisual* vis, TeGeomRep rep) 
{ 
	if ( visualMap_.find(rep) != visualMap_.end() )
	{
		if ((visualMap_[rep] != NULL) && (visualMap_[rep] != vis))
			delete visualMap_[rep];
	}	
	visualMap_[rep] = vis; 
}

void 
TeLegendEntry::setVisual(TeVisual& vis, TeGeomRep rep) 
{
	return (setVisual(vis.copy(), rep));
}
