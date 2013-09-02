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

#include "TeRasterTransform.h"
#include "TeVisual.h"

void 
TeRasterTransform::generateLUT(TeLegendEntryVector& legs, unsigned int nentries, TeColor& backColor)
{
	// find the range covered by the legends
	unsigned int n;
	double vmin = TeMAXFLOAT;
	double vmax = TeMINFLOAT;
	for (n=0; n <legs.size(); n++)
	{
		if (atof(legs[n].slice().from_.c_str()) < vmin)
			vmin = atof(legs[n].slice().from_.c_str());
		if (atof(legs[n].slice().to_.c_str()) > vmax)
			vmax = atof(legs[n].slice().to_.c_str());
	}
	// do a linear transformation from the range defined by the legend to the number of entries
	this->setLinearTransfParameters(vmin,vmax, 0, nentries-1);

	// initialize lut with the background color
	lutr_.clear();
	lutg_.clear();
	lutb_.clear();

	lutr_.insert(lutr_.begin(),nentries,backColor.red_);
	lutg_.insert(lutg_.begin(),nentries,backColor.green_);
	lutb_.insert(lutb_.begin(),nentries,backColor.blue_);

	// to each entry define its equivalent range of indexes on the LUT
	int nunIndx = 0;
	int indexFrom, indexTo;
	for (n=0; n <legs.size(); n++)
	{
		// find the range of entries that
		indexFrom = (int)(atof(legs[n].slice().from_.c_str()) * gain_ + offset_);
		if (indexFrom < 0)
			indexFrom = 0;
		indexTo = (int)(atof(legs[n].slice().to_.c_str()) * gain_ + offset_);
		if (indexTo > (int)nentries)
			indexTo = nentries;
		nunIndx = indexTo - indexFrom + 1;
		if (nunIndx >= 1)
		{
			fill_n(&lutr_[indexFrom],nunIndx,legs[n].getVisualMap()[TePOLYGONS]->color().red_);
			fill_n(&lutg_[indexFrom],nunIndx,legs[n].getVisualMap()[TePOLYGONS]->color().green_);
			fill_n(&lutb_[indexFrom],nunIndx,legs[n].getVisualMap()[TePOLYGONS]->color().blue_);
		}
	}
	lutSize_ = lutr_.size();
}

TeRasterTransform::TeRasterTransfFunctions TeRasterTransform::getTransfFunction()
{
	if (transfFuncPtr_ == 0)
		return TeNoTransf;

	if (transfFuncPtr_ == &TeRasterTransform::Mono2ThreeBand)
		return TeMono2Three;

	if (transfFuncPtr_ == &TeRasterTransform::Band2Band)
		return TeBand2Band;

	if (transfFuncPtr_ == &TeRasterTransform::Pallete2ThreeBand)
		return TePall2Three;

	if (transfFuncPtr_ == &TeRasterTransform::LUT2ThreeBand)
		return TeLUT2Three;

	if (transfFuncPtr_ == &TeRasterTransform::ExtractRGB)
		return TeExtractRGB;

	if (transfFuncPtr_ == &TeRasterTransform::ExtractBand)
		return TeExtractBand;

	if (transfFuncPtr_ == &TeRasterTransform::ExtractBands)
		return TeExtractBands;

	if (transfFuncPtr_ == &TeRasterTransform::ThreeBand2RGB)
		return TeThreeBand2RGB;
	else
		return TeNoTransf;
}

void TeRasterTransform::setTransfFunction(TeRasterTransfFunctions func)
{
	if (func == TeMono2Three)
		transfFuncPtr_ = &TeRasterTransform::Mono2ThreeBand;

	else if (func == TeBand2Band)
		transfFuncPtr_ = &TeRasterTransform::Band2Band;

	else if (func == TePall2Three)
		transfFuncPtr_ = &TeRasterTransform::Pallete2ThreeBand; 

	else if (func == TeLUT2Three)
		transfFuncPtr_ = &TeRasterTransform::LUT2ThreeBand;
		
	else if (func == TeExtractRGB)
		transfFuncPtr_ = &TeRasterTransform::ExtractRGB;

	else if (func == TeExtractBand)
		transfFuncPtr_ = &TeRasterTransform::ExtractBand;

	else if (func == TeExtractBands)
		transfFuncPtr_ = &TeRasterTransform::ExtractBands;

	else if (func == TeThreeBand2RGB)
		transfFuncPtr_ = &TeRasterTransform::ThreeBand2RGB;

	else
		transfFuncPtr_ = 0;
}
