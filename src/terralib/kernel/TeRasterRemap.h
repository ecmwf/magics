/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeRasterRemap.h
    \brief This file contains definitions the class and functions need to transform a raster geometry into another geometry
*/
#ifndef  __TERRALIB_INTERNAL_RASTERREMAP_H
#define  __TERRALIB_INTERNAL_RASTERREMAP_H

#include "TeRaster.h"
#include "TeGeometry.h"
#include "TeRasterTransform.h"

//!  A functional class to transform a raster geometry into another geometry 
/*!  
	RasterRemap class provides functions to remap a raster strtucture into another.
	The two raster can have a different projections, different bounding boxes and/or 
	different resolutions. 
 
	\sa
     TeRaster, TeRasterParams, TeRasterTransform, TeProjection, TeBox

	\note Raster representation in TerraLib is under development, all files that deal
	with raster representation should be considered as a beta version.
*/
class TL_DLL TeRasterRemap
{
protected:
	TeRaster*	rasterIn_;		//!< input raster
	TeRaster*	rasterOut_;		//!< output raster

	TeRasterTransform* transformer_;	

	int	 interpolation_;
	bool showProgress_;


	TeBox ROI_;					// region of interest (from input) where the remmaping should be done

	void TeInterpolateIn (TeBox &box);

	bool remap ();
	bool copy ();
	bool resample ();

public:

	//! Constructor
	/*
		\param rasterIn pointer to the input raster
		\param rasterOut pointer to the output raster
		\para showProgress flag to indicate that a progress status hould be reported
	*/
	TeRasterRemap (TeRaster* rasterIn=0, TeRaster* rasterOut=0, bool showProgress = false):
		rasterIn_(rasterIn),
		rasterOut_(rasterOut),
		transformer_(0),
		interpolation_(0),
		showProgress_(showProgress),
		ROI_(TeBox())
		{}

	//! Sets the input raster
	void setInput(TeRaster* rasterIn)
	{	
		rasterIn_ = rasterIn; 
		if (transformer_)
			transformer_->setRasterIn(rasterIn_);
	}
	
	//! Sets the output raster
	void setOutput(TeRaster* rasterOut)
	{	
		rasterOut_ = rasterOut; 
		if (transformer_)
			transformer_->setRasterOut(rasterOut_);
	}
	
	//! Sets the interpolation flag
	void setInterpolation(int interpolation)
	{	interpolation_ = interpolation; }

	//! Sets the transformation to be used when doing remap
	void setTransformer(TeRasterTransform* transf)
	{	
		if (transf)
		{
			transformer_ = transf; 
			transformer_->setRasterOut(rasterOut_);
			transformer_->setRasterIn(rasterIn_);
		}
	}
	
	//! Returns the transformation to be used when doing remap
	TeRasterTransform* transformer()
	{	return transformer_;	}

	//! Applies the remaping
	bool apply(bool showProgress = false);

	//! Defines a region of interest where the remmaping should be done
	/*
		\par roi a box that represents the region of interest. It should
		be inside the raster in bounding box;
		\return false if the roi in invalid or it is not within the input
		raster. True otherwise
	*/
	bool setROI(TeBox& roi);
};

#endif

