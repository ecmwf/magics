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
/*! \file TeImportRaster.h
    \brief This file contains functions to support importing of raster data into a TerraLib database
*/
#ifndef __TERRALIB_INTERNAL_IMPORTRASTER_H
#define __TERRALIB_INTERNAL_IMPORTRASTER_H

#include "TeDataTypes.h"
#include "TeRasterParams.h"

#include <string>
using namespace std;

class TeLayer;
class TeRaster;
/** @defgroup RasterFunctions Functions to deal with raster data
 *  @{
*/
//! Imports a TeRaster object to a database creationg a new TeLayer
/*!
	This function imports a raster to the database, generating a new layer.
      \param layerName name of the layer to be created
      \param rasterIn a pointer to the raster representation to be imported
      \param database a pointer to the database where the raster will be imported
	  \returns a pointer to the generated layer
*/
TL_DLL TeLayer* TeImportRaster (const string& layerName, TeRaster* rasterIn, TeDatabase* database);

//! Imports a TeRaster object to a TeLayer
/*!
	This function imports a raster as a geometry of an object of a layer.
      \param layer pointer to a layer already created
      \param rasterIn a raster representation
	  \param bWidth width of the blocks used to store the raster
	  \param bHeight height of the blocks used to store the raster
	  \param compress compression type
	  \param objectId object identification associated to this raster
	  \param dummy value to be used as a no data value when importing
	  \param useDummy flag used to indicate that imported raster will have a dummy value
	  \param indext type of tiling used to import raster
	  \returns true or false whether the raster was imported successfully
*/
TL_DLL bool TeImportRaster (TeLayer* layer, TeRaster* rasterIn, unsigned int bWidth=512, unsigned int bHeight=512, 
		TeRasterParams::TeRasterCompressionMode compress = TeRasterParams::TeNoCompression, 
		const string& objectId="", double dummy=255,bool useDummy = false, 
		TeRasterParams::TeRasterTilingType indext=TeRasterParams::TeNoExpansible);

//! This function mosaics an input raster to one previsously existing in a TerraLib layer
/*!
     \param rasterIn a raster representation
     \param layer pointer to an existing layer 
	 \param objectId object identification associated to this raster
	 \returns true or false whether the mosaic operation was successfull
*/
TL_DLL bool TeMosaicRaster(TeRaster* rasterIn, TeLayer* layer,  const string& objectId="");

//! Builds a degraded resolution of a raster data
/*!
      \param layer pointer to a layer already created 
      \param rasterIn a raster representation
	  \param resFac  value to multiply the original resolution
	  \param objectId object identification associated to this raster
	  \returns true or false whether the raster was imported successfully
*/
TL_DLL bool TeBuildLowerResolution(TeLayer* layer, TeRaster* rasterIn, int resFac, const string& objectId="");

//! Builds the multi resolution pyramid for a given raster
TL_DLL bool TeBuildMultiResolutionPyramid(TeRaster* rst, int nLevels);
/** @} */ 

/** \example importJPEG.cpp
	Shows how to import a JPEG file
 */

/** \example importGridData.cpp
	Shows how a raster data: a binary raw grid data
 */

/** \example mosaciTIFFImages.cpp
 * This is an example of how to build a mosaic of images as a layer in a TerraLib database
 */

#endif

