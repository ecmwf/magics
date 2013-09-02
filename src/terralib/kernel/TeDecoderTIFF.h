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
/*! \file TeDecoderTIFF.h
    \brief This file contais functions to deal with raster images in Tiff/GeoTIFF format.
*/

#ifndef  __TERRALIB_INTERNAL_DECODERTIFF_H
#define  __TERRALIB_INTERNAL_DECODERTIFF_H

#include "TeDecoder.h"
#include "TeDecoderSmartMem.h"

// LibTIFF includes
#include "geotiff.h"
#include "xtiffio.h"


//! Implements a decoder to a raster in TIFF (Tagged Image File Format) format with or without geographical tags (geotiff)
/*!
	Decoder TIFF is based on geotiff libray (http://remotesensing.org/geotiff/geotiff.html).
	This imposes some restrictions to this decoder. Using libtiff TIFF image files may not 
	be opened for both reading and writing access, or, there is no support for altering the contents of 
	a TIFF file. So this decoder is limited to create new tiff files or to read existing ones. \par
	The default file extensions associated to this decoder are ".tif" or ".tiff". \par
	\note This decoder doesn't support tiff files that are organizes in tiles and in separated planes.
*/
class TL_DLL TeDecoderTIFF: public TeDecoder
{
	TIFF 	*tif;       		// TIFF directory structure
	GTIF	*gtif;			// GEOTIFF directory structure

	unsigned char** dataBuffer_;	// buffer to hold decoded data
					// for tile data it holds 1 tile
					// for stripped data it holds 1 strip with all the planes

	unsigned int	nplanes_;	// number of planes or logical division of the data within the files

	bool		isGeoTiff_;	// indicates if the data has geographical information
	short 		TImage_;	// 0=MONOIMAGE, 1=RGBIMAGE, 2=PALLETE 
	unsigned short	planar_;	// storage organization : PLANARCONFIG_CONTIG or PLANARCONFIG_SEPARATE
	
	bool		isTiled_;	// indicates if the data is organizes in tiles 
	unsigned long	tilew_,tileh_;	// tile and height
	unsigned long	tilesacross_;	// number of tiles to compose a row of image
	unsigned long	bytespertile_;	// number of bytes per tile
	long		TCurTile_;	// current tile decoded in internal memory

	unsigned long	rowsperstrip_;	// number of rows per strip
	unsigned long	nstripsperplane_;	// number of strips per plane of data
	unsigned long	stripsize_;	// number of bytes per strip
	long		TCurStrip_;	// current line in memory
	
	unsigned int	nBands_;		// number of samples per data

	// This is an internal smart manager to support the creation of tiffs 
	TeDecoderSmartMem memManager_;
    
	//	Reads TIFF Directory and fills tif structure
	void readTiffDirectory();

	// Reads the georeferencing keys of a geotiff data
	bool getGeoTIFF();

	// Sets the georeferencing keys of a geotiff data
	void setGeoKeys();

	// Reads LUT information
	bool readLut();

	// Saves LUT information
	void saveLut();

	// Reads enough tiles to compose a row of data
	bool readTileImageContig(unsigned long tiler);

	// Reads a strip
	bool readStrip(unsigned long strip);

	// Allocates te internal memory
	bool allocateWorkMemory();

	//! Type definition for the getElement function pointer      
	typedef void (TeDecoderTIFF::*GetEleFunctPtrT)( const long& plane, const long& pos, double& val );

	//! A pointer to the current getElement method following the current data type
	GetEleFunctPtrT getelement_ptr_;

	//! A casted pointer to the current data
	unsigned char** data_TeUNSIGNEDCHAR_;  

	//! A casted pointer to the current data
	char** data_TeCHAR_;  

	//! A casted pointer to the current data
	unsigned short** data_TeUNSIGNEDSHORT_;  

	//! A casted pointer to the current data
	short** data_TeSHORT_;  

	//! A casted pointer to the current data
	int** data_TeINTEGER_;  

	//! A casted pointer to the current data
	unsigned long** data_TeUNSIGNEDLONG_;  

	//! A casted pointer to the current data
	long** data_TeLONG_;  

	//! A casted pointer to the current data
	float** data_TeFLOAT_;

	//! A casted pointer to the current data
	double** data_TeDOUBLE_;  

	inline void getElement_TeUNSIGNEDCHAR(const long& plane, const long& pos, double& val );  

	inline void getElement_TeCHAR(const long& plane, const long& pos, double& val );  

	inline void getElement_TeUNSIGNEDSHORT(const long& plane, const long& pos, double& val );

	inline void getElement_TeSHORT(const long& plane, const long& pos, double& val );

	inline void getElement_TeINTEGER(const long& plane, const long& pos, double& val );

	inline void getElement_TeUNSIGNEDLONG( const long& plane, const long& pos, double& val );

	inline void getElement_TeLONG(const long& plane, const long& pos, double& val );

	inline void getElement_TeFLOAT(const long& plane, const long& pos, double& val );

	inline void getElement_TeDOUBLE(const long& plane, const long& pos, double& val );  

public:

	//! Construtor from parameters 
	TeDecoderTIFF( const TeRasterParams& );

	//!	Normal destructor
	~TeDecoderTIFF();

	//! Initializes the internal structures
	void init();

	//! Clear internal structures
	bool clear();

	//! Reads an element
	bool setElement (int col,int lin, double val, int band=0);

	//! Writes an element
	bool getElement (int col,int lin, double &val,int band=0);
};

//! Implements a tiff decoder factory
class TL_DLL TeDecoderTIFFFactory : public TeDecoderFactory
{
public:

	TeDecoderTIFFFactory(const string& name);

	TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderTIFF(arg); }
};

#endif
