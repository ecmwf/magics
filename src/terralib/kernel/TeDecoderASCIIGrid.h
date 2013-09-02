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
/*! \file TeDecoderASCIIGrid.h
    \brief This file deals with decoding of raster data in a ASCII SPRING format
*/
#ifndef  __TERRALIB_INTERNAL_DECODERASCIIGrid_H
#define  __TERRALIB_INTERNAL_DECODERASCIIGrid_H

#include "TeDecoder.h"

class TeAsciiFile;

//! Implements a decoder for rasters in ESRI, Inc.'s ASCII Grid format
/*!
	The default file extension associated to this decoder ".grd", ".asc" and ".txt".
*/
class TL_DLL TeDecoderASCIIGrid : public TeDecoder
{
public:
	//! Empty constructor
	TeDecoderASCIIGrid();

	//! Constructor from some parameters
	TeDecoderASCIIGrid(const TeRasterParams& par);

	//! Destructor
	~TeDecoderASCIIGrid();

	//! Initializes the internal structures of the decoder.
	void init();

	//! Clears its internal structures.
	bool clear();

	//! Writes an element
	bool setElement(int col,int lin, double val, int band=0);

	//! Reads an element
	bool getElement(int col,int lin, double &val,int band=0);

private:

	bool isModified_;
	long size_;
	float* myData_;

	//! Reads the data from a raster file.
    bool readFile(const string& filename);

	//! Reads raster file header information.
	bool readParameters();

	//! Writes header information to raster file.
	bool writeParameters(TeAsciiFile& pFile);

	//! Saves data to raster file.
	bool saveData(TeAsciiFile& pFile);

};

//! Implements a factory to build decoders for ESRI's ASCII Grid raster format
class TL_DLL TeDecoderASCIIGridFactory : public TeDecoderFactory
{
public:
	//! Constructor
	TeDecoderASCIIGridFactory(const string& name);

	//! Implementation for the abstract TeFactory::build.
	/*!
	    \param arg A const reference to the used parameters.
		\return A pointer to the new generated instance.
    */
	TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderASCIIGrid(arg); }

};
#endif
