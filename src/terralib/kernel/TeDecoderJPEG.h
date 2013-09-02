/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2004 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeDecoderJPEG.h
    \brief This file deals with decoding of raster structures in JPEG format
*/
#ifndef  __TERRALIB_INTERNAL_DECODERJPEG_H
#define  __TERRALIB_INTERNAL_DECODERJPEG_H

#include "TeDecoder.h"

//! Implements a decoder for images in JPEG format
/*  
	This implementation compress/decompress the entire JPEG image in memory and  
	access its pixels acessed directly. \par
	The default file extensions associated to this decoder are ".jpg" and ".jpeg".
*/
class TL_DLL TeDecoderJPEG: public TeDecoder
{
public:
	//! Empty constructor
	TeDecoderJPEG();

	//! Constructor from some parameters
	TeDecoderJPEG(const TeRasterParams& par);

	//! Destructor
	~TeDecoderJPEG();

	//! Initializes the internal structures
	void init();

	//! Releases the internal structures
	bool clear();

	//! Writes an element
	bool setElement(int col,int lin, double val, int band=0);

	//! Reads an element
	bool getElement(int col,int lin, double &val,int band=0);

private:
	bool isModified_;
	long size_;
	unsigned char* myData_;
};

//! Implements a factory to build decoders to JPEG raster
class TL_DLL TeDecoderJPEGFactory : public TeDecoderFactory
{
public:

	//! Constructor for the factory
	TeDecoderJPEGFactory(const string& name);

	//! Built the object
	TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderJPEG(arg); }
};

#endif

