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
/*! \file TeDecoderSPR.h
    \brief This file deals with decoding of raster data in a ASCII SPRING format
*/
#ifndef  __TERRALIB_INTERNAL_DECODERSPR_H
#define  __TERRALIB_INTERNAL_DECODERSPR_H

#include "TeDecoderSmartMem.h"

class TeAsciiFile;

//! Implements a decoder for raster data in ASCII-SPRING format
/*! The description of this format can be found at SPRING website <http://www.dpi.inpe.br/spring>.\par
	The default file extension associated to this format is ".spr".
*/
class TL_DLL TeDecoderSPR : public TeDecoderSmartMem
{
public:
	//! Empty constructor
	TeDecoderSPR();

	//! Constructor from some parameters
	TeDecoderSPR(const TeRasterParams& par);

	//! Destructor
	~TeDecoderSPR();

	//! Intializes the internal structures
	void init();

	//! Releases the internal structures
	bool clear();

	//! Writes an element
	bool setElement (int col,int lin, double val, int band=0);

private:

	bool isModified_;

    bool readFile(const string& filename);
	bool readParameters();
	bool writeParameters(TeAsciiFile& pFile);
	bool saveData(TeAsciiFile& pFile);
};

//! Implements a factory to build decoders to ASCII-SPRING raster
class TL_DLL TeDecoderSPRFactory : public TeDecoderFactory
{
public:
	//! Factory constructor
	TeDecoderSPRFactory(const string& name);

	//! Builds the object
	TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderSPR(arg); }
};

#endif
