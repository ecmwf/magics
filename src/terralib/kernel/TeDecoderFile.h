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
/*! \file TeDecoderFile.h
    \brief This file deals with decoding of raster structures in a binary file using Windows support
*/
#ifndef  __TERRALIB_INTERNAL_DECODERFILE_H
#define  __TERRALIB_INTERNAL_DECODERFILE_H

#ifdef WIN32
#include "TeDecoderVirtualMemory.h"
#include <windows.h>
#include <winbase.h>

//! Implements decoder to raster data stored in a file in a binary raw format
/*	\note This implementation is valid only for the windows platform
*/
class TL_DLL TeDecoderFile : public TeDecoderVirtualMemory
{
public:
	//! Constructor
	TeDecoderFile ( const TeRasterParams& );

	//! Destructor
	~TeDecoderFile ();

	//! Initalizes its internal structures
	void init	();

	//! Created a handle
	bool create	();

	//! Releases the internal structures
	bool clear	();

protected:
	bool	getRasterBlock(const TeBlockIndex& index, void *buf); 
	bool	putRasterBlock(const TeBlockIndex& index, void *buf, long bsize);
	TeBlockIndex blockIndex(int col, int lin, int band);
	void blockIndexPos(const TeBlockIndex& index, int& ulCol, int& ulLin, int& band);
	
private:

	unsigned long	dataInitPos_;
	HANDLE  m_hFile;			// Handle to file we're currently zapping
	LPVOID  m_buffer;			// Pointer to view of file mapped to memory
	DWORD	m_dwSize;			// File size in bytes
	DWORD	m_dwPosition;		// Current file pointer position
	DWORD	m_dwWordSize;		// word size in bytes
};

//! Implements a factory to build file decoders
class TL_DLL TeDecoderFileFactory : public TeDecoderFactory
{
public:

	TeDecoderFileFactory(const string& name) : TeDecoderFactory(name) {}

	//! Builds a file decoder
	virtual TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderFile(arg); }
};

#endif
#endif
