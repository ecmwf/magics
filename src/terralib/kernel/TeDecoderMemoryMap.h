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
/*! \file TeDecoderMemoryMap.h
    \brief This file deals with decoding of raster data in a bynary file, using the memory map operation system functionality
*/
#ifndef  __TERRALIB_INTERNAL_DECODERMEMORYMAP_H
#define  __TERRALIB_INTERNAL_DECODERMEMORYMAP_H

#include "TeDecoder.h"

#ifdef WIN32
#include <windows.h>
#include <winbase.h>
#else
#include <sys/mman.h>
#endif

 
//! Implements a decoder to raster data in a bynary file, using the memory map operation system functionality
/*
	Due to limits set by the operating system, the maximum amount of data you can map 
	with a single instance of a memory map is 2^31 - 1 (or 2 GB).
*/
class TL_DLL TeDecoderMemoryMap : public TeDecoder
{
public:
	//! Constructor
	TeDecoderMemoryMap ( const TeRasterParams& );

	//! Destructor
	~TeDecoderMemoryMap ();

	//! Reads an element
	bool setElement (int col,int lin, double val, int band=0);

	//! Writes an element
	bool getElement (int col,int lin, double &val,int band=0);

	//! Initalizes the internal structures
	virtual void init();

	//! Releases the internal structures
	virtual bool clear();

private:
	long	dataInitPos_;
#ifdef WIN32
// Windows support to file memory mapping
	HANDLE  m_hFile;			// Handle to file we're currently zapping
	HANDLE	m_hMapping;			// Handle to memory-mapping of that file
	LPVOID  m_lpszFile;			// Pointer to view of file mapped to memory
	DWORD	m_dwSize;
#else
// Linux support to file memory mapping
	int	m_hFile;			//(fd) Handle to file we're currently zapping
	void*	m_lpszFile;			// Pointer to view of file mapped to memory
	long	m_dwSize;
#endif
};

//! Implements a factory to build memory map decoder
class TL_DLL TeDecoderMemoryMapFactory : public TeDecoderFactory
{
public:

	//! Factory constructor
	TeDecoderMemoryMapFactory(const string& name);

	//! Builds the object
	virtual TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderMemoryMap(arg); }
};
#endif

