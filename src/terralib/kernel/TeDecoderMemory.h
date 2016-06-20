/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
/*! \file TeDecoderMemory.h
    \brief This file deals with decoding of raster structures as a mulitdimensional matrix in memory.
*/
#ifndef  __TERRALIB_INTERNAL_DECODERMEMORY_H
#define  __TERRALIB_INTERNAL_DECODERMEMORY_H

#include "TeDecoder.h"

//! Implements a decoder to a raster stored as a as a mulitdimensional matrix in memory
/*
	This classes tries to allocate the matrix in the memory RAM. No strategies or policies
	to allocate huge matrices are applied. It also doesn't support multiple bands with different types.
	For overcome this limitations use the class TeDecoderSmartMem.
	\sa TeDecoderSmartMem
*/
class TL_DLL TeDecoderMemory : public TeDecoder
{
public:
	//! Empty constructor
	TeDecoderMemory();

	//! Constructor from parameters
	TeDecoderMemory (const TeRasterParams& par);

	//! Destructor
	~TeDecoderMemory ();

	//! Writes an element
	bool setElement (int col,int lin, double val, int band=0);

	//! Reads an element
	bool getElement (int col,int lin, double &val,int band=0);

	//! Retuns a pointer to a given band
	/*
		\note This method is applicable only when band interleaved organization
	*/
	void* data (int band=0);

	//! Initalizes the internal structures
	void init();

	//! Releases the internal structures
	bool clear();

	//! Allocates the necessary memory
	bool allocateMemory();

	//! Deallocates the used memory
	bool resetMemory();

private:

	unsigned int nelems_;
	unsigned int size_;

	//! Type definition for the getElement function pointer      
	typedef void (TeDecoderMemory::*GetEleFunctPtrT)( const long& pos, double& val );

	//! Type definition for the setElement function pointer     
	typedef void (TeDecoderMemory::*SetEleFunctPtrT)( const long& pos, const double& val );

	//! A pointer to the current getElement method following the current data type
	GetEleFunctPtrT getelement_ptr_;

	//! A pointer to the current setElement method following the current data
	SetEleFunctPtrT setelement_ptr_;

	//! A casted pointer to the current data
	unsigned char* data_TeUNSIGNEDCHAR_;  

	//! A casted pointer to the current data
	char* data_TeCHAR_;  

	//! A casted pointer to the current data
	unsigned short* data_TeUNSIGNEDSHORT_;  

	//! A casted pointer to the current data
	short* data_TeSHORT_;  

	//! A casted pointer to the current data
	int* data_TeINTEGER_;  

	//! A casted pointer to the current data
	unsigned long* data_TeUNSIGNEDLONG_;  

	//! A casted pointer to the current data
	long* data_TeLONG_;  

	//! A casted pointer to the current data
	float* data_TeFLOAT_;

	//! A casted pointer to the current data
	double* data_TeDOUBLE_;  

	//! Update the function pointer using the current raster parameters   
	void updateFuncPtr();  

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/      
	inline void setElement_TeUNSIGNEDCHAR( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void setElement_TeCHAR( const long& pos, const double& val );  

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void setElement_TeUNSIGNEDSHORT( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void setElement_TeSHORT( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void setElement_TeINTEGER( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void setElement_TeUNSIGNEDLONG( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/      
	inline void setElement_TeLONG( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/     
	inline void setElement_TeFLOAT( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/    
	inline void setElement_TeDOUBLE( const long& pos, const double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/     
	inline void getElement_TeUNSIGNEDCHAR( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/  
	inline void getElement_TeCHAR( const long& pos, double& val );  

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/  
	inline void getElement_TeUNSIGNEDSHORT( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/   
	inline void getElement_TeSHORT( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/    
	inline void getElement_TeINTEGER( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/      
	inline void getElement_TeUNSIGNEDLONG( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/      
	inline void getElement_TeLONG( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/      
	inline void getElement_TeFLOAT( const long& pos, double& val );

	/*! \brief Method overload.
		\param line Line.
		\param col Column.
		\param band Band.
		\param val Pixel value.
	*/
	inline void getElement_TeDOUBLE( const long& pos, double& val );  
};

//! Implements a factory to build decoder to MEMORY raster
class TL_DLL TeDecoderMemoryFactory : public TeDecoderFactory
{
public:

	//! Factory constructor
	TeDecoderMemoryFactory(const string& name) : TeDecoderFactory(name) {}

	//! Build an object
	TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderMemory(arg); }
};
#endif

