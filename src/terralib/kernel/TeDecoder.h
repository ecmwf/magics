/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2005 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeDecoder.h
    \brief This file deals with decoding of raster structures 
*/
#ifndef  __TERRALIB_INTERNAL_DECODER_H
#define  __TERRALIB_INTERNAL_DECODER_H

#include "TeDefines.h"
#include "TeFactory.h"
#include "TeCoord2D.h"
#include "TeRasterParams.h"

using namespace std;
#include <string>
#include <list>
#include <map>

class TeRaster;
class TeDecoderMemory;
class TeProjection;
struct TeBox;

//! An abstract class to decode raster data 
/* 
	A decoder keeps its own copy of the  parameters associated
	to the raster being decoded.
*/
class TL_DLL TeDecoder {

public:
	//! Empty constructor
	TeDecoder() {}

	//! Constructor from  raster parameters
	/*!
		\param par raster parameters
	*/
	TeDecoder(const TeRasterParams& par) : params_(par) {}

	//! Virtual destructor 
	virtual ~TeDecoder() {}	

	//! Returns the raster parameters
	TeRasterParams& params()
		{ return params_; }

	//! Updates the raster parameters
	void updateParams(TeRasterParams& par)
	{	params_ = par; }

	//! Sets the value of a specific raster element 
	/*!
		\param col	element column identifier
		\param lin	element line identifier
		\param val	element value being inserted
		\param band element band identifier
	*/
	virtual bool setElement(int col, int lin, double val, int band=0 ) = 0;

	//! An optimizated method to set values raster images
	/*
		This method should be used only to set the values of images with 3 bands associated
		to the three colour channes: RGB
		\param col column of the image
		\param lin line of the image
		\param Rval the value associated to the band 0 (associated to the R colour Channel)
		\param Gval the value associated to the band 1 (associated to the G colour Channel)
		\param Bval the value associated to the band 2 (associated to the B colour Channel)
		\param transp an optional transparency degree, with a range of 0 (totally transparent) to 255 (totally opaque)
		\return true if if succeed and false otherwise
	*/
	virtual bool setElementRGB(int col, int lin, double Rval, double Gval, double Bval, unsigned int /*transp*/ = 255)
	{
		if (!setElement(col,lin,Rval,0))
			return false;
		if (!setElement(col,lin,Gval,1))
			return false;		
		if (!setElement(col,lin,Bval,2))
			return false;
		return true;
	}

	//! Gets an specific element (col, lin, band) of a raster data
	/*!
		\param col	element column identifier
		\param lin	element line identifier
		\param val	element value being retrieved
		\param band element band identifier
	*/
	virtual bool getElement(int col, int lin, double& val, int band=0 ) = 0;

	//! Transforms a geographical coordinate to an index (lin, col) coordinate
	virtual TeCoord2D coord2Index (TeCoord2D &pt)
	{	return params_.coord2Index(pt);	}

	//! Transforms an index (lin, col) coordinate to a geographical coordinate
	virtual TeCoord2D index2Coord (TeCoord2D &pt)
	{	return params_.index2Coord(pt);	}

	//! Initializes the internal structures of the decoder from a raster parameters structure
	/*  
	    \param par the raster parameters structure
	*/
	virtual void init( TeRasterParams& par ) 
	{ params_= par; init();}

	//! Initializes the internal structures of the decoder
	virtual void init() = 0;

	//! Clears its internal structures
	virtual bool clear() = 0;

	//! Returns a default object - for compatibility reasons with TeDecoderFactory
	static TeDecoder* DefaultObject( const TeRasterParams& /* par */) 
	{ return 0; }

/** @name Selection
  These routines are used to implement a selection of blocks or tiles 
  that intercept a given bounding box, and provide sequential access to them.
  These routines should be implemented by concrete decoders to formats that support tilling.
*/
//@{ 
	//! Returns TRUE if it there are raster blocks that intersect a given box
	/*
		\param bb		box	that represents the interest region
		\param resFac	resolution factor
		\param parBlock to return the common parameters of the blocks that are selected
		\returns TRUE if could select at least one block and FALSE otherwise
	*/
	virtual bool selectBlocks(TeBox& /* bb */, int /* resFac */, TeRasterParams& /*parBlock*/)  
	{ return false; }

	//! Returns the number of blocks selected in the last block selection
	virtual int numberOfSelectedBlocks()
	{	return 0; }	
	
	//! Gets a decoder to the current selected block
	virtual bool getSelectedRasterBlock(TeDecoderMemory* /* memDec */) 
	{ return false; }

	//! Clear a previous block selection
	virtual void clearBlockSelection() {}

	//! Returns the raster best resolution level available to fill a given bounding box with a given number of lines and columns in a given projection
	/*
		\param bb		box that represents the interest region
		\param ncols	number of columns in the box
		\param nlines	number of lines in the box
		\param proj		pointer to the projection of the box
	*/
	virtual int bestResolution(TeBox& /*bb*/, int /*ncols*/, int /*nlines*/, TeProjection* /*proj*/)
	{	return 1; }

	//! Returns the raster resolution level available that is more similiar to a given desired resolution
	virtual int bestResolution(double /*res*/)
	{	return 1; }


//@}

protected:

//! The parameters that describes a raster data
	TeRasterParams  params_;	
};

//! A decoder abstract factory
/*! 
	Implements an abstract factory that builds appropriate decoders
	according to a string identifier.
*/
class TL_DLL TeDecoderFactory: public TeFactory<TeDecoder,TeRasterParams> 
{
public:

	//! Maps synonyms to decoders identifiers (example "TIFF" or "tif" to a decoder to TIFF format)
	typedef map<string,string> TeNames2Decoders;

	//! Unique instance instance of map from synonyms to decoder identifiers
	static TeNames2Decoders& instanceName2Dec ()
	{ 
		static TeNames2Decoders names2dec_;
		return names2dec_;
	}

	//! Builds an appropriate decoder from a identifier
	TeDecoderFactory(const string& name) : TeFactory<TeDecoder,TeRasterParams>(name) { }

	//! Virtual destructor
	virtual ~TeDecoderFactory() {}
};

#endif
