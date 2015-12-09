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
/*! \file TeDecoderDatabase.h
    \brief This file deals with decoding of raster structures stored in a TerraLib database
*/

#ifndef  __TERRALIB_INTERNAL_DECODERDATABASE_H
#define  __TERRALIB_INTERNAL_DECODERDATABASE_H

#include "TeDefines.h"
#include "TeDecoderVirtualMemory.h"
#include "TeDatabase.h"

//! A concrete class to manipulate raster data stored in a TerraLib database
/*
	TeDecoderDatabase uses a virtual memory, to handle the raster blocks 
	(as proposed by the TerraLib data model to store raster data) traffic from
	memory to database and vice versa.

	\sa TeDatabase TeDecoderVirtualMemory
*/
class TL_DLL TeDecoderDatabase: public TeDecoderVirtualMemory
{
public:
	//! Empty constructor
	TeDecoderDatabase() :
		db_(0),	
		blockPortal_(0),
		memAux_(0),
		nSelectedBlocks_(0)
	{	params_.decoderIdentifier_ = "DB"; }

	//! Constructor from parameters
	TeDecoderDatabase( const TeRasterParams& par );

	//! Destructor
	~TeDecoderDatabase();

	//! Sets the database pointer associated to this decoder
	void setDB (TeDatabase* db) 
	{ db_ = db; }

	//! Initializes the decoder
	virtual void init();

	//! Creates the decoder
	virtual bool create();

	//! Clears the decoder
	virtual bool clear();

	//! Gets the raster block with index identifier
	/*!
		\param index tile unique identifier
		\param buf pointer to a raster tile in memory
	*/
	bool getRasterBlock(const string& index, void *buf);

	//! Saves a raster tile from a virtual memory to permanent storage
	/*! 
	    \param index tile unique identifier 
		\param buf pointer to a raster tile in memory
		\param bsize size of the block in bytes
	*/	
	bool putRasterBlock(const string& index, void *buf, long bsize);

	//! Codifiy the index for the block that contains an element
	string codifyId(int col, int lin, int band, int res, int subb);

	//! Decodify the parameters of the block that has an index 
	void decodifyId(const string& id, int& col,int& lin, int& band, int& res, int& subb);


	//! Saves the lut table of the raster associated to this decoder
	bool saveLUTTable();

	//! Search for tiles of image that intersects a certain bounding box in a given resolution
	/* 
		\param bb bounding box that should be filled
		\param resFac resolution factor that identify a level of resolution
		\param parBlock returns the basic parameters  of the selected tiles
		\note Resolution factor in decoder database is a multiplier of the original resolution
		and assume integer multiples of two. Smaller resolution has resolution factor
		of 1 and the higher resolution have multiple of two values (2,4,8, 16...)
	*/
	int bestResolution(TeBox& bb, int ncols, int nlines, TeProjection* proj);

	//! Select the stored raster blocks with a certain resolution to fill a given bounding box
	/*
		\param bb bounding box that should be filled 
		\param the resolution factor desired
		\param parBlock common parameters of the blocks selected
		\returns TRUE if could select at least one block and FALSE otherwise
	*/
	bool selectBlocks(TeBox& bb, int resFac, TeRasterParams& parBlock);

	//! Gets current raster block an outputs it in a memory decoder
	bool getSelectedRasterBlock(TeDecoderMemory* memDec);

	//! Clears selected blocks portal
	void clearBlockSelection();

	//! Returns the number of selectd raster blocks
	int numberOfSelectedBlocks()
	{	return nSelectedBlocks_; }

	//! Decodifies the position of a block within the raster from its index
	virtual void blockIndexPos( const TeBlockIndex& index, int& ulCol, int& ulLin, int& band); 

protected:

	virtual TeBlockIndex blockIndex(int col, int lin, int band);  
	virtual string codifyId(const TeBlockIndex& idx, int res = 1, int subb = 0); 

	bool getRasterBlock(const TeBlockIndex& index, void *buf);
	bool putRasterBlock(const TeBlockIndex& index, void *buf, long bsize);

private:
	TeDatabase			*db_;
	TeDatabasePortal	*blockPortal_;
	unsigned char		*memAux_;
	int					nSelectedBlocks_;
};

//! Implements a factory to build database decoders
class TL_DLL TeDecoderDatabaseFactory : public TeDecoderFactory
{
public:

	//! Constructor
	TeDecoderDatabaseFactory(const string& name) : TeDecoderFactory(name) {}

	//! Builds a database decoder
	virtual TeDecoder* build (const TeRasterParams& arg)
	{  return new TeDecoderDatabase(arg); }
};
#endif

