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
/*! \file TeDecoderVirtualMemory.h
    \brief This file supports a virtual memory strategy to deal whith raster structures
*/
#ifndef  __TERRALIB_INTERNAL_DECODERVIRTUALMEMORY_H
#define  __TERRALIB_INTERNAL_DECODERVIRTUALMEMORY_H

#include "TeDefines.h"
#include "TeDecoder.h"

#include <queue>
#include <vector>

using namespace std;

//! A page of memory
class TL_DLL TeMemoryPage
{
public:
	void*   data_;			//<! Pointer to the data in memory
	bool	used_;			//<! Flag that indicate if the page was modified in memory
	unsigned long size_;	//<! Page number of elements

	int ulLin_;				//<! Line index of the upper-left corner
	int ulCol_;				//<! Column index of the upper-left corner
	double   defValue_;		//<! Default value (fo initially fill the page)
	TeDataType dataType_;	//<! Pixel data type

	//! Constructor
	TeMemoryPage(unsigned long size, double defValue, TeDataType dataType = TeDOUBLE);
	
	//! Destructor
	~TeMemoryPage();

	//! Copy constructor
	TeMemoryPage(const TeMemoryPage& rhs);
	
	//! Operator =
	TeMemoryPage& operator=(const TeMemoryPage& rhs);

	//! Returns the value of on position within the block
	/*
		\par col column identifier of the position
		\par lin line identifier of the position
		\par nCols number of columns per line of the block
		\returns the value of the position indicated by (col x lin)
	*/
	double getVal(int col,int lin, int nCols);

	//! Sets the value of a position within the block
	/*
		\par col column identifier of the position
		\par lin line identifier of the position
		\par nCols number of columns per line of the block
		\par val the value to be put in the position
	*/
	void setVal(int col,int lin,int nCols, double val);

	//! Set all positions of the page to the default value;
	void clear();

	//! Returns the physical size of a memory page
	long pageSize();
};

class TL_DLL TeBlockIndex{
public:
	TeBlockIndex()
	{
		col_ = 0;
		lin_ = 0;
		band_ = -1;
	}

	int col_;
	int lin_;
	int band_;

	int column()  const
	{
		return col_;
	}

	int line() const
	{
		return lin_;
	}

	int band() const
	{
		return band_;
	}

	int operator==(const TeBlockIndex& idx2) const
	{
		if(col_ != idx2.col_)
			return 0;

		if(lin_ != idx2.lin_)
			return 0;

		return (band_ == idx2.band_);
	}
};

//! Implements a virtual memory strategy to decode raster in blocks
/*
	It should be used as a parent class of decoder that access raster
	blocks from a physical storage
*/
class TL_DLL TeDecoderVirtualMemory: public TeDecoder
{
public:

	//! Empty constructor
	TeDecoderVirtualMemory() {};
	
	//! Constructor from parameters
	TeDecoderVirtualMemory( const TeRasterParams par);

	//! Destructor
	virtual ~TeDecoderVirtualMemory();

	//! Sets the value of a specific raster pixel 
	/*!
		\param col pixel column identifier
		\param lin pixel line identifier
		\param val pixel value being inserted
		\param band pixel band identifier
	*/
	bool getElement(int col,int lin, double &val,int band);

	//! Gets an specific element (col, lin, band) of a raster data
	/*!
		\param col pixel column identifier
		\param lin pixel line identifier
		\param val pixel value being retrieved
		\param band pixel band identifier
	*/
	bool setElement(int col, int lin, double val,int band);

	//! Initializes the internal structures of the decoder, from its raster parameters structure.
	void init();

	//! Clears  the decoder internal structures
	bool clear();

	//! Defines the physical size of the cache
	void setCacheSize(int size);  

	//! Returns the physical size of the cache
	int getCacheSize();

	//! Defines the number of tiles in the cache
	void setCacheNTiles(int n);

	//! Returns the number of tiles in the cache
	int getCacheNTiles();

protected:
	struct TeBlockIndexMapFunc
	{
		bool operator()(const TeBlockIndex& idx1, const TeBlockIndex& idx2) const;
	};

	//! Saves a raster tile from a virtual memory to permanent storage
	/*! 
	    \param index block index
		\param buf pointer to a raster tile in memory
		\param bsize block size
	*/	
	virtual bool putRasterBlock(const TeBlockIndex& index, void *buf, long bsize) = 0; /******/

	//! Gets the raster block with index identifier
	/*!
		\param index block index
		\param buf pointer to a raster tile in memory
	*/
	virtual bool getRasterBlock(const TeBlockIndex& index, void *buf) = 0; /******/

	//! Codifies the unique identifier of the raster block that contains a certain pixel
	/*!
		\param col column number
		\param lin  pixel line number
		\param band pixel band 
		\return block index
	*/
	virtual TeBlockIndex blockIndex(int col, int lin, int band); 

	//! Returns the parameters of a tile from its indexs
	virtual void blockIndexPos( const TeBlockIndex& index, int& ulCol, int& ulLin, int& band); /******/

	//! Retrieve a block of the cache that contains an element
	TeMemoryPage* loadBlock(int col,int lin, int band);

private:

	//! A map of string identifiers to pointer to memory pages 
	typedef map<TeBlockIndex, TeMemoryPage*, TeBlockIndexMapFunc> MapMemoryPage; 

	//! A const iterator to a map of pages
	typedef MapMemoryPage::const_iterator MapMemoryPageIterator;

	MapMemoryPage virtualMemory_;
	queue<TeBlockIndex> pagesQueue_; 
	vector<TeBlockIndex> indexCache_; 
	vector<TeMemoryPage*> pageCache_;
};

inline bool TeDecoderVirtualMemory::TeBlockIndexMapFunc::operator()(const TeBlockIndex& idx1, const TeBlockIndex& idx2) const
{
  if(idx1.band_ != idx2.band_)
    return (idx1.band_ < idx2.band_);

  if(idx1.lin_ != idx2.lin_)
    return idx1.lin_ < idx2.lin_;

  return (idx1.col_ < idx2.col_);
}
#endif
