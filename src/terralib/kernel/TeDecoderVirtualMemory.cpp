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

#include "TeDecoderVirtualMemory.h"
#include <cstring>

TeMemoryPage::TeMemoryPage(const TeMemoryPage& rhs)
{
	data_ = rhs.data_;
	used_ = rhs.used_;
	defValue_ = rhs.defValue_;
	dataType_ = rhs.dataType_;
}

//! Operator =
TeMemoryPage& 
TeMemoryPage::operator=(const TeMemoryPage& rhs)
{
	if (this != &rhs)
	{
		data_ = rhs.data_;
		used_ = rhs.used_;
		size_ = rhs.size_;
		defValue_ = rhs.defValue_;
		dataType_ = rhs.dataType_;
	}
	return *this;
}

//! Destructor
TeMemoryPage::~TeMemoryPage()
{
	if (this->data_ != 0)
	{
		switch (dataType_)
		{
		case (TeUNSIGNEDCHAR):
			delete [](unsigned char*)data_;
			break;
		case (TeCHAR) :
			delete (char*)data_;
			break;
		case (TeUNSIGNEDSHORT):
			delete [](unsigned short*)data_;
			break;
		case (TeSHORT):
			delete (short*) data_;
			break;
		case (TeUNSIGNEDLONG):
			delete [](unsigned long*)data_;
			break;
		case (TeLONG):
			delete [](long*) data_;
			break;
		case (TeFLOAT):
			delete [](float*)data_;
			break;
		case (TeDOUBLE):
			delete [](double*) data_;
			break;
		default:
			break;
		}
		data_ = 0;
	}
}

TeMemoryPage::TeMemoryPage(unsigned long size, double defValue, TeDataType dataType):
		used_(false),
		size_(size),
		defValue_(defValue),
		dataType_(dataType)
{
	data_ = 0;
	try 
	{
		unsigned int i;
		// Try to allocate the necessary memory
		switch (dataType_) {
		case (TeUNSIGNEDCHAR):
			data_ = new unsigned char [size_];
			memset(data_, (int)defValue, size);
			break;
		case (TeCHAR) :
			data_ = new char [size_];
			memset(data_, (int)defValue, size);
			break;
		case (TeUNSIGNEDSHORT):
			data_ = new unsigned short [size_];
			for (i=0;i<size;((unsigned short*)data_)[i]= (unsigned short) defValue_,i++);
			break;
		case (TeSHORT):
			data_ = new short [size_];
			for (i=0;i<size;((short*)data_)[i]=(short)defValue_,i++);
			break;
		case (TeUNSIGNEDLONG):
			data_ = new unsigned long [size_];
			for (i=0;i<size;((unsigned long*)data_)[i]=(unsigned long)defValue_,i++);
			break;
		case (TeLONG):
			data_ = new long [size_];
			for (i=0;i<size;((long*)data_)[i]=(long)defValue_,i++);
			break;
		case (TeFLOAT):
			data_ = new float [size_];
			for (i=0;i<size;((float*)data_)[i]=(float)defValue_,i++);
			break;
		case (TeDOUBLE):
			data_ = new double [size_];
			for (i=0;i<size;((double*)data_)[i]=(double)defValue_,i++);
			break;
		default:
			break;
		}
	}
	catch (...)
	{
	}
}

void
TeMemoryPage::clear()
{	
	unsigned int i;
	switch (dataType_) 
	{
	case (TeUNSIGNEDCHAR):
	case (TeCHAR) :
		memset((void*)data_, (int)defValue_, size_);
		break;
	case (TeUNSIGNEDSHORT):
		for (i=0;i<size_;((unsigned short*)data_)[i]= (unsigned short) defValue_,i++);
		break;
	case (TeSHORT):
		for (i=0;i<size_;((short*)data_)[i]=(short)defValue_,i++);
		break;
	case (TeUNSIGNEDLONG):
		for (i=0;i<size_;((unsigned long*)data_)[i]=(unsigned long)defValue_,i++);
		break;
	case (TeLONG):
		for (i=0;i<size_;((long*)data_)[i]=(long)defValue_,i++);
		break;
	case (TeFLOAT):
		for (i=0;i<size_;((float*)data_)[i]=(float)defValue_,i++);
		break;
	case (TeDOUBLE):
		for (i=0;i<size_;((double*)data_)[i]=(double)defValue_,i++);
		break;
	default:
		break;
	}
}

long
TeMemoryPage::pageSize()
{
	long s;
	switch (dataType_) 
	{
	case (TeCHAR) :
		s = size_*sizeof(char);
		break;
	case (TeUNSIGNEDSHORT):
		s = size_*sizeof(unsigned short);
		break;
	case (TeSHORT):
		s = size_*sizeof(short);
		break;
	case (TeUNSIGNEDLONG):
		s = size_*sizeof(unsigned long);
		break;
	case (TeLONG):
		s = size_*sizeof(long);
		break;
	case (TeFLOAT):
		s = size_*sizeof(float);
		break;
	case (TeDOUBLE):
		s = size_*sizeof(double);
		break;
	default:
		s = size_*sizeof(unsigned char);
	}
	return s;
}

double 
TeMemoryPage::getVal(int col,int lin, int nCols)
{
	unsigned long offset = (lin-ulLin_)*nCols+(col-ulCol_);
	double val = defValue_;

	if (offset < size_) // solucao temporaria, rever
	{
		switch (dataType_) 
		{
		case (TeUNSIGNEDCHAR):
			val = ((unsigned char*)data_)[offset];
			break;
		case (TeCHAR) :
			val = ((char*) data_)[offset];
			break;
		case (TeUNSIGNEDSHORT):
			val = ((unsigned short*)data_)[offset];
			break;
		case (TeSHORT):
			val = ((short*)data_)[offset];
			break;
		case (TeUNSIGNEDLONG):
			val = ((unsigned long*)data_)[offset];
			break;
		case (TeLONG):
			val = ((long*)data_)[offset];
			break;
		case (TeFLOAT):
			val = ((float*)data_)[offset];
			break;
		case (TeDOUBLE):
			val = ((double*)data_)[offset];
			break;
		default:
			break;
		}
	}

	return val;
}

void 
TeMemoryPage::setVal(int col,int lin,int nCols, double val)
{
	unsigned long offset = (lin-ulLin_)*nCols+(col-ulCol_);
	if (offset < size_) 
	{
		switch (dataType_) 
		{
		case (TeUNSIGNEDCHAR):
			((unsigned char*)data_)[offset] = (unsigned char) val;
			break;
		
		case (TeCHAR) :
			((char*) data_)[offset] = (char) val;
			break;
		
		case (TeUNSIGNEDSHORT):
			((unsigned short*)data_)[offset] = (unsigned short) val;
			break;
		
		case (TeSHORT):
			((short*)data_)[offset] = (short) val;
			break;
		
		case (TeUNSIGNEDLONG):
			((unsigned long*)data_)[offset] = (unsigned long) val;
			break;
		
		case (TeLONG):
			((long*)data_)[offset] = (long) val;
			break;
		
		case (TeFLOAT):
			((float*)data_)[offset] = (float) val;
			break;
		
		case (TeDOUBLE):
			((double*)data_)[offset] = val;

		default:
			break;
		}
	}
	return;
}

TeDecoderVirtualMemory::TeDecoderVirtualMemory(const TeRasterParams par)
{
	params_ = par;
}

TeDecoderVirtualMemory::~TeDecoderVirtualMemory()
{
	if (!pagesQueue_.empty())
		TeDecoderVirtualMemory::clear();
}

TeMemoryPage* TeDecoderVirtualMemory::loadBlock(int col,int lin, int band)
{
	// If element is outside raster boundaries return
	if(col < 0 || lin < 0 || col > params_.ncols_ || lin > params_.nlines_)
		return NULL;

	TeBlockIndex index = blockIndex(col,lin,band);
	TeMemoryPage* block = NULL;

	// check band cache first
	if (pageCache_[band] != NULL && indexCache_[band] == index)
		block = pageCache_[band];
	else
	{
		// check if page is already in memory
		MapMemoryPageIterator p = virtualMemory_.find(index);
		if (p != virtualMemory_.end()) // use it
			block = p->second;
		else
		{
			// page is not in memory
			// check whether there is space in memory to bring another page
			if (virtualMemory_.size() >= (unsigned int)params_.nTilesInMemory_)
			{
				// FIFO strategy: replace the oldest page in memory
				TeBlockIndex first = pagesQueue_.front();
				pagesQueue_.pop();
				p = virtualMemory_.find(first); 
				if (p != virtualMemory_.end())
				{
					block = p->second;
					if (block->used_)
						putRasterBlock(first,block->data_,block->pageSize());

					block->used_ = false;
					block->clear();				// reuse the allocated memory
					virtualMemory_.erase(first);
				}
			}
			else // bring the page to memory
				block = new TeMemoryPage(params_.blockHeight_*params_.blockWidth_, params_.dummy_[band],params_.dataType_[band]);

			virtualMemory_.insert(MapMemoryPage::value_type(index,block));
			pagesQueue_.push(index);
			getRasterBlock(index,block->data_);
			int band_out;
			blockIndexPos(index, block->ulCol_, block->ulLin_, band_out);
		}
		indexCache_[band] = index;
		pageCache_[band] = block;
	}
  return block;
}

bool
TeDecoderVirtualMemory::getElement(int col,int lin, double &val,int band)
{
	TeMemoryPage* block = loadBlock(col, lin, band);
	if(block == NULL)
	{
		val = params_.dummy_[band];
		return false;
	}
	val = block->getVal(col,lin,params_.blockWidth_);
	return true;
}

bool
TeDecoderVirtualMemory::setElement(int col, int lin, double val,int band)
{
	TeMemoryPage* block = loadBlock(col, lin, band);
	if(block == NULL)
		return false;

	block->setVal(col,lin,params_.blockWidth_,val);
	block->used_ = true;
	return true;
}

TeBlockIndex TeDecoderVirtualMemory::blockIndex(int col, int lin, int band)
{
	TeBlockIndex bl_idx;

	bl_idx.band_ = band;
	bl_idx.col_ = (int)(col/params_.blockWidth_);
	bl_idx.lin_ = (int)(lin/params_.blockHeight_);
	return bl_idx;
}

void TeDecoderVirtualMemory::blockIndexPos( const TeBlockIndex& index, int& ulCol, int& ulLin, int& band)
{
	ulCol = index.column()*params_.blockWidth_;
	ulLin = index.line()*params_.blockHeight_;
	band = index.band();
}

void
TeDecoderVirtualMemory::init()
{
	TeDecoderVirtualMemory::clear();
	if (params_.nBands() == 0 || params_.ncols_ <= 0 || params_.nlines_ == 0)
	{
		params_.errorMessage_ = "Raster doesnt have valid dimensions.";
		params_.status_ = TeRasterParams::TeNotReady;
		return;
	}
	if ( params_.nTilesInMemory_ == 0 )
		params_.nTilesInMemory_ = params_.nBands() * (params_.ncols_ / params_.blockWidth_+1) ;
}

bool
TeDecoderVirtualMemory::clear()
{
	TeMemoryPage* block;
	TeBlockIndex index;
	MapMemoryPageIterator p;
	while (!pagesQueue_.empty() )
	{
		index = pagesQueue_.front();
		pagesQueue_.pop();
		p = virtualMemory_.find(index);
		if (p != virtualMemory_.end())
		{
			block = p->second;
			if (block->used_)
				putRasterBlock(index,block->data_,block->pageSize());
			delete block;
			virtualMemory_.erase(index);
		}
	}
	
	pageCache_.resize(params_.nBands());
	for(unsigned int i = 0; i < pageCache_.size(); ++i)
	{
		pageCache_[i] = NULL;
	}

	indexCache_.resize(params_.nBands());
	for(unsigned int i = 0; i < indexCache_.size(); ++i)
	{
		indexCache_[i] = TeBlockIndex();
	}

	return true;
}

void TeDecoderVirtualMemory::setCacheSize(int size)
{
	int block_size = params_.blockHeight_ * params_.blockWidth_;
	setCacheNTiles((int)(size / block_size));  
}

int TeDecoderVirtualMemory::getCacheSize()
{
	int block_size = params_.blockHeight_ * params_.blockWidth_;
	return getCacheNTiles() * block_size;
}

void TeDecoderVirtualMemory::setCacheNTiles(int n)
{
	params_.nTilesInMemory_ = n;
	TeDecoderVirtualMemory::clear();
}

int TeDecoderVirtualMemory::getCacheNTiles()
{
	return params_.nTilesInMemory_;
}
