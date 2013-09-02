/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2004 INPE and Tecgraf/PUC-Rio.

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

#include "TeDecoderMemory.h"
#include "TeException.h"

#include <assert.h>
#include <cstring> //FAMI

TeDecoderMemory::TeDecoderMemory(): 
	nelems_(0),
	size_(0),
	getelement_ptr_(0),
	setelement_ptr_(0),	
	data_TeUNSIGNEDCHAR_(0),  
	data_TeCHAR_(0),  
	data_TeUNSIGNEDSHORT_(0),  
	data_TeSHORT_(0),  
	data_TeINTEGER_(0), 
	data_TeUNSIGNEDLONG_(0), 
	data_TeLONG_(0),  
	data_TeFLOAT_(0),
	data_TeDOUBLE_(0)
{
	 params_.decoderIdentifier_ = "MEM";
}

TeDecoderMemory::TeDecoderMemory(const TeRasterParams& par): 
	nelems_(0),
	size_(0),
	getelement_ptr_(0),
	setelement_ptr_(0),	
	data_TeUNSIGNEDCHAR_(0),  
	data_TeCHAR_(0),  
	data_TeUNSIGNEDSHORT_(0),  
	data_TeSHORT_(0),  
	data_TeINTEGER_(0), 
	data_TeUNSIGNEDLONG_(0), 
	data_TeLONG_(0),  
	data_TeFLOAT_(0),
	data_TeDOUBLE_(0)
{	
	params_ = par; 
	params_.decoderIdentifier_ = "MEM"; 
}

TeDecoderMemory::~TeDecoderMemory ()
{
	clear();
}

void*	
TeDecoderMemory::data(int band)
{
	// this is possible when band interleaved data
	if (params_.interleaving_ != TeRasterParams::TePerBand)
		return 0;

	void* pt = 0;
	switch (params_.dataType_[band]) 
	{
	case TeUNSIGNEDCHAR:
		pt =  (data_TeUNSIGNEDCHAR_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case TeCHAR:
		pt =  (data_TeCHAR_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case TeUNSIGNEDSHORT:
		pt = (data_TeUNSIGNEDSHORT_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case (TeSHORT):
		pt = (data_TeSHORT_) + (params_.ncols_ * params_.nlines_*band);
		break;
    case (TeINTEGER):
        pt = (data_TeINTEGER_) + (params_.ncols_ * params_.nlines_*band);
        break;                
	case (TeUNSIGNEDLONG):
		pt = (data_TeUNSIGNEDLONG_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case (TeLONG):
		pt = (data_TeLONG_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case (TeFLOAT):
		pt = (data_TeFLOAT_) + (params_.ncols_ * params_.nlines_*band);
		break;
	case (TeDOUBLE):
		pt = (data_TeDOUBLE_) + (params_.ncols_ * params_.nlines_*band);
		break;
	default:
		break;
	}
	return (void*) pt;
}

void 
TeDecoderMemory::init()
{	
	params_.errorMessage_.clear();
	params_.status_= TeRasterParams::TeNotReady;
	nelems_ = params_.nlines_*params_.ncols_*params_.nBands();
	if (!allocateMemory() || !resetMemory())
	{
		params_.errorMessage_ = "Fail to allocate enough memory to hold the raster.";
		return;
	}
	updateFuncPtr();

	// this decoder always grant an read/write access
	params_.status_ = TeRasterParams::TeReadyToWrite;
}

bool 
TeDecoderMemory::getElement (int col,int lin, double& val,int band)
{
	unsigned long position = 0;
	switch (params_.interleaving_)
	{
	case TeRasterParams::TePerPixel:
		position = params_.nBands()*(params_.ncols_*lin+col)+band;
		break;
	case TeRasterParams::TePerLine:
		position = (params_.nBands()*params_.ncols_*lin)+band*params_.ncols_+col;
		break;
	case TeRasterParams::TePerBand:
		position = band*(params_.ncols_*params_.nlines_)+(params_.ncols_*lin+col);
		break;
	}
	assert(position < nelems_);
	(this->*( getelement_ptr_))(position,val);
	return true;
}

bool 
TeDecoderMemory::setElement (int col, int lin, double val,int band)
{
	unsigned long position = 0;
	switch (params_.interleaving_)
	{
	case TeRasterParams::TePerPixel:
		position = params_.nBands()*(params_.ncols_*lin+col)+band;
		break;
	case TeRasterParams::TePerLine:
		position = (params_.nBands()*params_.ncols_*lin)+band*params_.ncols_+col;
		break;
	case TeRasterParams::TePerBand:
		position = band*(params_.ncols_*params_.nlines_)+(params_.ncols_*lin+col);
		break;
	}
	assert(position < nelems_);
	(this->*( setelement_ptr_))(position,val);
	return true;
}

void TeDecoderMemory::updateFuncPtr()
{
	if ( getelement_ptr_ != 0 ) 
		getelement_ptr_ = 0;

	if ( setelement_ptr_ != 0 ) 
		setelement_ptr_ = 0;

	switch ( params_.dataType_[0] ) 
	{
		case (TeCHAR) :
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeCHAR;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeCHAR;
			break;
		}
		case (TeUNSIGNEDSHORT):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeUNSIGNEDSHORT;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeUNSIGNEDSHORT;
			break;
		}
		case (TeSHORT):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeSHORT;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeSHORT;
			break;
		}
		case (TeINTEGER):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeINTEGER;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeINTEGER;
			break;
		}
		case (TeUNSIGNEDLONG):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeUNSIGNEDLONG;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeUNSIGNEDLONG;
			break;
		}
		case (TeLONG):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeLONG;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeLONG;
			break;
		}
		case (TeFLOAT):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeFLOAT;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeFLOAT;
			break;
		}
		case (TeDOUBLE):
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeDOUBLE;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeDOUBLE;
			break;
		}
		default:
		{
			getelement_ptr_ = &TeDecoderMemory::getElement_TeUNSIGNEDCHAR;
			setelement_ptr_ = &TeDecoderMemory::setElement_TeUNSIGNEDCHAR;
			break;
		}				
	}
}

bool 
TeDecoderMemory::allocateMemory()
{
	long nelems = params_.nlines_*params_.ncols_*params_.nBands();

	// allocate necessary memory
	switch (params_.dataType_[0]) 
	{
	case (TeUNSIGNEDCHAR):
		if ((nelems*sizeof(unsigned char)) == (nelems_*sizeof(unsigned char)) && data_TeUNSIGNEDCHAR_)
			return true;
        data_TeUNSIGNEDCHAR_ = new unsigned char [nelems_];
		if (!data_TeUNSIGNEDCHAR_)
			return false;
		size_ = nelems_ * sizeof(unsigned char);
		break;

	case (TeCHAR):
		if ((nelems*sizeof(char)) == (nelems_*sizeof(char)) && data_TeCHAR_)
			return true;
		data_TeCHAR_ = new char [nelems_];
		if (!data_TeCHAR_)
			return false;
		size_ = nelems_ * sizeof(char);
		break;

	case (TeUNSIGNEDSHORT):
		if ((nelems*sizeof(unsigned short)) == (nelems_*sizeof(unsigned short)) && data_TeUNSIGNEDSHORT_)
			return true;
		data_TeUNSIGNEDSHORT_ = new unsigned short [nelems_];
		if (!data_TeUNSIGNEDSHORT_)
			return false;
		size_ = nelems_ * sizeof(unsigned short);
		break;

	case (TeSHORT):
		if ((nelems*sizeof( short)) == (nelems_*sizeof(short)) && data_TeSHORT_)
			return true;
		data_TeSHORT_ = new short [nelems_];
		if (!data_TeSHORT_)
			return false;
		size_ = nelems_ * sizeof(short);
		break;

    case (TeINTEGER):
 		if ((nelems*sizeof(int)) == (nelems_*sizeof(int)) && data_TeINTEGER_)
			return true;
       data_TeINTEGER_ = new int [nelems_];
        if (!data_TeINTEGER_)
			return false;
        size_ = nelems_ * sizeof(int);
        break;

	case (TeUNSIGNEDLONG):
		if ((nelems*sizeof(unsigned long)) == (nelems_*sizeof(unsigned long)) && data_TeUNSIGNEDLONG_)
			return true;
		data_TeUNSIGNEDLONG_ = new unsigned long [nelems_];
		if (!data_TeUNSIGNEDLONG_)
			return false;
		size_ = nelems_ * sizeof(unsigned long);
		break;

	case (TeLONG):
		if ((nelems*sizeof(long)) == (nelems_*sizeof(long)) && data_TeLONG_)
			return true;
		data_TeLONG_ = new long [nelems_];
		if (!data_TeLONG_)
			return false;
		size_ = nelems_ * sizeof(long);
		break;

	case (TeFLOAT):
		if ((nelems*sizeof(float)) == (nelems_*sizeof(float)) && data_TeFLOAT_)
			return true;
		data_TeFLOAT_ = new float [nelems_];
		if (!data_TeFLOAT_)
			return false;
		size_ = nelems_ * sizeof(float);
		break;

	case (TeDOUBLE):
		if ((nelems*sizeof(double)) == (nelems_*sizeof(double)) && data_TeDOUBLE_)
			return true;
		data_TeDOUBLE_ = new double [nelems_];
		if (!data_TeDOUBLE_)
			return false;
		size_ = nelems_ * sizeof(double);
		break;

	default:
		return false;
	}
	return true;
}


bool 
TeDecoderMemory::resetMemory()
{
	unsigned int i;
	switch (params_.dataType_[0]) 
	{
	case (TeBIT):
	case (TeCHAR):
		if (!data_TeCHAR_)
			return false;
		for(i=0; i<nelems_; data_TeCHAR_[i]=static_cast<char>(params_.dummy_[0]),++i);
		break;

	case (TeUNSIGNEDCHAR):
		if (!data_TeUNSIGNEDCHAR_)
			return false;
		for(i=0; i<nelems_; data_TeUNSIGNEDCHAR_[i]=static_cast<unsigned char>(params_.dummy_[0]),++i);
		break;

	case (TeUNSIGNEDSHORT):
		if (!data_TeUNSIGNEDSHORT_)
			return false;
		for(i=0; i<nelems_; data_TeUNSIGNEDSHORT_[i]=static_cast<unsigned short>(params_.dummy_[0]),++i);
		break;

	case (TeSHORT):
		if (!data_TeSHORT_)
			return false;
		for(i=0; i<nelems_; data_TeSHORT_[i]=static_cast<short>(params_.dummy_[0]),++i);
		break;

	case (TeINTEGER):
		if (!data_TeINTEGER_)
			return false;
		for(i=0; i<nelems_; data_TeINTEGER_[i]=static_cast<int>(params_.dummy_[0]),++i);
		break;

	case (TeUNSIGNEDLONG):
		if (!data_TeUNSIGNEDLONG_)
			return false;
		for(i=0; i<nelems_; data_TeUNSIGNEDLONG_[i]=static_cast<unsigned long>(params_.dummy_[0]),++i);
		break;

	case (TeLONG):
		if (!data_TeLONG_)
			return false;
		for(i=0; i<nelems_; data_TeLONG_[i]=static_cast<long>(params_.dummy_[0]),++i);
		break;

	case (TeFLOAT):
		if (!data_TeFLOAT_)
			return false;
		for(i=0; i<nelems_; data_TeFLOAT_[i]=static_cast<float>(params_.dummy_[0]),++i);
		break;

	case (TeDOUBLE):
		if (!data_TeDOUBLE_)
			return false;
		for(i=0; i<nelems_; data_TeDOUBLE_[i]=static_cast<double>(params_.dummy_[0]),++i);
		break;

	default:
		break;	
}

	return true;
}

bool
TeDecoderMemory::clear()
{
	if (data_TeUNSIGNEDCHAR_)
	{
		delete [] data_TeUNSIGNEDCHAR_;
		data_TeUNSIGNEDCHAR_ = 0;
		return true;
	}
	if (data_TeCHAR_)
	{
		delete [] data_TeCHAR_; 
		data_TeCHAR_ = 0;
		return true;
	}
	if (data_TeUNSIGNEDSHORT_)
	{
		delete [] data_TeUNSIGNEDSHORT_;  
		data_TeUNSIGNEDSHORT_ = 0;
		return true;
	}
	if (data_TeSHORT_)
	{
		delete [] data_TeSHORT_;  
		data_TeSHORT_ = 0;
		return true;
	}
	if (data_TeINTEGER_) 
	{
		delete [] data_TeINTEGER_;  
		data_TeINTEGER_ = 0;
		return true;
	}
	if (data_TeUNSIGNEDLONG_)
	{
		delete [] data_TeUNSIGNEDLONG_;  
		data_TeUNSIGNEDLONG_ = 0;
		return true;
	}
	if (data_TeLONG_)
	{
		delete [] data_TeLONG_;  
		data_TeLONG_ = 0;
		return true;
	}
	if (data_TeFLOAT_)
	{
		delete [] data_TeFLOAT_;
		data_TeFLOAT_ = 0;
		return true;
	}
	if (data_TeDOUBLE_)
	{
		delete [] data_TeDOUBLE_;
		data_TeDOUBLE_ = 0;
		return true;
	}
	params_.status_ = TeRasterParams::TeNotReady;
	return true;
}


inline void TeDecoderMemory::setElement_TeUNSIGNEDCHAR(const long& pos, const double& val)
{
	assert(data_TeUNSIGNEDCHAR_ != 0 );
	data_TeUNSIGNEDCHAR_[pos] = (unsigned char)val;
}


inline void TeDecoderMemory::setElement_TeCHAR(const long& pos, const double& val)
{
	assert(data_TeCHAR_ != 0 );
	data_TeCHAR_[pos] = (char)val;
}


inline void TeDecoderMemory::setElement_TeUNSIGNEDSHORT(const long& pos, const double& val)
{
	assert(data_TeUNSIGNEDSHORT_ != 0 );
	data_TeUNSIGNEDSHORT_[pos] = (unsigned short)val;
}


inline void TeDecoderMemory::setElement_TeSHORT(const long& pos, const double& val)
{
	assert(data_TeSHORT_ != 0 );
	data_TeSHORT_[pos] = (short)val;
}


inline void TeDecoderMemory::setElement_TeINTEGER(const long& pos, const double& val)
{
	assert(data_TeINTEGER_ != 0 );
	data_TeINTEGER_[pos] = (int)val;
}


inline void TeDecoderMemory::setElement_TeUNSIGNEDLONG(const long& pos, const double& val)
{
	assert(data_TeUNSIGNEDLONG_ != 0 );
	data_TeUNSIGNEDLONG_[pos] = (unsigned long)val;
}


inline void TeDecoderMemory::setElement_TeLONG(const long& pos, const double& val)
{
	assert(data_TeLONG_ != 0 );
	data_TeLONG_[pos] = (long)val;
}


inline void TeDecoderMemory::setElement_TeFLOAT(const long& pos, const double& val)
{
	assert(data_TeFLOAT_ != 0 );
	data_TeFLOAT_[pos] = (float)val;
}


inline void TeDecoderMemory::setElement_TeDOUBLE(const long& pos, const double& val)
{
	assert(data_TeDOUBLE_ != 0 );
	data_TeDOUBLE_[pos] = val;
}


inline void TeDecoderMemory::getElement_TeUNSIGNEDCHAR(const long& pos, double& val)
{
	assert(data_TeUNSIGNEDCHAR_ != 0 );
	val = (double)data_TeUNSIGNEDCHAR_[pos];
}


inline void TeDecoderMemory::getElement_TeCHAR(const long& pos, double& val)
{
	assert(data_TeCHAR_ != 0 );
	val = (double)data_TeCHAR_[pos];
}


inline void TeDecoderMemory::getElement_TeUNSIGNEDSHORT(const long& pos, double& val)
{
	assert(data_TeUNSIGNEDSHORT_ != 0 );
	val = (double)data_TeUNSIGNEDSHORT_[pos];
}


inline void TeDecoderMemory::getElement_TeSHORT(const long& pos, double& val)
{
	assert(data_TeSHORT_ != 0 );
	val = (double)data_TeSHORT_[pos];
}


inline void TeDecoderMemory::getElement_TeINTEGER(const long& pos, double& val)
{
	assert(data_TeINTEGER_ != 0 );
	val = (double)data_TeINTEGER_[pos];
}


inline void TeDecoderMemory::getElement_TeUNSIGNEDLONG(const long& pos, double& val)
{
	assert(data_TeUNSIGNEDLONG_ != 0 );
	val = (double)data_TeUNSIGNEDLONG_[pos];
}


inline void TeDecoderMemory::getElement_TeLONG(const long& pos,double& val)
{
	assert(data_TeLONG_ != 0 );
	val = (double)data_TeLONG_[pos];
}


inline void TeDecoderMemory::getElement_TeFLOAT(const long& pos, double& val)
{
	assert(data_TeFLOAT_ != 0 );
	val = (double)data_TeFLOAT_[pos];
}


inline void TeDecoderMemory::getElement_TeDOUBLE(const long& pos, double& val)
{
	assert(data_TeDOUBLE_ != 0 );
	val = (double)data_TeDOUBLE_[pos];
}

