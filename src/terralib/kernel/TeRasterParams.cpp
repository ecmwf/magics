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

#ifdef WIN32 
#pragma warning ( disable: 4786 ) 
#endif

#include "TeRasterParams.h"
#include "TeUtils.h"
#include "TeAsciiFile.h"
#include "TeAgnostic.h"
#include <cstdlib>

// copy constructor
TeRasterParams::TeRasterParams(const TeRasterParams& other)
{
	resolution_ = other.resolution_;
	subBand_  = other.subBand_;
	nlines_		= other.nlines_;
	ncols_		= other.ncols_;
	nbands_		= other.nbands_;
	lutName_	= other.lutName_;
	swap_		= other.swap_;
	useDummy_	= other.useDummy_;

	nbitsperPixel_.clear();
	vmin_.clear();
	vmax_.clear();
	bands_.clear();
	photometric_.clear();
	dataType_.clear();
	dummy_.clear();
	compression_.clear();

	nbitsperPixel_= other.nbitsperPixel_;
	vmin_= other.vmin_;
	vmax_= other.vmax_;
	bands_= other.bands_;
	photometric_= other.photometric_;
	dataType_= other.dataType_;
	dummy_= other.dummy_;
	compression_= other.compression_;

	resx_ = other.resx_;
	resy_ = other.resy_;
	fileName_ = other.fileName_;
	mode_ = other.mode_;
	status_ = other.status_;
	offset_ = other.offset_;
	tiling_type_ = other.tiling_type_;
	blockHeight_ = other.blockHeight_;
	blockWidth_ = other.blockWidth_;
	decoderIdentifier_ = other.decoderIdentifier_;
	nTilesInMemory_ = other.nTilesInMemory_;
	if (other.projection_)
		projection_ = TeProjectionFactory::make(other.projection_->params());
	else
		projection_=0;
	box_ = other.box_;
	database_ = other.database_;
	layerId_ = other.layerId_;
	objectId_ = other.objectId_;

	interleaving_ = other.interleaving_;

	lutr_.clear();
	lutg_.clear();
	lutb_.clear();
	lutr_ = other.lutr_;
	lutg_ = other.lutg_;
	lutb_ = other.lutb_;

	dyJ_ = other.dyJ_;
	dxI_ = other.dxI_;
	dyI_ = other.dyI_;
	dxJ_ = other.dxJ_;
	x0_ = other.x0_;
	y0_ = other.y0_;
	errorMessage_ = other.errorMessage_;
	hasSetDummy_ = other.hasSetDummy_;
}

// operator =
TeRasterParams& 
TeRasterParams::operator=(const TeRasterParams& rhs)
{
	if ( this != &rhs )
	{
		resolution_ = rhs.resolution_;
		subBand_  = rhs.subBand_;
		nlines_		= rhs.nlines_;
		ncols_		= rhs.ncols_;
		nbands_		= rhs.nbands_;
		swap_		= rhs.swap_;
		useDummy_	= rhs.useDummy_;
		lutName_	= rhs.lutName_;

		nbitsperPixel_.clear();
		vmin_.clear();
		vmax_.clear();
		bands_.clear();
		photometric_.clear();
		dataType_.clear();
		dummy_.clear();
		compression_.clear();

		nbitsperPixel_= rhs.nbitsperPixel_;
		vmin_= rhs.vmin_;
		vmax_= rhs.vmax_;
		bands_= rhs.bands_;
		photometric_= rhs.photometric_;
		dataType_= rhs.dataType_;
		dummy_= rhs.dummy_;
		compression_= rhs.compression_;

		resx_ = rhs.resx_;
		resy_ = rhs.resy_;
		fileName_ = rhs.fileName_;
		mode_ = rhs.mode_;
		status_ = rhs.status_;
		offset_ = rhs.offset_;
		tiling_type_ = rhs.tiling_type_;
		blockHeight_ = rhs.blockHeight_;
		blockWidth_ = rhs.blockWidth_;
		decoderIdentifier_ = rhs.decoderIdentifier_;
		nTilesInMemory_ = rhs.nTilesInMemory_;
		if (projection_)
		{
			delete projection_;
			projection_ = 0;
		}
		if (rhs.projection_)
			projection_ = TeProjectionFactory::make(rhs.projection_->params());
		box_ = rhs.box_;
		database_ = rhs.database_;			
		layerId_ = rhs.layerId_;
		objectId_ = rhs.objectId_;
		interleaving_ = rhs.interleaving_;

		lutr_.clear();
		lutg_.clear();
		lutb_.clear();
		lutr_ = rhs.lutr_;
		lutg_ = rhs.lutg_;
		lutb_ = rhs.lutb_;

		dyJ_ = rhs.dyJ_;
		dxI_ = rhs.dxI_;
		x0_ = rhs.x0_;
		y0_ = rhs.y0_;
		dyI_ =  rhs.dyI_;
		dxJ_ =  rhs.dxJ_;
		errorMessage_ = rhs.errorMessage_;
		hasSetDummy_ = rhs.hasSetDummy_;
	}
	return *this;
}

void
TeRasterParams::resizeToTiling(TeBox& bb, int bWidth, int bHeight)
{
	blockHeight_ = bHeight;
	blockWidth_ = bWidth;

	double bXSize = bWidth*resx_;
	double bYSize = bHeight*resy_;
	
	TeBox baux = adjustToCut(bb, bXSize, bYSize);
	this->boundingBoxResolution(baux.x1_,baux.y1_,baux.x2_,baux.y2_,resx_,resy_);
}

//! Calculates the *bounding box* when box coordinates are relative to the center of the pixel
TeBox	
TeRasterParams::boundingBox ()
{ return TeBox (box_.x1()-resx_/2.,
				box_.y1()-resy_/2.,
				box_.x2()+resx_/2.,
				box_.y2()+resy_/2.);
}

void	
TeRasterParams::boundingBoxResolution (double x1, double y1, double x2, double y2, double resx, double resy, TeBox::TeBoxCorner lock)
{
	double xx2 = x2; //FAMI

	resx_ = resx;
	resy_ = resy;
	ncols_  = (int)((x2-x1)/resx_+0.5);
//FAMI BEGIN
	if ( (x1+360.) == x2 ) //around the globe
	{
		ncols_--;
		xx2 = x2 - resx_;
	}
//FAMI END
	nlines_ = (int)((y2-y1)/resy_+0.5);

	if (lock == TeBox::TeUPPERLEFT)
		box_ = TeBox(x1+0.5*resx_, y2-nlines_*resy_+0.5*resy_,
		             x1+ncols_*resx_-0.5*resx_, y2-0.5*resy_);

	else if (lock == TeBox::TeLOWERLEFT)
		box_ = TeBox(x1+0.5*resx_, y1+0.5*resy_,
		             x1+ncols_*resx_-0.5*resx_, y1+nlines_*resy_-0.5*resy_);

	else if (lock == TeBox::TeUPPERRIGHT)
//FAMI		box_ = TeBox(x2-ncols_*resx_+0.5*resx_, y2-nlines_*resy_+0.5*resy_,
//FAMI			         x2-0.5*resx_,y2-0.5*resy_);
		box_ = TeBox(xx2-ncols_*resx_+0.5*resx_, y2-nlines_*resy_+0.5*resy_,
			         xx2-0.5*resx_,y2-0.5*resy_);

	else		// TeLOWERRIGHT
//FAMI		box_ = TeBox(x2-ncols_*resx_+0.5*resx_,y1+0.5*resy_,
//FAMI		             x2-0.5*resx_,y1+nlines_*resy_-0.5*resy_);
		box_ = TeBox(xx2-ncols_*resx_+0.5*resx_,y1+0.5*resy_,
		             xx2-0.5*resx_,y1+nlines_*resy_-0.5*resy_);
}

void	
TeRasterParams::boundingBoxLinesColumns (double x1, double y1, double x2, double y2, 
										 int nlines, int ncols, TeBox::TeBoxCorner lock)
{
	nlines_ = nlines;
	ncols_ = ncols;
	
	resx_ = (x2-x1)/ncols_;
	resy_ = (y2-y1)/nlines_;

	if (lock == TeBox::TeUPPERLEFT)
		box_ = TeBox(x1+0.5*resx_, y2-nlines_*resy_+0.5*resy_,
		             x1+ncols_*resx_-0.5*resx_, y2-0.5*resy_);

	else if (lock == TeBox::TeLOWERLEFT)
		box_ = TeBox(x1+0.5*resx_, y1+0.5*resy_,
		             x1+ncols_*resx_-0.5*resx_, y1+nlines_*resy_-0.5*resy_);

	else if (lock == TeBox::TeUPPERRIGHT)
		box_ = TeBox(x2-ncols_*resx_+0.5*resx_, y2-nlines_*resy_+0.5*resy_,
			         x2-0.5*resx_,y2-0.5*resy_);

	else		// TeLOWERRIGHT
		box_ = TeBox(x2-ncols_*resx_+0.5*resx_,y1+0.5*resy_,
		             x2-0.5*resx_,y1+nlines_*resy_-0.5*resy_);
}

void	
TeRasterParams::boxResolution (double x1, double y1, double x2, double y2, 
							   double resx, double resy, TeBox::TeBoxCorner lock)
{
	resx_ = resx;
	resy_ = resy;

	ncols_  = (int)((x2-x1)/resx_+0.45)+1;
	nlines_ = (int)((y2-y1)/resy_+0.45)+1;

	if (lock == TeBox::TeUPPERLEFT)
		box_ = TeBox(x1, y2-(nlines_-1)*resy_,x1+(ncols_-1)*resx_,y2);

	else if (lock == TeBox::TeLOWERLEFT)
		box_ = TeBox(x1, y1,x1+(ncols_-1)*resx_,y1+(nlines_-1)*resy_);

	else if (lock == TeBox::TeUPPERRIGHT)
		box_ = TeBox(x2-(ncols_-1)*resx_, y2-(nlines_-1)*resy_,x2,y2);

	else		// TeLOWERRIGHT
		box_ = TeBox(x2-(ncols_-1)*resx_,y1,x2,y1+(nlines_-1)*resy_);
}

void	
TeRasterParams::boxLinesColumns (double x1, double y1, double x2, double y2, 
								 int nlines, int ncols, TeBox::TeBoxCorner lock)
{
	nlines_ = nlines;
	ncols_ = ncols;

	resx_ = (x2-x1)/(ncols_-1);
	resy_ = (y2-y1)/(nlines_-1);

	if (lock == TeBox::TeUPPERLEFT)
		box_ = TeBox(x1, y2-(nlines_-1)*resy_,x1+(ncols_-1)*resx_,y2);

	else if (lock == TeBox::TeLOWERLEFT)
		box_ = TeBox(x1, y1,x1+(ncols_-1)*resx_,y1+(nlines_-1)*resy_);

	else if (lock == TeBox::TeUPPERRIGHT)
		box_ = TeBox(x2-(ncols_-1)*resx_, y2-(nlines_-1)*resy_,x2,y2);

	else		// TeLOWERRIGHT
		box_ = TeBox(x2-(ncols_-1)*resx_,y1,x2,y1+(nlines_-1)*resy_);
}

void	
TeRasterParams::topLeftResolutionSize (double left, double top, 
									   double resx, double resy, 
									   int ncol, int nlin, 
									   bool coordIsCentrePixel)
{
	resx_ = resx;
	resy_ = resy;
	ncols_  = ncol;
	nlines_ = nlin;
	
	if (coordIsCentrePixel)
		box_ = TeBox(left,top-(nlin-1)*resy_,left+(ncol-1)*resx_,top);
	else
	{
		TeBox bb(left,top-nlines_*resy_,left+ncols_*resx_,top);
		box_ = TeBox(bb.x1()+resx_/2.,
					 bb.y1()+resy_/2.,
					 bb.x2()-resx_/2.,
					 bb.y2()-resy_/2.);
	}
}

void	
TeRasterParams::lowerLeftResolutionSize (double left, double lower, 
										 double resx, double resy, 
										 int ncol, int nlin, 
										 bool coordIsCentrePixel)
{
	resx_ = resx;
	resy_ = resy;
	ncols_  = ncol;
	nlines_ = nlin;
	
	if (coordIsCentrePixel)
		box_ = TeBox(left,lower,left+(ncol-1)*resx_,lower+(nlin-1)*resy_);
	else
	{
		TeBox bb(left,lower,left+ncols_*resx_,lower+nlines_*resy_);
		box_ = TeBox(bb.x1()+resx_/2.,
					 bb.y1()+resy_/2.,
					 bb.x2()-resx_/2.,
					 bb.y2()-resy_/2.);
	}
}

void 
TeRasterParams::nBands(int n)
{
  TEAGN_TRUE_OR_THROW( ( n > 0 ), "Invalid number of bands" )
  
	if (nbands_ != n)
	{
		if ( nbands_ == 0 ) {
		  nbitsperPixel_.resize(nbands_);
		  vmin_.resize(nbands_);
		  vmax_.resize(nbands_);
		  bands_.resize(nbands_);
		  photometric_.resize(nbands_);
		  dataType_.resize(nbands_);
		  dummy_.resize(nbands_);
		  compression_.resize(nbands_);
		  double d = dummy_[0];
		  for (int i = 0; i<n; i++)
		  {
			  bands_[i] = i;
			  vmin_[i] = TeMAXFLOAT;
			  vmax_[i] = -TeMAXFLOAT;
			  dataType_[i] = TeUNSIGNEDCHAR;
			  photometric_[i] = TeRasterParams::TeUnknown;
			  dummy_[i] = d;
		  }
		} else if ( nbands_ > n ) {
		  for( unsigned int count = ( nbands_ - n ) ; count > 0 ; --count ) {
		    nbitsperPixel_.pop_back();
		    vmin_.pop_back();
		    vmax_.pop_back();
		    bands_.pop_back();
		    photometric_.pop_back();
		    dataType_.pop_back();
		    dummy_.pop_back();
		    compression_.pop_back();		  
		  }
		} else {
		  /* nbands_ < n */
		
		  for( unsigned int count = ( n - nbands_ ) ; count > 0 ; --count ) {
		    nbitsperPixel_.push_back( nbitsperPixel_[ nbands_ - 1 ] );
		    vmin_.push_back( vmin_[ nbands_ - 1 ] );
		    vmax_.push_back( vmax_[ nbands_ - 1 ] );
		    bands_.push_back( bands_[ nbands_ - 1 ] );
		    photometric_.push_back( photometric_[ nbands_ - 1 ] );
		    dataType_.push_back( dataType_[ nbands_ - 1 ] );
		    dummy_.push_back( dummy_[ nbands_ - 1 ] );
		    compression_.push_back( compression_[ nbands_ - 1 ] );  
		  }		
		}
		
		nbands_ = n;
	}
}

void 
TeRasterParams::projection(TeProjection* proj)
{
	if (projection_)
	{
		delete projection_;
		projection_ = 0;
	}
	if (proj)
		projection_ = TeProjectionFactory::make(proj->params());
}

void 
TeRasterParams::setDataType(TeDataType type, int band)
{
	int nb = 8;
	double defValue = 0.0;
	switch (type) {
	case TeUNSIGNEDCHAR:
		nb = sizeof(unsigned char)*8;
		defValue = 255;
		break;
	case TeCHAR :
		defValue = 255;
		nb = sizeof(char)*8;
		break;
	case TeUNSIGNEDSHORT:
		nb = sizeof(unsigned short)*8;
		defValue = pow(2.,nb)-1;
		break;
	case (TeSHORT):
		nb = sizeof(short)*8;
		defValue = pow(2.,nb)-1;
		break;
	case (TeUNSIGNEDLONG):
		nb = sizeof(unsigned long)*8;
		defValue = pow(2.,nb)-1;
		break;
	case (TeLONG):
		nb = sizeof(long)*8;
		break;
	case (TeFLOAT):
		nb = sizeof(float)*8;
		defValue = pow(2.,nb)-1;
		break;
	case (TeDOUBLE):
		nb = sizeof(double)*8;
		defValue = TeMAXFLOAT;
        break;
    default:
        break;
	}
	if (band<0 || band>= (int)dataType_.size())
		for (unsigned int i=0; i<dataType_.size();++i)
		{
			dataType_[i]=type;
			nbitsperPixel_[i]=nb;
			if (!hasSetDummy_)
				dummy_[i]=defValue;
		}
	else
	{
		dataType_[band]=type;
		nbitsperPixel_[band]=nb;
		dummy_[band]=defValue;
	}
}

int TeRasterParams::elementSize(int band)
{
	switch (dataType_[band]) {
	case TeUNSIGNEDCHAR:
		return sizeof(unsigned char);
	case TeCHAR :
		return sizeof(char);
	case TeUNSIGNEDSHORT:
		return sizeof(unsigned short);
	case (TeSHORT):
		return sizeof(short);
	case TeINTEGER:
		return sizeof(int);
	case (TeUNSIGNEDLONG):
		return (sizeof(unsigned long));
	case (TeLONG):
		return (sizeof(long));
	case (TeFLOAT):
		return (sizeof(float));
	default:
		return (sizeof(double));
	}
}


void 
TeRasterParams::setPhotometric(TeRasterParams::TeRasterPhotometricInterpretation photom, int band)
{
	if (band<0 || band>= (int)photometric_.size())
		for (unsigned int i=0; i<photometric_.size(); photometric_[i]=photom,i++);
	else
		photometric_[band]=photom;
}

void 
TeRasterParams::setCompressionMode(TeRasterParams::TeRasterCompressionMode cmode, int band)
{
	if (band<0 || band>= (int)compression_.size())
		for (unsigned int i=0; i<compression_.size(); compression_[i]=cmode,i++);
	else
		compression_[band] = cmode;
}

void 
TeRasterParams::setDummy(double dummy, int band)
{
	if (band<0 || band>= (int)dummy_.size())
		for (unsigned int i=0; i<dummy_.size(); dummy_[i]=dummy,i++);
	else
		dummy_[band] = dummy;
	this->useDummy_ = true;
	hasSetDummy_ = true;
}

TeCoord2D 
TeRasterParams::coord2Index (const TeCoord2D& pt) const
{
	double i, j;
	if (dxI_ != 0 || dyJ_ != 0)
	{
		double x = pt.x();
		double y = pt.y();
		
		i = (dyJ_*(x-x0_)-dxJ_*(y-y0_))/(dxI_*dyJ_-dxJ_*dyI_);
		j = (x-x0_-i*dxI_)/dxJ_;
		return TeCoord2D (j,i);
	}
	else
	{
		i = (pt.x() - box_.x1()) / resx_;
		j = (box_.y2() - pt.y()) / resy_;
		return TeCoord2D (i,j);
	}
}

TeCoord2D 
TeRasterParams::index2Coord (const TeCoord2D& pt) const
{
	double x,y;
	double i = pt.y();
	double j = pt.x();
		
	if (dxI_ != 0 || dyJ_ != 0)
	{
		x = x0_ + i*dxI_ + j*dxJ_;
		y = y0_ + i*dyI_ + j*dyJ_;
	}
	else
	{
		x = box_.x1() + pt.x() * resx_;
		y = box_.y2() - pt.y() * resy_;
	}
	return TeCoord2D (x,y);
}

void
TeRasterParams::writeParametersFile()
{
	string metFileName = TeGetName (fileName_.c_str())+".met";
	try {
		TeAsciiFile metFile(metFileName,"w");
		string line = "%Raw raster file metadata\n";
		metFile.writeString(line);
		line = "NROWS " + Te2String(nlines_);
		metFile.writeString(line);
		metFile.writeNewLine();
		line = "NCOLS " + Te2String(ncols_);
		metFile.writeString(line);
		metFile.writeNewLine();
		line = "NBANDS " + Te2String(this->nBands());
		metFile.writeString(line);
		metFile.writeNewLine();

		if (this->nBands() > 1)
		{
			line = "INTERLEAVING ";
			if (interleaving_ == TeRasterParams::TePerPixel)
				line += "BIP";
			else if (interleaving_ == TeRasterParams::TePerLine)
				line += "BIL";
			else
				line += "BSQ";
			metFile.writeString(line);
			metFile.writeNewLine();
		}

		int ndecimals = 0;
		line = "DATATYPE ";
		if (dataType_[0] == TeBIT)
			line += "bit";
		else if (dataType_[0] == TeUNSIGNEDCHAR)
			line += "unsignedchar";
		else if (dataType_[0] == TeCHAR)
			line += "char";
		else if (dataType_[0] == TeUNSIGNEDSHORT)
			line += "unsignedshort";
		else if (dataType_[0] == TeSHORT)
			line += "short";
		else if (dataType_[0] == TeINTEGER)
			line += "integer";
		else if (dataType_[0] == TeUNSIGNEDLONG)
			line += "unsignedlong";
		else if (dataType_[0] == TeLONG)
			line += "long";
		else if (dataType_[0] == TeFLOAT)
		{
			line += "float";
			ndecimals = 6;
		}
		else if (dataType_[0] == TeDOUBLE)
		{
			line += "double";	
			ndecimals = 10;
		}
		metFile.writeString(line);
		metFile.writeNewLine();
		
		if (swap_)
		{
			line = "SWAPPED yes";
			metFile.writeString(line);
			metFile.writeNewLine();
		}
		else
			line.clear();
		if (useDummy_)
		{
			line += "NO_DATA " + Te2String(dummy_[0], ndecimals);
			metFile.writeString(line);
			metFile.writeNewLine();
		}
    
    if( projection_ ) {
      if (projection_->name() == "NoProjection" ||
        projection_->name() == "LatLong") {
        
        ndecimals = 8;
      } else {
        ndecimals = 3;
      }
    } else {
      ndecimals = 8;
    }

		line = "RESOLUTION_X " + Te2String(resx_,ndecimals);
		metFile.writeString(line);
		metFile.writeNewLine();
		line = "RESOLUTION_Y " + Te2String(resy_,ndecimals);
		metFile.writeString(line);
		metFile.writeNewLine();
		line = "LOWERLEFT_X " + Te2String(this->box().x1_,ndecimals);
		metFile.writeString(line);
		metFile.writeNewLine();
		line = "LOWERLEFT_Y " + Te2String(this->box().y1_,ndecimals);
		metFile.writeString(line);
		metFile.writeNewLine();
		if (this->projection())
		{
			line = "PROJECTION ";
			line += this->projection()->describe();
			metFile.writeString(line);
			metFile.writeNewLine();
			line = "DATUM " +  this->projection()->datum().name();
			metFile.writeString(line);
		}
	}
	catch(...)
	{
	}
}



void 
TeRasterParams::readParametersFile()
{
	// read all keys in the metadata file (there is no order expected) 
	string metFileName = TeGetName (fileName_.c_str())+".met";
	map<string,string> metadata;
	try {
		TeAsciiFile metFile(metFileName,"r");
		string key, value;
		while (metFile.isNotAtEOF())
		{
			key = metFile.readString();
			if (key[0] == '%')
			{
				metFile.findNewLine();
				continue;
			}
			value = metFile.readString();
			metadata.insert(make_pair(key,value));
			metFile.findNewLine();
		}
	}
	catch(...)
	{
	}
	if (!metadata.empty())
	{
		double xll=0, yll=0;
		map<string,string>::iterator it;
		it = metadata.find("NROWS");
		if (it != metadata.end())
			nlines_ = atoi(it->second.c_str());
		it = metadata.find("NCOLS");
		if (it != metadata.end())
			ncols_ = atoi(it->second.c_str());
		it = metadata.find("NBANDS");
		if (it != metadata.end())
			this->nBands(atoi(it->second.c_str()));
		else
			this->nBands(1);
		it = metadata.find("RESOLUTION_X");
		if (it != metadata.end())
			resx_ = atof(it->second.c_str());
		it = metadata.find("RESOLUTION_Y");
		if (it != metadata.end())
			resy_ = atof(it->second.c_str());
		it = metadata.find("LOWERLEFT_X");
		if (it != metadata.end())
		{
			xll = atof(it->second.c_str());
			it = metadata.find("LOWERLEFT_Y");
			if (it != metadata.end())
			{
				yll = atof(it->second.c_str());
				lowerLeftResolutionSize(xll,yll, resx_, resy_,ncols_, nlines_);
			}
		}
		it = metadata.find("INTERLEAVING");
		if (it != metadata.end())
		{
			if (it->second == "BIL")
				interleaving_ = TeRasterParams::TePerLine;
			else if (it->second == "BSQ")
				interleaving_ = TeRasterParams::TePerBand;
			else  interleaving_ = TeRasterParams::TePerPixel;
		}

		it = metadata.find("DATATYPE");
		if (it != metadata.end())
		{
			if (it->second == "bit")
				this->setDataType(TeBIT);
			else if (it->second == "unsignedchar")
				this->setDataType(TeUNSIGNEDCHAR);
			else if (it->second == "char")
				this->setDataType(TeCHAR);
			else if (it->second == "unsignedshort")
				this->setDataType(TeUNSIGNEDSHORT);
			else if (it->second == "short")
				this->setDataType(TeSHORT);
			else if (it->second == "integer")
				this->setDataType(TeINTEGER);
			else if (it->second == "unsignedlong")
				this->setDataType(TeUNSIGNEDLONG);
			else if (it->second == "long")
				this->setDataType(TeLONG);
			else if (it->second == "float")
				this->setDataType(TeFLOAT);
			else if (it->second == "double")
				this->setDataType(TeDOUBLE);
		}
		it = metadata.find("SWAPPED");
		if (it != metadata.end() && it->second == "yes")
			swap_ = true;

		it = metadata.find("NO_DATA");
		if (it != metadata.end())
		{
			useDummy_ = true;
			this->setDummy(atof(it->second.c_str()));
		}
			
		it = metadata.find("PROJECTION");
		if (it != metadata.end())
		{
			string projdesc = it->second;
			TeProjectionParams pars;
			it = metadata.find("DATUM");
			if (it != metadata.end())
			{
				TeDatum dat = TeDatumFactory::make(it->second);
				pars.datum = dat;
			}
			if (decodifyDescription(projdesc,pars))
			{
				TeProjection* proj = TeProjectionFactory::make(pars);
				this->projection(proj);
			}
		}
	}
}

void 
TeRasterParams::setNavigationParameters(const vector<double>& nwf)
{
	if (nwf.size() < 6)
		return;

	dxJ_ = nwf[0];
	dxI_ = nwf[1];
	dyJ_ = nwf[2];
	dyI_ = nwf[3];
	x0_ = nwf[4];
	y0_ = nwf[5];
	resx_ = dxJ_;
	resy_ = ABS(dyI_);

	if (nwf[1] == 0. && nwf[2] == 0)
	{
		this->topLeftResolutionSize(nwf[4],nwf[5],nwf[0],-1*nwf[3],ncols_,nlines_);
		return;
	}

	// set the bounding box considering the navigation parameters 
	// assumes that: x0 and y0 are relative to the center of the raster element

	double xmin = min(x0_-0.5*dxJ_-0.5*dxI_,x0_-0.5*dxJ_+(nlines_-0.5)*dxI_);
	double xmax = max(x0_+(ncols_-0.5)*dxJ_-0.5*dxI_,x0_+(ncols_-0.5)*dxJ_+(nlines_-0.5)*dxI_);
	double ymin = min(y0_+(nlines_-0.5)*dyI_-0.5*dyJ_,y0_+(nlines_-0.5)*dyI_+(ncols_*0.5)*dyJ_);
	double ymax = max(y0_-0.5*dyI_-0.5*dyJ_,y0_-0.5*dyI_+(ncols_*0.5)*dyJ_);
	boundingBoxLinesColumns(xmin,ymin,xmax,ymax,nlines_,ncols_);
}

void 
TeRasterParams::getNavigationParameters(vector<double>& nwf)
{
	nwf.clear();
	if (dxI_ != 0. || dyJ_ != 0.)
	{
		nwf.push_back(dxJ_);
		nwf.push_back(dxI_);
		nwf.push_back(dyJ_);
		nwf.push_back(dyI_);
		nwf.push_back(x0_);
		nwf.push_back(y0_);
	}
	else
	{
		nwf.push_back(resx_);
		nwf.push_back(0.);
		nwf.push_back(0.);
		nwf.push_back(-1*resy_);
		nwf.push_back(box_.x1_);
		nwf.push_back(box_.y2_);
	}
}

void 
TeRasterParams::setNLinesNColumns(int nlines, int ncolumns)
{
	this->lowerLeftResolutionSize(0.5,0.5,1,1,ncolumns, nlines);
}

