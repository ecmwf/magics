/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright ¨ 2001-2004 INPE and Tecgraf/PUC-Rio.

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
#include <TeDecoderJPEG.h>
#include <TeAsciiFile.h>
#include <TeUtils.h>
#include <TeLibJpegWrapper.h>

#include <fstream>
using std::ifstream;
using std::ofstream;

TeDecoderJPEG::TeDecoderJPEG():
	isModified_(false),
	size_(0),
	myData_(0)
{	
	params_.decoderIdentifier_ = "JPEG";	
}


TeDecoderJPEG::TeDecoderJPEG (const TeRasterParams& par):
	isModified_(false),
	size_(0),
	myData_(0)
{	
	params_ = par; 
	params_.errorMessage_.clear();
	// 'w' and 'r' modes suppose that the file already exists: try to read the basic information
	if (params_.mode_ == 'w' || params_.mode_ == 'r')
	{
		ifstream dataFile(params_.fileName_.c_str());
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to access the raster file:" + params_.fileName_;
			return;
		}
		dataFile.close();
		
		int nb;
		if (!Jpeg::ReadFileParams(params_.fileName_, params_.ncols_, params_.nlines_, nb))
		{
			params_.errorMessage_ = "Fail to read the parameters from the raster the file.";
			return;
		}
		params_.nBands(nb);
		params_.setDataType(TeUNSIGNEDCHAR);
		params_.setPhotometric(TeRasterParams::TeMultiBand);
		params_.decoderIdentifier_ = "JPEG";
		params_.blockWidth_ = params_.ncols_;
		params_.blockHeight_ = params_.nlines_;
		params_.interleaving_ = TeRasterParams::TePerPixel;
		TeProjection* proj = new TeNoProjection();
		params_.projection(proj);
		delete proj;

		// Try reading navigation parameters from auxiliary file
		string jgwFile = TeGetName(params_.fileName_.c_str()) +".jgw";
		try
		{
			vector<double> jgw;
			TeAsciiFile	pFile(jgwFile.c_str());
			jgw.push_back(pFile.readFloat());
			jgw.push_back(pFile.readFloat());
			jgw.push_back(pFile.readFloat());
			jgw.push_back(pFile.readFloat());
			jgw.push_back(pFile.readFloat());
			jgw.push_back(pFile.readFloat());
			params_.setNavigationParameters(jgw);
		}
		catch (...)
		{
			// no .jgw inexistent or inconsistent, uses resolution of 1 unit 
			// and box lower left coordinate of (0.5,0.5)
			params_.lowerLeftResolutionSize(0.5,0.5,1.0,1.0,params_.ncols_,params_.nlines_,true);
		}
	}
}

TeDecoderJPEG::~TeDecoderJPEG()
{
	clear();
}


bool
TeDecoderJPEG::clear()
{
	size_ = 0;
	if (myData_ == 0)
	    return true;
	if ( isModified_ && (params_.mode_ == 'w' || params_.mode_ == 'c') )	// save contents to disk
	{
		ofstream dataFile(params_.fileName_.c_str());
		if (dataFile)
		{
			dataFile.close();
			Jpeg::CompressToFile(myData_, params_.ncols_, params_.nlines_, params_.nBands(), params_.fileName_);
			isModified_ = false;
		}
	}
	delete [] myData_;
	myData_ = 0;
	return true;
}


void
TeDecoderJPEG::init()
{
	params_.status_= TeRasterParams::TeNotReady;
	params_.interleaving_ = TeRasterParams::TePerPixel;

	// try to allocate enough memory to hold the data
	if (size_ != params_.nlines_*params_.ncols_*params_.nBands())
	{
		size_ = params_.nlines_*params_.ncols_*params_.nBands();
		if (myData_)
			delete [] myData_;
		myData_ = new unsigned char [size_];
	}
	if (!myData_)
		return;

	for (long i=0; i<size_; myData_[i]=static_cast<unsigned char>(params_.dummy_[0]),++i);

	if (params_.mode_ == 'c')	// creating a new file
	{
		int nb = params_.nBands();
		if (nb != 1 && nb != 3)
		{
			params_.errorMessage_ = "JPEG files can have only 1 or 3 bands.";
			return;
		}
		ofstream dataFile(params_.fileName_.c_str());
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to (re)create the raster the file:" + params_.fileName_;
			return;
		}
		dataFile.close();
		
		// write navigation file
		string jgw = TeGetName(params_.fileName_.c_str())+".jgw";
		try {
			TeAsciiFile jgwFile(jgw,"w");
			vector<double> jgw;
			params_.getNavigationParameters(jgw);
			for (unsigned int nl=0; nl<6; ++nl)
			{
				string line;
				line = Te2String(jgw[nl]) + "\n";
				jgwFile.writeString(line);
			}
		}
		catch(...)
		{}
		// write initial dummy  data
		if (!Jpeg::CompressToFile(myData_, params_.ncols_, params_.nlines_, params_.nBands(), params_.fileName_))
			return;
		params_.status_ = TeRasterParams::TeReadyToWrite;
	}
	else if (params_.mode_ == 'w' || params_.mode_ == 'r')
	{
		ifstream dataFile(params_.fileName_.c_str());
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to open the raster the file:" + params_.fileName_;
			return;
		}
		dataFile.close();
		
		int nb;
		if (!Jpeg::DecompressFile(params_.fileName_.c_str(), myData_, params_.ncols_, params_.nlines_, nb))
		{
			params_.errorMessage_ = "Fail to decompress JPEG file.";
			return;
		}
		params_.nBands(nb);
		if (params_.mode_ == 'w')
			params_.status_ = TeRasterParams::TeReadyToWrite;
		else
			params_.status_ = TeRasterParams::TeReadyToRead;	
	}
}

bool 
TeDecoderJPEG::getElement (int col,int lin, double& val,int band)
{
	if (col < 0 || lin < 0 || col >= params_.ncols_ || lin >= params_.nlines_)
		return false;
	
	int position = params_.nBands()*(params_.ncols_*lin+col)+band;
	val = myData_[position];
	return true;
}

bool 
TeDecoderJPEG::setElement (int col, int lin, double val,int band)
{
	if (col < 0 || lin < 0 || col >= params_.ncols_ || lin >= params_.nlines_)
		return false;
	int position = params_.nBands()*(params_.ncols_*lin+col)+band;
	myData_[position] = static_cast<unsigned char>(val);
	isModified_ = true;
	return true;
}

TeDecoderJPEGFactory::TeDecoderJPEGFactory(const string& name): 
	TeDecoderFactory(name) 
{
	TeDecoderFactory::instanceName2Dec()["JPG"]  = "JPEG";	
	TeDecoderFactory::instanceName2Dec()["JPEG"] = "JPEG";
}
