/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include "TeException.h"
#include "TeDecoderASCIIGrid.h"
#include "TeAsciiFile.h"
#include "TeUtils.h"

#include <cstring>
#include <fstream>
using std::ifstream;
using std::ofstream;

/*
Expected format for ESRI ASCII GRID Files (Copied from the ArcWorkstation 8.3 Help File):

<NCOLS xxx>
<NROWS xxx>
<XLLCENTER xxx | XLLCORNER xxx>
<YLLCENTER xxx | YLLCORNER xxx>
<CELLSIZE xxx>
{NODATA_VALUE xxx}
row 1
row 2
.
.
.
row n
*/

TeDecoderASCIIGrid::TeDecoderASCIIGrid (const TeRasterParams& par) : 
	isModified_(false),
	size_(0),
	myData_(0)
{	
	params_.errorMessage_.clear();
	if (par.fileName_.empty())
		return;
	params_ = par;
	params_.decoderIdentifier_ = "ASCIIGRID";
	if (params_.mode_ == 'w' || params_.mode_ == 'r')
		readParameters();
}

TeDecoderASCIIGrid::~TeDecoderASCIIGrid ()
{
	if ( isModified_ && (params_.mode_ == 'w' || params_.mode_ == 'c'))
	{			// save contents to disk
		try
		{
			TeAsciiFile sFile(params_.fileName_,"w+");
			if (writeParameters(sFile))
				saveData(sFile);
		}
		catch (...)
		{
		}
	}
	delete [] myData_;
}

bool
TeDecoderASCIIGrid::clear()
{
	if ( isModified_ && (params_.mode_ == 'w' || params_.mode_ == 'c'))
	{			// save contents to disk
		try
		{
			TeAsciiFile sFile(params_.fileName_,"w+");
			if (writeParameters(sFile))
				saveData(sFile);
		}
		catch (...)
		{
		}
		isModified_ = false;
	}
	if (myData_)
		delete [] myData_;
	myData_ = 0;
	return true;
}

bool 
TeDecoderASCIIGrid::readParameters()
{
	try
	{
		TeAsciiFile	pFile (params_.fileName_);
		string name;
		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"NCOLS"))
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		params_.ncols_ = pFile.readInt();
		pFile.findNewLine();

		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"NROWS"))
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		params_.nlines_ = pFile.readInt();
		pFile.findNewLine();

		bool isCenter = false;
		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"XLLCENTER"))
			{
				isCenter = true; 
				break;
			}
			else if (TeStringCompare(name,"XLLCORNER"))
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		double llx = pFile.readFloat();
		pFile.findNewLine();

		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"YLLCENTER"))
			{
				isCenter = true; 
				break;
			}
			else if (TeStringCompare(name,"YLLCORNER"))
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		double lly = pFile.readFloat();	
		pFile.findNewLine();

		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"CELLSIZE"))
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		params_.resx_ = params_.resy_ = pFile.readFloat();		
		pFile.findNewLine();

		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (TeStringCompare(name,"NODATA_VALUE"))
				break;
		}
		double dummy = -9999.0;
		if (pFile.isNotAtEOF())					// unexpected end of file: NODATA statment is not present
			dummy = pFile.readFloat();		
		params_.nBands(1);
		params_.setDataType(TeFLOAT);		
		params_.setDummy(dummy);
		params_.setPhotometric(TeRasterParams::TeMultiBand);
		params_.lowerLeftResolutionSize(llx,lly,params_.resx_,params_.resy_,
				                      params_.ncols_,params_.nlines_,isCenter);
		TeProjection* pp = new TeNoProjection();
		params_.projection(pp);
		delete pp;
	}
	catch(...)
	{
		params_.errorMessage_ = "Fail to read the parameters from the raster the file.";
		return false;
	}
	return true;
}

bool
TeDecoderASCIIGrid::readFile(const string& filename)
{
	string valstring;
	try 
	{
		TeAsciiFile	pFile (filename);
		string name;
		while (pFile.isNotAtEOF() && !TeStringCompare(name,"NODATA_VALUE"))
			name = pFile.readString();
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		pFile.findNewLine();
		double val;
		for (int lin = 0; lin < params_.nlines_; ++lin)
		{
			for (int col = 0; col < params_.ncols_; ++col)
			{
				val = pFile.readFloat();
				if (val != params_.dummy_[0])
				{
					setElement(col,lin,val);
					if (val < params_.vmin_[0])
						params_.vmin_[0] = val;
					if (val > params_.vmax_[0])
						params_.vmax_[0] = val;
				}
			}
		}
	}
	catch(...)
	{
		params_.errorMessage_ = "Fail to read the data from the raster the file.";
		return false;
	}
	return true;
}

void
TeDecoderASCIIGrid::init()
{
	params_.status_= TeRasterParams::TeNotReady;
	int nb = params_.nBands();
	if (nb != 1 )
	{
		params_.errorMessage_ = "Ascii-GRID can store raster with only 1 band.";
		return;
	}

	// try to allocate enough memory to hold the data
	if (size_ != params_.nlines_*params_.ncols_*params_.nBands())
	{
		size_ = params_.nlines_*params_.ncols_*params_.nBands();
		if (myData_)
			delete [] myData_;
		myData_ = new float [size_];
	}
	if (!myData_)
		return;

	for (long i=0; i<size_; myData_[i]=static_cast<float>(params_.dummy_[0]),++i);

	if (params_.mode_ == 'c')	// creating a new file
	{
		ofstream dataFile(params_.fileName_.c_str()); // try to (re)create the file
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to (re)create the raster the file:" + params_.fileName_;
			return;
		}
		dataFile.close();
		try
		{
			TeAsciiFile sFile(params_.fileName_,"w+");
			if (!writeParameters(sFile) || !saveData(sFile))
			{
				params_.errorMessage_ = "Fail to write the raster to file";
				return;
			}
			else
			{
				params_.status_ = TeRasterParams::TeReadyToWrite;
			}
		}
		catch (...)
		{
			params_.errorMessage_ = "Fail to write the raster to file";
			return;
		}
	}
	else if (params_.mode_ == 'w')
	{
		ifstream dataFile(params_.fileName_.c_str()); // check if file exists
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to open the raster the file:" + params_.fileName_;
			return; 
		}
		dataFile.close();	
		if (!readFile(params_.fileName_))
		{
			params_.errorMessage_ = "Fail to read the raster the file:" + params_.fileName_;
			return; 
		}
		params_.status_ = TeRasterParams::TeReadyToWrite;
	}
	else if (params_.mode_ == 'r')
	{
		ifstream dataFile(params_.fileName_.c_str()); // check if file exists
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to open the raster the file:" + params_.fileName_;
			return; 
		}
		dataFile.close();	
		if (!readFile(params_.fileName_))
		{
			params_.errorMessage_ = "Fail to read the raster the file:" + params_.fileName_;
			return; 
		}
		params_.status_ = TeRasterParams::TeReadyToRead;	
	}
}

bool 
TeDecoderASCIIGrid::writeParameters(TeAsciiFile& pFile)
{
	string name;
	try
	{
		TeBox box = params_.box();
		name = "NCOLS " + Te2String(params_.ncols_) + "\n";
		pFile.writeString(name);
		name = "NROWS " + Te2String(params_.nlines_) + "\n";
		pFile.writeString(name);
		name = "XLLCENTER " + Te2String(box.x1_,6) + "\n";
		pFile.writeString(name);
		name = "YLLCENTER " + Te2String(box.y2_,6) + "\n";
		pFile.writeString(name);
		name = "CELLSIZE " + Te2String(params_.resx_,6) + "\n";
		pFile.writeString(name);
		name = "NODATA_VALUE " + Te2String(params_.dummy_[0],6) + "\n";
		pFile.writeString(name);
	}
	catch (...)
	{
		params_.errorMessage_ = "Fail to write raster parameters to the file";
		return false;
	}
	return true;
}

bool 
TeDecoderASCIIGrid::saveData(TeAsciiFile& pFile)
{
	bool isi = (params_.dataType_[0] != TeDOUBLE) && (params_.dataType_[0] != TeFLOAT);

	FILE* fp = pFile.FilePtr();
	char fmt[100];
	if (isi)
		strcpy(fmt,"%.0f ");
	else
		strcpy(fmt,"%f ");
	try
	{
		double d;
		for (int l=0; l<params_.nlines_; ++l)
		{
			for (int c=0; c<params_.ncols_; ++c)
			{
				getElement(c,l,d);
				fprintf(fp,fmt,d);
			}
			pFile.writeNewLine();
		}
	}
	catch(...)
	{
		params_.errorMessage_ = "Fail to write raster data to the file";
		return false;
	}
	return true;
}

bool 
TeDecoderASCIIGrid::getElement (int col,int lin, double& val,int band)
{
	if (col < 0 || lin < 0 || col >= params_.ncols_ || lin >= params_.nlines_)
		return false;
	
	int position = params_.nBands()*(params_.ncols_*lin+col)+band;
	val = myData_[position];
	return true;
}

bool 
TeDecoderASCIIGrid::setElement (int col, int lin, double val,int band)
{
	if (col < 0 || lin < 0 || col >= params_.ncols_ || lin >= params_.nlines_)
		return false;
	int position = params_.nBands()*(params_.ncols_*lin+col)+band;
	myData_[position] = static_cast<float>(val);
	isModified_ = true;
	return true;
}

TeDecoderASCIIGridFactory::TeDecoderASCIIGridFactory(const string& name): 
	TeDecoderFactory(name) 
{
	TeDecoderFactory::instanceName2Dec()["GRD"]  = "ASCIIGRID";	
	TeDecoderFactory::instanceName2Dec()["ASC"]  = "ASCIIGRID";	
	TeDecoderFactory::instanceName2Dec()["TXT"]  = "ASCIIGRID";	
}
