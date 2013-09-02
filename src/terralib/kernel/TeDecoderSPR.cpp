/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
#include "TeDecoderSPR.h"
#include "TeAsciiFile.h"
#include "TeUtils.h"

#include <cstdlib>
#include <cstring>
#include <fstream>
using std::ifstream;
using std::ofstream;

TeDecoderSPR::TeDecoderSPR() : 
	TeDecoderSmartMem(),
	isModified_(false)
{	
	params_.decoderIdentifier_ = "SPR";	
}

TeDecoderSPR::TeDecoderSPR (const TeRasterParams& par) : 
	TeDecoderSmartMem(),
	isModified_(false)
{	
	params_.errorMessage_.clear();
	if (par.fileName_.empty())
		return;
	params_ = par;
	params_.decoderIdentifier_ = "SPR";
	if (params_.mode_ == 'w' || params_.mode_ == 'r')
		readParameters();
}

TeDecoderSPR::~TeDecoderSPR()
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
	TeDecoderSmartMem::clear();
}

bool
TeDecoderSPR::clear()
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
	TeDecoderSmartMem::clear();
	return true;
}

bool 
TeDecoderSPR::readParameters()
{
	try
	{
		TeAsciiFile	pFile (params_.fileName_);
		string name;

		// check if there is the GRIDDEF section
		while (pFile.isNotAtEOF())
		{
			name = pFile.readString();
			if (name[0] =='/' && name[1] == '/')	// skip comments
				pFile.findNewLine();
			else if (name == "GRIDDEF")
				break;
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;
		
		// read INFO section
		double x1, y2;
		int ncols;
		double dummy;

		// Expected format to grid definition:
		// GRIDDEF  <ncols>  <nlins>  <X1>  <Y2>  <resX>  <resY>  <nodatavalue>
		params_.ncols_ = ncols = pFile.readInt();
		params_.nlines_ = pFile.readInt();
		x1 = pFile.readFloat();
		y2 = pFile.readFloat();
		params_.resx_ = pFile.readFloat();
		params_.resy_ = pFile.readFloat();
		params_.nBands(1);
		dummy = pFile.readFloat();
		params_.useDummy_ = true;
		params_.setDummy(dummy);

		do
		{
			name = pFile.readString();
			if (name[0] =='/' && name[1] == '/')	// skip comments
				pFile.findNewLine();
		} while (pFile.isNotAtEOF() && name != "GRIDCLASS" && name != "INFO_END");
		
		if (name == "GRIDCLASS" )
		{
			params_.setPhotometric(TeRasterParams::TePallete);
			params_.setDataType(TeUNSIGNEDCHAR);

			// Expected format GRIDCLASS  <nclass>  <ncolorR>  <ncolorG>  <ncolorB>  <nameclass>
			params_.lutr_.clear();
			params_.lutg_.clear();
			params_.lutb_.clear();

			params_.lutr_.insert(params_.lutr_.begin(),256,0);
			params_.lutg_.insert(params_.lutg_.begin(),256,0);
			params_.lutb_.insert(params_.lutb_.begin(),256,0);
			
			vector<string> lnames;
			name = pFile.readString();
			while (pFile.isNotAtEOF() && name != "INFO_END") // loop through all class definitions
			{
				int index = atoi(name.c_str());
				params_.lutr_[index] = pFile.readInt(); 
				params_.lutg_[index] = pFile.readInt(); 
				params_.lutb_[index] = pFile.readInt(); 
				lnames.clear();
				pFile.readStringList(lnames);   // class name			
				name = pFile.readString();	 // next index		
			} 

			if (!pFile.isNotAtEOF())	// unexpected end of file
				return false;
		}
		else if (name == "INFO_END")
		{
			params_.setDataType(TeDOUBLE);
			params_.setPhotometric(TeRasterParams::TeMultiBand);
		}
		params_.topLeftResolutionSize(x1,y2,params_.resx_,params_.resy_,
				                      params_.ncols_,params_.nlines_,false);
		TeProjection* pp = new TeNoProjection();
		params_.projection(pp);
		delete pp;
	}
	catch(...)
	{
		return false;
	}
	return true;
}

bool
TeDecoderSPR::readFile(const string& filename)
{
	string valstring;
	try 
	{
		TeAsciiFile	pFile (filename);
		string name;
		while (pFile.isNotAtEOF() && name != "INFO_END")
		{
			name = pFile.readString();
			if (name[0] =='/' && name[1] == '/')	// skip comments
				pFile.findNewLine();
		}
		if (!pFile.isNotAtEOF())					// unexpected end of file
			return false;

		for (int lin = 0; lin < params_.nlines_; lin++)
		{
			for (int col = 0; col < params_.ncols_; col++)
			{
				valstring = pFile.readString();
				setElement(col,lin,atof(valstring.c_str()));
			}
		}
	}
	catch(...)
	{
		return false;
	}
	return true;
}

void
TeDecoderSPR::init()
{
	params_.status_= TeRasterParams::TeNotReady;
	int nb = params_.nBands();
	if (nb != 1 )
	{
		params_.errorMessage_ = "Ascii SPRING can store raster with only 1 band.";
		return;
	}

	// 1 - check if file exists and has the necessary permission
	if (params_.mode_ == 'c')	// creating a new file
	{
		ofstream dataFile(params_.fileName_.c_str()); // try to (re)create the file
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to (re)create the raster the file:" + params_.fileName_;
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
	}

	// 2 - check if there is memory enough to handle the data
	TeDecoderSmartMem::init();		
	if (params_.status_ == TeRasterParams::TeNotReady)
		return;

	// 3 - try to read the data
	if (params_.mode_ == 'c')	// creating a new file
	{
		try
		{
			params_.status_ = TeRasterParams::TeNotReady;
			TeAsciiFile sFile(params_.fileName_,"w+");
			if (!writeParameters(sFile) || !saveData(sFile))
			{
				params_.errorMessage_ = "Fail to write the raster to file";
				return;
			}
			params_.status_ = TeRasterParams::TeReadyToWrite;
		}
		catch (...)
		{
			params_.errorMessage_ = "Fail to write the raster to file";
		}
	}
	else if (params_.mode_ == 'w')
	{
		if (!readFile(params_.fileName_))
		{
			params_.status_ = TeRasterParams::TeNotReady;
			params_.errorMessage_ = "Fail to read the raster the file:" + params_.fileName_;
			return; 
		}
		params_.status_ = TeRasterParams::TeReadyToWrite;
	}
	else if (params_.mode_ == 'r')
	{
		if (!readFile(params_.fileName_))
		{
			params_.status_ = TeRasterParams::TeNotReady;
			params_.errorMessage_ = "Fail to read the raster the file:" + params_.fileName_;
			return; 
		}
		params_.status_ = TeRasterParams::TeReadyToRead;	
	}
}

bool 
TeDecoderSPR::writeParameters(TeAsciiFile& pFile)
{
	string name;
	try
	{
		TeBox box = params_.boundingBox();
		name = "GRIDREG\nINFO\n";
		pFile.writeString(name);
		name = "//Formato GRIDDEF  <ncols>  <nlins>  <X1>  <Y2>  <resX>  <resY>  <nodatavalue>\n";
		pFile.writeString(name);
		name = "GRIDDEF ";
		pFile.writeString(name);
		name = Te2String(params_.ncols_) + " " + Te2String(params_.nlines_) + " ";
		pFile.writeString(name);
		name = Te2String(box.x1_,6) + " " + Te2String(box.y2_,6) + " ";
		pFile.writeString(name);
		name = Te2String(params_.resx_,6) + " " + Te2String(params_.resy_,6) + " ";
		pFile.writeString(name);
		name = Te2String(params_.dummy_[0],6) + "\n";
		pFile.writeString(name);

		if (params_.photometric_[0] == TeRasterParams::TePallete)
		{
			name = "GRIDCLASS\n";
			for (unsigned int nc=0; nc<params_.lutr_.size(); ++nc)
			{
				name = Te2String(nc) + " " + Te2String(params_.lutr_[nc]);
				name += " " + Te2String(params_.lutg_[nc]) + " " + Te2String(params_.lutb_[nc]);
				name += " c" + Te2String(nc) + "\n";
				pFile.writeString(name);
			}
		}
		name = "INFO_END\n";
		pFile.writeString(name);
	}
	catch (...)
	{
		return false;
	}
	return true;
}

bool 
TeDecoderSPR::saveData(TeAsciiFile& pFile)
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
				TeDecoderSmartMem::getElement(c,l,d);
				fprintf(fp,fmt,d);
			}
			pFile.writeNewLine();
		}
		string spc = "END\n";
		pFile.writeString(spc);
	}
	catch(...)
	{
		return false;
	}
	return true;
}

bool 
TeDecoderSPR::setElement (int col,int lin, double val, int band)
{
	if (TeDecoderSmartMem::setElement(col,lin,val,band))
	{
		isModified_ = true;
		return true;
	}
	else
		return false;
}


TeDecoderSPRFactory::TeDecoderSPRFactory(const string& name): 
	TeDecoderFactory(name) 
{
	TeDecoderFactory::instanceName2Dec()["SPR"] = "SPR";
}
