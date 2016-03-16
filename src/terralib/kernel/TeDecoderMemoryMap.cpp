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

#include "TeDecoderMemoryMap.h"
#include "TeUtils.h"
#include "TeAsciiFile.h"
#include <map>
#include <string>

TeDecoderMemoryMap::TeDecoderMemoryMap ( const TeRasterParams& par )
{
	params_ = par;
	dataInitPos_ = 0;
	dataInitPos_ = par.offset_;
	m_lpszFile = 0;
	m_hFile = 0;
	params_.decoderIdentifier_ = "MEMMAP";
	params_.errorMessage_.clear();
}

TeDecoderMemoryMap::~TeDecoderMemoryMap ()
{
	if (m_hFile != 0)
		clear();
}
bool
TeDecoderMemoryMap::getElement (int col,int lin, double& val,int band)
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
	
	position += dataInitPos_;
    switch (params_.dataType_[0])
	{
	case (TeUNSIGNEDCHAR):
		val = ((unsigned char*)m_lpszFile)[position];
		break;
	case (TeCHAR) :
		val = ((char*) m_lpszFile)[position];
		break;
	case (TeUNSIGNEDSHORT):
		unsigned short uval;
		if (params_.swap_)
			uval = swaps(((unsigned short*)m_lpszFile)[position]);
		else
			uval = ((unsigned short*)m_lpszFile)[position];
		val = uval;
		break;
	case (TeSHORT):
		if (params_.swap_)
			val = swaps(((short*)m_lpszFile)[position]);
		else
			val = ((short*)m_lpszFile)[position];
		break;
	case (TeINTEGER):
		val = ((int*)m_lpszFile)[position];
		break;
	case (TeUNSIGNEDLONG):
		val = ((unsigned long*)m_lpszFile)[position];
		break;
	case (TeLONG):
		val = ((long*)m_lpszFile)[position];
		break;
	case (TeFLOAT):
		val = ((float*)m_lpszFile)[position];
		break;
	case (TeDOUBLE):
		val = ((double*)m_lpszFile)[position];
		break;
	default:
		return false;
	}
	return true;
}

bool
TeDecoderMemoryMap::setElement (int col,int lin, double val, int band)
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
	
	position += dataInitPos_;
	switch (params_.dataType_[0])
	{
	case (TeUNSIGNEDCHAR):
		((unsigned char*)m_lpszFile)[position] = (unsigned char) val;
		break;
	
	case (TeCHAR) :
		((char*) m_lpszFile)[position] = (char) val;
		break;

	case (TeUNSIGNEDSHORT):
		if (params_.swap_)
			((unsigned short*)m_lpszFile)[position] = swaps((unsigned short) val);
		else
			((unsigned short*)m_lpszFile)[position] = (unsigned short) val;
		break;

	case (TeSHORT):
		if (params_.swap_)
			((short*)m_lpszFile)[position] = swaps((short) val);
		else
			((short*)m_lpszFile)[position] = (short) val;
		break;

	case (TeINTEGER):
		((int*)m_lpszFile)[position] = (int) val;
		break;

	case (TeUNSIGNEDLONG):
		((unsigned long*)m_lpszFile)[position] = (unsigned long) val;
		break;
	
	case (TeLONG):
		((long*)m_lpszFile)[position] = (long) val;
		break;

	case (TeFLOAT):
		((float*)m_lpszFile)[position] = (float) val;
		break;
	
	case (TeDOUBLE):
		((double*)m_lpszFile)[position] = val;
    break;
    
	default:
		return false;
	}
	return true;
}

#ifdef WIN32
void
TeDecoderMemoryMap::init()
{
	clear();
	DWORD dwDesiredAccess, dwCreationDisposition, flProtect,dwDesiredAccessV ;
	if (params_.mode_ == 'c')
	{
		params_.writeParametersFile();
		dwCreationDisposition = CREATE_ALWAYS;
		dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;
		flProtect = PAGE_READWRITE;
		dwDesiredAccessV = FILE_MAP_WRITE;
	}
	else if (params_.mode_ == 'w')
	{
		params_.readParametersFile();
		dwCreationDisposition = OPEN_ALWAYS;
		dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;		
		flProtect = PAGE_READWRITE;
		dwDesiredAccessV = FILE_MAP_WRITE;
	}
	else 	// default mode: reading
	{
		params_.readParametersFile();
		dwCreationDisposition = OPEN_EXISTING;
		dwDesiredAccess = GENERIC_READ;		
		flProtect = PAGE_READONLY;
		dwDesiredAccessV = FILE_MAP_READ;
	}

	
	// calculate the expected file size accordingly to the parameters
	unsigned long fsize = (long)(params_.nBands()*params_.ncols_*params_.nlines_);    
	fsize *= params_.elementSize(0);
	
	if (fsize <= 0)
		return;
	
	// First open the file
	m_hFile = CreateFileA(
		params_.fileName_.c_str(),	// File name
		dwDesiredAccess,			// Desired access
		FILE_SHARE_READ| FILE_SHARE_WRITE,	// Allow sharing-- we're only doing a quick scan
		NULL,								// No security attributes
		dwCreationDisposition,				// Creation disposition
		0,									// Ignore file attributes
		NULL);								// Ignore hTemplateFile

	if (m_hFile == INVALID_HANDLE_VALUE)
		return;		// could not open file
			
	// get the file's size
	m_dwSize = GetFileSize(m_hFile, NULL);
	if (m_dwSize == 0xffffffff)
	{
		m_hFile = NULL;
		return;
	}
	
	if (params_.mode_ == 'c')	
	{
		if (m_dwSize < fsize)
		{
			char lpBuffer[1024];
			int nNumberOfBytesToWrite = fsize-m_dwSize;
			unsigned long nNumberOfBytesWritten;  // pointer to number of bytes written
			for (;nNumberOfBytesToWrite > 1024; nNumberOfBytesToWrite-=1024)
			{
				if (!WriteFile(m_hFile, lpBuffer, 1024, &nNumberOfBytesWritten, NULL))
				{
					CloseHandle(m_hFile);
					m_hFile = NULL;
					return;
				}
			}
			if (!WriteFile(m_hFile, lpBuffer, nNumberOfBytesToWrite, &nNumberOfBytesWritten, NULL))
			{
				CloseHandle(m_hFile);
				m_hFile = NULL;
				return;
			}	
		}
	}
	else
	{
		if (m_dwSize < fsize)
			return;
	}

	// Create a mapping object from the file
	m_hMapping = CreateFileMapping(
		m_hFile,		// Handle we got from CreateFile
		NULL,			// No security attributes
		flProtect,		// read-write
		0, 0,			// Max size = current size of file
		NULL);			// Don't name the mapping object
		
	if ( m_hMapping == NULL )
	{
		CloseHandle(m_hFile);
		m_hFile = NULL;
		return;
	}

	// Map the file to memory
	switch (params_.dataType_[0]) {
	case (TeUNSIGNEDCHAR):
		m_lpszFile = (unsigned char*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0,	0);
		break;
	case (TeCHAR) :
		m_lpszFile = (char*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0, 0);
		break;
	case (TeUNSIGNEDSHORT):
		m_lpszFile = (unsigned short*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0, 0);
		break;
	case (TeSHORT):
		m_lpszFile = (short*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0,	0);
		break;
	case (TeUNSIGNEDLONG):
		m_lpszFile = (unsigned long*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0,	0);
		break;
	case (TeLONG):
		m_lpszFile = (long*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0, 0);
		break;
	case (TeFLOAT):
		m_lpszFile = (float*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0,	0);
		break;
	case (TeDOUBLE):
		m_lpszFile = (double*) MapViewOfFile(m_hMapping, dwDesiredAccessV, 0, 0, 0);
		break;
	}
	if ( m_lpszFile == NULL )
		return ;

	if (params_.mode_ == 'c')
	{
		int b, l, c;
		for (b=0; b<params_.nBands();++b)
			for (l=0; l<params_.nlines_;++l)
				for (c=0; c<params_.ncols_;++c)
					setElement(c, l, params_.dummy_[b], b);
	}
	if (params_.mode_ =='c' || params_.mode_ == 'w')
		params_.status_ = TeRasterParams::TeReadyToWrite;
	else 
		params_.status_ = TeRasterParams::TeReadyToRead;	
	return;
}

bool
TeDecoderMemoryMap::clear()
{
	params_.status_ = TeRasterParams::TeNotReady;
	params_.errorMessage_.clear();
	if (m_hFile == NULL)
		return true;

	if ( !UnmapViewOfFile(m_lpszFile) )
		return false;

	CloseHandle(m_hMapping);
	CloseHandle(m_hFile);
	m_hFile = NULL;
	return true;
}
#else
// Linux version: uses mmap
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

void
TeDecoderMemoryMap::init()
{
	clear();
	mode_t	mode=0;
	if (params_.mode_ == 'c') 
	{
		m_dwSize = (long)(params_.nBands()*params_.ncols_*params_.nlines_);    
		m_dwSize *= params_.elementSize(0);
		if (m_dwSize <= 0)
			return;
		params_.writeParametersFile();
		m_hFile = open(params_.fileName_.c_str(),O_RDWR|O_CREAT,S_IRWXU);
		mode =  (PROT_READ | PROT_WRITE);
      		lseek(m_hFile, m_dwSize - 1, SEEK_SET);
      		//write(m_hFile, '\0', 1);	
	}
	else if (params_.mode_ == 'r') 
	{	
		params_.readParametersFile();
		m_dwSize = (long)(params_.nBands()*params_.ncols_*params_.nlines_);    
		m_dwSize *= params_.elementSize(0);
		if (m_dwSize <= 0)
			return;
		m_hFile = open(params_.fileName_.c_str(),O_RDONLY,S_IRWXU);	
		mode = PROT_READ;
	}
	else
	{
		params_.readParametersFile();
		m_dwSize = (long)(params_.nBands()*params_.ncols_*params_.nlines_);    
		m_dwSize *= params_.elementSize(0);
		if (m_dwSize <= 0)
			return;	
		m_hFile = open(params_.fileName_.c_str(),O_RDWR,S_IRWXU);
		mode = (PROT_READ | PROT_WRITE);
	}
	
	if (m_hFile == -1)
		return;				// could not open file

	m_lpszFile  =  mmap(0, m_dwSize, mode, MAP_SHARED, m_hFile, 0);
	if (((long) m_lpszFile)  == -1 )
		return;

	if (params_.mode_ == 'c')	
	{
		int b, l, c;
		for (b=0; b<params_.nBands();++b)
			for (l=0; l<params_.nlines_;++l)
				for (c=0; c<params_.ncols_;++c)
					setElement(c, l, params_.dummy_[b], b);
	}
	
	if (params_.mode_ =='c' || params_.mode_ == 'w')
		params_.status_ = TeRasterParams::TeReadyToWrite;
	else 
		params_.status_ = TeRasterParams::TeReadyToRead;	
	return;
}

bool
TeDecoderMemoryMap::clear()
{
	params_.status_ = TeRasterParams::TeNotReady;
	params_.errorMessage_.clear();
	if (m_lpszFile == 0)
		return true;

	if ( munmap(m_lpszFile, m_dwSize) == -1)
		return false;
	m_lpszFile = 0;
	close(m_hFile);
	m_hFile = 0;
	return true;
}
#endif

TeDecoderMemoryMapFactory::TeDecoderMemoryMapFactory(const string& name) : 
	TeDecoderFactory(name) 
{
	TeDecoderFactory::instanceName2Dec()["RAW"] = "MEMMAP";
}
