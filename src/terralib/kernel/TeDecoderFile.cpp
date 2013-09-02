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

#include <TeDecoderFile.h>

#ifdef WIN32

TeDecoderFile::TeDecoderFile ( const TeRasterParams& par )
{
	params_ = par;
	dataInitPos_ = 0;
	dataInitPos_ = par.offset_;
	m_dwPosition = 0;
	m_dwWordSize = params_.elementSize();
	params_.blockHeight_ = 1;
	params_.blockWidth_ = params_.ncols_;
	params_.decoderIdentifier_ = "RAW";
}

TeDecoderFile::~TeDecoderFile ()
{
	if (m_hFile != NULL)
		clear();
}

void
TeDecoderFile::init()
{
	clear();

	// First open the file
	m_hFile = CreateFileA(
		params_.fileName_.c_str(),	// File name
		GENERIC_READ | GENERIC_WRITE,	// Read-write
		FILE_SHARE_READ
		| FILE_SHARE_WRITE,		// Allow sharing-- we're only doing a quick scan
		NULL,					// No security attributes
		OPEN_EXISTING,			// Only open an existing file
		0,						// Ignore file attributes
		NULL);					// Ignore hTemplateFile

	if (m_hFile == INVALID_HANDLE_VALUE) 
			return ;		// could not open file

// Get the file's size
	m_dwSize = GetFileSize(m_hFile, NULL);
	if (m_dwSize == 0xffffffff)
	{
		m_hFile = NULL;
		return ;
	}

// Allocate buffer to get raster line from file

	long mBufferSize = params_.ncols_;
	if (params_.interleaving_ == TeRasterParams::TePerPixel)
		mBufferSize *= params_.nBands();

	switch (params_.dataType_[0]) {
	case (TeUNSIGNEDCHAR):
		m_buffer = new unsigned char[mBufferSize];
		break;
	case (TeCHAR) :
		m_buffer = new char[mBufferSize];
		break;
	case (TeUNSIGNEDSHORT):
		m_buffer = new unsigned short[mBufferSize];
		break;
	case (TeSHORT):
		m_buffer = new short[mBufferSize];
		break;
	case (TeUNSIGNEDLONG):
		m_buffer = new unsigned long[mBufferSize];
		break;
	case (TeLONG):
		m_buffer = new long[mBufferSize];
		break;
	case (TeFLOAT):
		m_buffer = new float[mBufferSize];
		break;
	case (TeDOUBLE):
		m_buffer = new double[mBufferSize];
		break;
	}
	if ( m_buffer == NULL )
		return ;
	else
		return ;
		
}

bool
TeDecoderFile::create()
{
	m_hFile = CreateFileA(
		params_.fileName_.c_str(),	// File name
		GENERIC_READ | GENERIC_WRITE,	// Read-write
		FILE_SHARE_READ
		| FILE_SHARE_WRITE,		// Allow sharing-- we're only doing a quick scan
		NULL,					// No security attributes
		CREATE_NEW,			    // Open a new file
		0,						// Ignore file attributes
		NULL);					// Ignore hTemplateFile
  
	if ( m_hFile == INVALID_HANDLE_VALUE) 
		return false;		// could not open file

	char			lpBuffer[1024];
	unsigned long	nNumberOfBytesToWrite=(long)params_.nBands() * (long)params_.ncols_ * (long)params_.nlines_;    // number of bytes to write
	nNumberOfBytesToWrite *= (long)params_.nbitsperPixel_[0]/8;    // number of bytes to write
	unsigned long	nNumberOfBytesWritten;  // pointer to number of bytes written

	for (;nNumberOfBytesToWrite > 1024; nNumberOfBytesToWrite-=1024)
	{
		if ( !WriteFile(
			m_hFile,                // handle to file to write to
			lpBuffer,               // pointer to data to write to file
			1024,					// number of bytes to write
			&nNumberOfBytesWritten,  // pointer to number of bytes written
			NULL					// pointer to structure for overlapped I/O
			))
 			return false;			// could not write to file
	}

	if ( !WriteFile(
		m_hFile,                // handle to file to write to
		lpBuffer,               // pointer to data to write to file
		nNumberOfBytesToWrite,	// number of bytes to write
		&nNumberOfBytesWritten,  // pointer to number of bytes written
		NULL					// pointer to structure for overlapped I/O
		))
 		return false;			// could not write to file
	
	SetFilePointer (m_hFile, NULL, NULL, FILE_BEGIN);

// Allocate buffer to get raster line from file

	switch (params_.dataType_[0]) {
	case (TeUNSIGNEDCHAR):
		m_buffer = new unsigned char[params_.ncols_];
		break;
	case (TeCHAR) :
		m_buffer = new char[params_.ncols_];
		break;
	case (TeUNSIGNEDSHORT):
		m_buffer = new unsigned short[params_.ncols_];
		break;
	case (TeSHORT):
		m_buffer = new short[params_.ncols_];
		break;
	case (TeUNSIGNEDLONG):
		m_buffer = new unsigned long[params_.ncols_];
		break;
	case (TeLONG):
		m_buffer = new long[params_.ncols_];
		break;
	case (TeFLOAT):
		m_buffer = new float[params_.ncols_];
		break;
	case (TeDOUBLE):
		m_buffer = new double[params_.ncols_];
		break;
	}

	if ( m_buffer == NULL )
		return false;

	if (params_.dummy_[0])
	{
		for (int b=0; b<params_.nBands();b++)
			for (int l=0; l<params_.nlines_;l++)
				for (int c=0; c<params_.ncols_;c++)
					setElement ( c, l, params_.dummy_[0], b);
	}
	return true;
}

bool
TeDecoderFile::clear()
{
	if (!m_hFile)
		return true;

	CloseHandle(m_hFile);
	m_hFile = NULL;
	delete m_buffer;
	m_buffer = 0;
	params_.status_ = TeRasterParams::TeNotReady;
	return true;
}


bool
TeDecoderFile::getRasterBlock(const TeBlockIndex& index, void *buf)
{   
	int ulCol, ulLin, band;
        blockIndexPos(index, ulCol, ulLin, band);

	int nb = params_.nBands();
	DWORD position;
	DWORD nBytesToRead;
	int inc, inic;
	if (params_.interleaving_ == TeRasterParams::TePerPixel)
	{
		inc = nb;
		inic = index.band();
		position = dataInitPos_ + params_.ncols_*(nb* ulLin);
		nBytesToRead = m_dwWordSize*params_.ncols_*nb;
	}
	else
	{
		inc = 1;
		inic = 0;
		position = dataInitPos_ + params_.ncols_*(nb*ulLin + band);
		nBytesToRead = m_dwWordSize*params_.ncols_;
	}
	DWORD diff = position - m_dwPosition;
	if (diff != 0)
		m_dwPosition = SetFilePointer (m_hFile, diff, NULL, FILE_CURRENT);
	unsigned long nBytesRead;
	if (!ReadFile(m_hFile, m_buffer, nBytesToRead, &nBytesRead, NULL))
		return false;
	m_dwPosition += nBytesRead;

	int i,n;
	switch (params_.dataType_[0]) 
	{
	case (TeUNSIGNEDCHAR):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((unsigned char*)buf)[n] = ((unsigned char*)m_buffer)[i];
		break;
	case (TeCHAR) :
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((char*)buf)[n] = ((char*)m_buffer)[i];
		break;
	case (TeUNSIGNEDSHORT):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((unsigned short*)buf)[n] = ((unsigned short*)m_buffer)[i];
		break;
	case (TeSHORT):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((short*)buf)[n] = ((short*)m_buffer)[i];
		break;
	case (TeUNSIGNEDLONG):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((unsigned long*)buf)[n] = ((unsigned long*)m_buffer)[i];
		break;
	case (TeLONG):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((long*)buf)[n] = ((long*)m_buffer)[i];
		break;
	case (TeFLOAT):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((float*)buf)[n] = ((float*)m_buffer)[i];
		break;
	case (TeDOUBLE):
		for (i=inic,n=0;i<params_.ncols_*nb;i+=inc, n++)
			((double*)buf)[n] = ((double*)m_buffer)[i];
		break;
	}

	return true;
}



bool 
TeDecoderFile::putRasterBlock(const TeBlockIndex& index, void *buf, long bsize)
{
	int band,ulCol,ulLin;
  blockIndexPos(index, ulCol, ulLin, band);

	DWORD position = dataInitPos_ + params_.ncols_*(params_.nBands()*ulLin + band);
	DWORD diff = position - m_dwPosition;
	if (diff != 0)
		SetFilePointer (m_hFile, diff, NULL, FILE_CURRENT);
	DWORD nBytesToWrite = m_dwWordSize*params_.ncols_,
		nBytesWritten;
	int i;
	switch (params_.dataType_[0]) 
	{
	case (TeUNSIGNEDCHAR):
		for (i=0;i<params_.ncols_;i++)
			((unsigned char*)m_buffer)[i] = ((unsigned char*)buf)[i];
		break;
	case (TeCHAR) :
		for (i=0;i<params_.ncols_;i++)
			((char*)m_buffer)[i] = ((char*)buf)[i];
		break;
	case (TeUNSIGNEDSHORT):
		for (i=0;i<params_.ncols_;i++)
			((unsigned short*)m_buffer)[i] = ((unsigned short*)buf)[i];
		break;
	case (TeSHORT):
		for (i=0;i<params_.ncols_;i++)
			((short*)m_buffer)[i] = ((short*)buf)[i];
		break;
	case (TeUNSIGNEDLONG):
		for (i=0;i<params_.ncols_;i++)
			((unsigned long*)m_buffer)[i] = ((unsigned long*)buf)[i];
		break;
	case (TeLONG):
		for (i=0;i<params_.ncols_;i++)
			((long*)m_buffer)[i] = ((long*)buf)[i];
		break;
	case (TeFLOAT):
		for (i=0;i<params_.ncols_;i++)
			((float*)m_buffer)[i] = ((float*)buf)[i];
		break;
	case (TeDOUBLE):
		memcpy (m_buffer,buf,params_.ncols_);
		break;
	}

	if (!WriteFile(m_hFile, m_buffer, nBytesToWrite, &nBytesWritten, NULL))
		return false;

	m_dwPosition += nBytesWritten;

	return true;
}

TeBlockIndex TeDecoderFile::blockIndex(int col, int lin, int band)
{
  TeBlockIndex bl_index;

  bl_index.band_ = band;
  bl_index.lin_ = lin;
  bl_index.col_ = 0;

  return bl_index;
}

void TeDecoderFile::blockIndexPos(const TeBlockIndex& index, int& ulCol, int& ulLin, int& band)
{
  ulCol = 0;
  ulLin = index.line() * params_.blockWidth_;
  band = index.band();
}

#endif
