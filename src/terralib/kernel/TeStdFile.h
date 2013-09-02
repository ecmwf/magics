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
/*! \file TeStdFile.h
    \brief This file contains a class for handling "stdio" files
*/
#ifndef  __TERRALIB_INTERNAL_STDFILE_H
#define  __TERRALIB_INTERNAL_STSFILE_H

#include "TeDefines.h"
#include <stdio.h>
#include <string>
using namespace std;

//! A class for handling "stdio" files
/*  Provide support for using a file as a class, and 
    throws exceptions if things go wrong
	\sa TeException TeAsciiFile
*/
class TL_DLL TeStdFile {
public:

//! Simple constructor taking the file name and the mode of oppening
		TeStdFile(const string& name, const char* mode = "r");
	
	virtual ~TeStdFile(); 

//! Return true if the file pointer is not at the end of file
	bool isNotAtEOF()
	{ 
		if ( feof ( file_ ) != 0  )
			return false;
		return true;
	}

// name
	string name ()
	{ return fileName_; }

// -- operator ()	
	operator FILE*() { return file_; }

// -- returns the file pointer
	FILE* FilePtr() { return file_; }

	void rewind();

protected:
	
// -- File pointer

	FILE *file_;
	string fileName_;
	string mode_;

private:
	
// No copy allowed
	TeStdFile(const TeStdFile&);
	TeStdFile& operator=(const TeStdFile&);

};

#endif

