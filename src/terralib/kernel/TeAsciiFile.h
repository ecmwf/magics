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
/*! \file TeAsciiFile.h
    \brief This file contains structures and definitions to deal ASCII files
*/
#ifndef  __TERRALIB_INTERNAL_ASCIIFILE_H
#define  __TERRALIB_INTERNAL_ASCIIFILE_H

#include "TeDefines.h"
#include "TeStdFile.h"
#include "TeCoord2D.h"
#include "TeBox.h"

#include <stdio.h>

#include <vector> 
#include <string>
using namespace std;

//!  A class for handling reading/writing for ASCII files
/*!  Simple wrapper around a stdio file. Clientes are all the 
	 functions that imports from ASCII files. 

 \sa
  TeStdFile, TeSPRFile
*/
class TL_DLL TeAsciiFile: public TeStdFile {

public:

	//! Contructors
	TeAsciiFile(const string& name, const char* mode = "r");
	
	//! Destructor
	virtual ~TeAsciiFile();

	//! Writes a carriage return character
	void writeNewLine ()
	{ fprintf ( file_, "\n" ); } 
	
	//! Goes to a new line 
	void findNewLine () ; 

	//! Reads a string
	string readString(); 

	//! Reads a full line up to carriage return
	string readLine(); 

	//! Writes a generic string to the file
	void writeString (const string& s);

	//! Reads a string whithin quotes
	string readQuotedString(); 

	//! Reads a comma-separated string ( with a skip character )
	string readStringCSV( const char sep = ',', bool skip = false, 
						  const char skip_char = ' ');

	//! Reads a character whithin quotes
	char readQuotedChar();

	//! Reads a character
	char readChar();

	//! Reads a comma-separated string(ignore spaces)
	string readStringCSVNoSpace( const char del = ',');

	//! Reads a comma-separated string (ignore quote )
	string readStringCSVNoQuote ( const char del = ',' );

	//! Reads an integer
	int readInt();

	//! Reads a comma-separated int
	int readIntCSV( const char del = ',');

	//! Reads a float
	double readFloat();

	//! Reads a comma-separated float
	double readFloatCSV( const char del = ',');

	//! Reads a 2D coordinate ( x, y )
	TeCoord2D readCoord2D();

	//! Reads a line and put in a stringlist
	void readStringList ( vector<string>& );

	//! Reads a comma-separated line string and put in a string list
	void readStringListCSV ( vector<string>&, const char sep = ',');

	//! Reads the first n strings from a comma separated line string and put in a string list
	void readNStringCSV ( vector<string>&, unsigned int n, const char sep = ',');

	//! Reads a bounding box
	TeBox readBox();

	//! Reads the entire content of the file, skipping new line characters only
	string readAll();

private:
	
// No copy allowed

	TeAsciiFile(const TeAsciiFile&);
	TeAsciiFile& operator=(const TeAsciiFile&);

};
#endif


