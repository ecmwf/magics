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

#include "TeAsciiFile.h"
#include "TeErrorLog.h"
#include "TeException.h"
#include "TeAssertions.h"
#include "TeDefines.h"

#include <cstdlib> //FAMI

TeAsciiFile::TeAsciiFile ( const string& name, const char* mode ): 	
	TeStdFile ( name, mode  ) 
{
}  

TeAsciiFile::~TeAsciiFile() 
{ 	
}  

void
TeAsciiFile::findNewLine ()
{
	if ( feof ( file_ ) != 0 )
		return;
	char ch = '0';

	while ( ch != '\n' )
	{
		fscanf ( file_, "%c", &ch );
		if ( feof ( file_ ) != 0 )
			return;
	}
}

char 
TeAsciiFile::readQuotedChar()
{
	require ( feof ( file_ ) == 0 );

	char ch = ',', ch2;

	while ( ch != '"' )
	{
		fscanf ( file_, "%c", &ch );
	}
	fscanf ( file_, "%c", &ch );
	fscanf ( file_, "%c", &ch2 );

	ensure ( ch2 == '"' ); // just to make sure
return ch;
}

char 
TeAsciiFile::readChar()
{
	char ch=' ';
	require ( feof ( file_ ) == 0 );
	while ( ch == ' ' )
	{
		fscanf ( file_, "%c", &ch );
	}
	return ch;
}


string
TeAsciiFile::readString ()
{
	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	char input [TeNAME_LENGTH];
	input[0]=0;
	fscanf ( file_, "%s", input );

return input;
}

void
TeAsciiFile::writeString (const string& s)
{
	fprintf ( file_, "%s", s.c_str() );
}

string
TeAsciiFile::readLine ()
{
	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	char input [TeNAME_LENGTH];
	fgets ( input, TeNAME_LENGTH, file_ );

return input;
}

string 
TeAsciiFile::readQuotedString()
{
	require ( feof ( file_ ) == 0 );

	char ch = ' ';

	string quote;

	while ( ch != '"' )
	{
		fscanf ( file_, "%c", &ch );
	}

	while ( true )
	{
		fscanf ( file_, "%c", &ch );
		if ( ch == '\n' || ch == '\r') 
		{
			ungetc ( ch, file_ );
			break;
		}
		if ( ch == '"'  ) break;

		quote = quote + ch;
	}
return quote;
}

void
TeAsciiFile::readStringList ( vector<string>& strlist )
{
	require ( file_ != 0 );

	this->readStringListCSV ( strlist, ' ');
}

void
TeAsciiFile::readStringListCSV ( vector<string>& strlist, const char sep)
{

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	char ch, lastChr = 0;
	while (fscanf ( file_, "%c", &ch ) !=  EOF )
	{
		if ( ch == '\n' || ch == '\r') 
		{ 
			if(lastChr==sep)
				strlist.push_back ( "" );

			ungetc ( ch, file_ );
			return;
		}
		ungetc ( ch, file_ );
		// there are still more values to be read
		string name = readStringCSV (sep);
		if ( name.size() !=  0 || sep != ' ')
			strlist.push_back ( name );
		
		lastChr = ch;
	}
}

void
TeAsciiFile::readNStringCSV ( vector<string>& strlist, unsigned int n, const char sep)
{

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	strlist.clear();
	char ch, lastChr = 0;
	while (fscanf ( file_, "%c", &ch ) !=  EOF )
	{
		if ( ch == '\n' || ch == '\r') 
		{ 
			if(lastChr==sep)
				strlist.push_back ( "" );

			ungetc ( ch, file_ );
			return;
		}
		ungetc ( ch, file_ );
		// there are still more values to be read
		string name = readStringCSV (sep);
		if ( name.size() !=  0 || sep != ' ')
			strlist.push_back ( name );
		if (strlist.size() == n)
			break;
		
		lastChr = ch;
	}
}

string
TeAsciiFile::readStringCSV( const char del, bool skip, const char skip_char )
{
	require ( file_ != 0 );

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	char ch = '0';
	string line;
	bool inQuotes = false;

	int ret;
	while ( (ret=fscanf ( file_, "%c", &ch )) != EOF)
	{
		if ( ch == '\n' || ch == '\r') 
		{
			ungetc ( ch, file_ );
			break;
		}
		if ( ch == del  && !inQuotes ) break;
		if (skip)
			if (ch == skip_char) 
			{
				if (skip_char == '"')
					inQuotes = !inQuotes;
				continue;
			}
		line = line + ch;
	}
	return line;
}

string
TeAsciiFile::readStringCSVNoSpace( const char del )
{
	char blank = ' ';
	string line = readStringCSV ( del, true, blank );
return line;
}

string
TeAsciiFile::readStringCSVNoQuote( const char del )
{
	char quote = '"';
	string line = readStringCSV ( del, true, quote );
return line;
}

int
TeAsciiFile::readInt ()
{
	require ( file_ != 0 );

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );
	
	int value;
	fscanf ( file_, "%d", &value );

return value;
}

int
TeAsciiFile::readIntCSV ( const char del )
{
	char blank  = ' ';
	string line = readStringCSV ( del, true, blank );

return atoi(line.c_str());
}

double
TeAsciiFile::readFloatCSV ( const char del )
{
	char blank = ' ';
	string line = readStringCSV ( del, true, blank );

return atof(line.c_str());
}


double
TeAsciiFile::readFloat ()
{
	require ( file_ != 0 );

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );
	
	double value;
	fscanf ( file_, "%lf", &value );

return value;
}

TeCoord2D
TeAsciiFile::readCoord2D ()
{
	require ( file_ != 0 );

	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	double x, y;
	fscanf ( file_, "%lf %lf", &x, &y);

return TeCoord2D( x,  y );
}

TeBox
TeAsciiFile::readBox() 
{
	require ( file_ != 0 );

	if ( feof ( file_ ) != 0 )
	{
		throw TeException ( END_OF_FILE_REACHED, name(), true );
	}
	double x1, y1, x2, y2;


	fscanf ( file_, "%lf %lf %lf %lf", &x1, &y1, &x2, &y2 );

return TeBox ( x1, y1, x2, y2 );

}

string
TeAsciiFile::readAll()
{
	require ( file_ != 0 );
	if ( feof ( file_ ) != 0 )
		throw TeException ( END_OF_FILE_REACHED, name(), true );

	char ch = '0';
	string text="";
	int ret;
	while ((ret=fscanf ( file_, "%c", &ch )) != EOF)
	{
		if (ch != '\n') 
			text = text + ch;
	}
	return text;
}

