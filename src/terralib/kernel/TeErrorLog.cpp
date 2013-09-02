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

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeErrorLog.h"
#include "TeAsciiFile.h"
#include "TeUtils.h"
#include "TeException.h"

TeErrorLog::TeErrorLog():logFile_ ( 0 )
{
	errorMessage_ [ LOG_MESSAGE             ] = "Message ";

	errorMessage_ [ EMPTY_FILE_NAME         ] = "Empty File Name";
	errorMessage_ [ CANNOT_OPEN_FILE        ] = "Could Not Open File";
    errorMessage_ [ CANNOT_READ_FILE		] = "Could Not Read File";
    errorMessage_ [ CANNOT_WRITE_FILE   	] = "Could Not Write File";
    errorMessage_ [ CANNOT_CLOSE_FILE		] = "Could Not Close File";
    errorMessage_ [ END_OF_FILE_REACHED 	] = "End of File Reached";


// Errors associated with projection

    errorMessage_ [  DATUM_ERROR              ] = "Error when creating Datum";
    errorMessage_ [  PROJECTION_NOT_AVAILABLE ] = "Projection not available in TerraLib";

// Error codes for MIF files
    errorMessage_ [   NOT_MIF_FILE			] = "Not a MIF File";
    errorMessage_ [   MIF_REGION_CLOSE		] = "Error - MIF Region is not Closed";
    errorMessage_ [   MIF_CONVERSION_ERROR	] = "Error during conversion of MIF data";
    errorMessage_ [   NOT_CSV               ] = "Data not separated by commas";
    errorMessage_ [   NOT_ENOUGH_ROWS       ] = "Less rows in MID file than regions in MIF file";
    errorMessage_ [   NOT_POLYGON_FILE		] = "Not a polygon (planar areas) file";
    errorMessage_ [   PLINE_ERROR			] = "Wrongly defined PLINE attribute"; 
    errorMessage_ [   NO_DTM_QUOTE_VALUE    ] = "Unable to define an attribute as a DTM quote file";

// Error Codes for shapefiles
    errorMessage_ [  POLYSHAPE_IS_NOT_RING  ] = "Shape PolyShape is not a ring";
    errorMessage_ [  UNHANDLED_SHP_TYPE     ] = "Shape type unhandled currently";

// Error Codes for DBF files

    errorMessage_ [  UNABLE_TO_OPEN_DBF_FILE  ] = "Unable to Open DBF file";
    errorMessage_ [  NO_DATA_IN_DBF_FILE      ] = "No Data in DBF File";
    errorMessage_ [  ATTRIBUTE_INDEX_MISMATCH ] = "Mismatch between attribute index and number of geometries";

// Error codes for E00 Files
    errorMessage_ [  NOT_E00_FILE            ] = "Not an E00 File";

// Error codes for algorithms
    errorMessage_ [  CENTROID_NOT_FOUND   ] = "Could not find centroid of a polygon";
    errorMessage_ [  REGIONS_WITH_HOLES   ] = "Problems in regions with holes";
    errorMessage_ [  LINE_IS_NOT_RING	  ] = "Tried to a create a ring from a non-closed line";

// Error codes for tables

    errorMessage_ [  ROW_MISMATCH		  ] = "Unable to insert a row into a table (attribute mismatch)";
    errorMessage_ [  NO_SUCH_ATTRIBUTE    ] = "Attribute does not exist in the table";

	// Error codes for Raster stuff
    errorMessage_ [  UNKNOWN_DECODER_TECHNOLOGY_TYPE ] = "Decoder technology not specified";
    errorMessage_ [  UNKNOWN_RASTER_FORMAT	         ] = "Raster format not supported";

	// Error codes for layer stuff
	errorMessage_ [  GEOMETRY_REPRESENTATION_MISMATCH ] = "Geometry representation has not been created";
  
  // TeFactory related errors
  errorMessage_ [  FACTORY_PRODUCT_INSTATIATION_ERROR ] = 
    "Factory product instatiation error";

	errorMessage_ [  UNKNOWN_ERROR_TYPE ] = "";
}

TeErrorLog::~TeErrorLog()
{
  endSession();
}
string
TeErrorLog::message( const TeErrorType msgCode )
{
	// find the message associated to the errorcode
	map <TeErrorType, string>::iterator it = errorMessage_.find ( msgCode );
	if ( it != errorMessage_.end()  )
		return (*it).second;
	else
		return errorMessage_ [ UNKNOWN_ERROR_TYPE ];
}

void
TeErrorLog::insert ( TeErrorType code, const string& msg )
{
	// create a new logfile, if needed
	if ( logFile_  == 0 )
		//logFile_ = new TeAsciiFile ( "terralib.err", "rw" );
		return;

	string log = errorMessage_ [ code ] + " " + msg;
	logFile_->writeString ( log );
  logFile_->writeNewLine();

	// increment the count associated to this error type
	map<TeErrorType, int>::iterator it = errorLog_.find( code );
	if ( it == errorLog_.end() )
		errorLog_[ code ] = 1;
	else
	{ 
		int count = (*it).second;
		errorLog_[ code ] = ++count;
	}
}


void 
TeErrorLog::startSession ( const string& errLogFileName)
{
  endSession();
  
	// create a new logfile
	logFile_ = new TeAsciiFile ( errLogFileName, "w+" );
  
  if( logFile_ == 0 ) {
    throw TeException( UNKNOWN_ERROR_TYPE, "Unable to create log file", 
      false );
  }
}

int
TeErrorLog::endSession ()
{
  if( logFile_ == 0 ) {
    errorLog_.clear ();
    return 0;
  }

	int numerr =  errorLog_.size();

	// increment the count associated to this error type
	map<TeErrorType, int>::iterator it = errorLog_.begin();

	while ( it != errorLog_.end() )
	{
		int count = (*it).second;
		string log = "Number of Error of Type " + errorMessage_ [ (*it).first ] 
			+ " = " + Te2String ( count );
		logFile_->writeString ( log );
    logFile_->writeNewLine();
    
    ++it;
	}

	// close the log file
	delete logFile_;
  logFile_ = 0;

	// clear the stack
	errorLog_.clear ();

  return numerr;
}
 
 
