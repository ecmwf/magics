/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeErrorLog.h"
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
	
}


void 
TeErrorLog::startSession ( const string& errLogFileName)
{
  endSession();
  
}

int
TeErrorLog::endSession ()
{
 

  return 0;
}
 
 
