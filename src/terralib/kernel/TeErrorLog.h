/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeErrorLog.h
    \brief This file deals log of errors in TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_ERRORLOG_H
#define  __TERRALIB_INTERNAL_ERRORLOG_H

#include "TeSingleton.h"

#include <string>
#include <stack>
#include <map>
using namespace std;

class TeAsciiFile;

enum TeErrorType
{

	// Generic message inserted to log file
	LOG_MESSAGE,

	// Errors associated to files
	EMPTY_FILE_NAME,
	CANNOT_OPEN_FILE,
    CANNOT_READ_FILE,
    CANNOT_WRITE_FILE,
    CANNOT_CLOSE_FILE,
    END_OF_FILE_REACHED,

	// Errors associated with projection
    DATUM_ERROR,
    PROJECTION_NOT_AVAILABLE,

// Error codes for MIF files
    NOT_MIF_FILE,
    MIF_REGION_CLOSE,
    MIF_CONVERSION_ERROR,
    NOT_CSV,
    NOT_ENOUGH_ROWS,
    NOT_POLYGON_FILE,
    PLINE_ERROR,
    NO_DTM_QUOTE_VALUE,

// Error Codes for shapefiles
    POLYSHAPE_IS_NOT_RING,
    UNHANDLED_SHP_TYPE,

// Error Codes for DBF files

    UNABLE_TO_OPEN_DBF_FILE,
    NO_DATA_IN_DBF_FILE,
    ATTRIBUTE_INDEX_MISMATCH,

	// Error codes for E00 Files
    NOT_E00_FILE,

	// Error codes for algorithms
    CENTROID_NOT_FOUND,
    REGIONS_WITH_HOLES,
    LINE_IS_NOT_RING,

	// Error codes for tables

    ROW_MISMATCH,
    NO_SUCH_ATTRIBUTE,

	// Error codes for Raster stuff
    UNKNOWN_DECODER_TECHNOLOGY_TYPE,
    UNKNOWN_RASTER_FORMAT,

	// Layer stuff
	GEOMETRY_REPRESENTATION_MISMATCH,
  
  // TeFactory related errors
  FACTORY_PRODUCT_INSTATIATION_ERROR,

	// Other
	UNKNOWN_ERROR_TYPE
};

typedef map<TeErrorType, string> TeErrorTypeMap;

typedef map<TeErrorType, int> TeErrorLogMap;

//! Describes a class for describing error logging in TerraLib
/*!
	All errors in TerraLib may be  logged into an error file. 
	The error logging facilities include the notion of a 
	session, using the "start" and "end" methods, of a 
	dump method for acessing the contenst

\sa TeException, TeSingleton
*/
class TL_DLL TeErrorLog: public TeSingleton<TeErrorLog>
{
public:
	TeErrorLog();

	~TeErrorLog();
	
	string message(const TeErrorType code);
	
	virtual void insert ( TeErrorType code, const string& msg = "" );

	virtual void startSession ( const string& errLogFileName = "terralib.err");

	virtual int endSession ();

private:
		
    TeAsciiFile* logFile_;

	TeErrorLogMap errorLog_;

	TeErrorTypeMap  errorMessage_;
};

#endif


