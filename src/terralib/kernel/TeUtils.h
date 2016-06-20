/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
/*! \file TeUtils.h
     \brief This file contains some general purpose utilitary  functions
*/
#ifndef  __TERRALIB_INTERNAL_UTILS_H
#define  __TERRALIB_INTERNAL_UTILS_H

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include <string>
#include <vector>
#include <cstdio>
#include <time.h>


#include "TeCoord2D.h"

using namespace std;

/** @defgroup Utils General purpose utilitary functions
 *  General purpose utilitary functions
 *  @{
 */

/** @defgroup FileNameFunctions Functions to manipulate file names 
	@ingroup  Utils
 *  @{
 */
	//! Get the extension part (right to the dot) of a string
	TL_DLL string TeGetExtension ( const char* value );

	//! Get the name part (left to the dot) of a string, path included
	TL_DLL string TeGetName ( const char* value );

	//! Get the base name part of a string, strip path information
	TL_DLL string TeGetBaseName ( const char* value );

	//! Get the path part of a file name
	TL_DLL string TeGetPath(const char* value);

	//! Writes a string to a file.
	/*!
		\param fileName the file name to write to, including the directory.
		\param text     the string with the text to write to.
		\param mode     the mode used to write to the file (a | w | r).
	*/
	TL_DLL void TeWriteToFile(const string& fileName, const string& text, const string& mode);
/** @} */ 

/** @defgroup C2StringFunctions Functions convert numbers to strings
	@ingroup  Utils
 *  @{
 */
	//! Transforms an integer to string
	TL_DLL string Te2String ( const int value );

	//! Transforms an unsigned integer to string
	TL_DLL string Te2String ( const unsigned int value );

	//! Transforms a long to string
	TL_DLL string Te2String ( const long value );

	//! Transforms an unsigned long to string
	TL_DLL string Te2String ( const unsigned long value );

	//! Transforms a double to string floating point notation with precision decimal places
	TL_DLL string Te2String ( const double value, int precision );

	//! Transforms a double to string in exponential notation
	TL_DLL string Te2String ( const double value );

/** @} */ 

/** @defgroup StringFunctions Functions to manipulate strings
 	@ingroup  Utils
*  @{
 */
	//! Converts a string to upper case
	TL_DLL void TeConvertToUpperCase ( const string& , char* );

	//! Converts a string to upper case
	TL_DLL string TeConvertToUpperCase (const string &name);

	//! Converts a string to lower case
	TL_DLL string TeConvertToLowerCase (const string &name);

	//! Removes special characteres from a string
	TL_DLL string TeRemoveSpecialChars ( string& str);

	//! Removes left and right blank, tab and carriage return characters of a string
	TL_DLL void TeTrim(string &str);

	//! Splits a string, given a separator, in a vector of parts
	TL_DLL int TeSplitString(const string& input, const string& delimiter, vector<string>& results);

	//! Compares two strings
	/*
		\params caseS flag indicating if it is a case sensitive comparation
	*/
	TL_DLL bool TeStringCompare(const string& str1, const string& str2, bool caseS=false);


	//! Validate a string to check if it can be used as a column name
	/*
		\param name			string to be checked
		\param changed		output flag to identify that string has changed
		\param invalidChar	output or sequence of chars that are invalid in the name
		\return the modified valid name
	*/
	TL_DLL string TeCheckName(const string& name, bool& changed, string& invalidChar);
/** @} */ 

/** @defgroup MathFunctions Mathematical functions
 	@ingroup  Utils
*  @{
 */
	//! Rounds a double to int
	TL_DLL inline int TeRound(double val)
	{	
		if (val>=0)
			return (int)(val+.5);
		else
			return (int)(val-.5);
	}

	//! Rounds a double value to a given number of decimal digits
	TL_DLL double TeRoundD(double val, int precision=8);

	//! Compares two doubles
	TL_DLL bool TeCompareDouble(double a, double b, int precision);

	//! Adjust a number to a given precision 
	/*
		\param val the number to be adjusted
		\param precision the number of decimals places to be used
		\param reduce flag to indicate that the number should be reduced instead to increased
		\return the adjusted number
	*/
	TL_DLL double TeAdjustToPrecision(double val, int precision, bool reduce=false);

	//! Rounds a double raster element index to an integer
	/*
		Raster elements have area, so raster element in upper-left position has
		index from [-0.5,+0.5) in i and j dimension.
	*/
	TL_DLL inline int TeRoundRasterIndex(double val)
	{	
		int ind = (int) val;
		if (val < (ind-0.5))
			ind -= 1;
		else if (val >= (ind+0.5))
			ind += 1;
		return ind;
	}
	/**
	* Cubic root from x.
	*
	* @param x X.
	* @return The cubic root from x.
	*/         
	TL_DLL inline double TeCubicRoot( double x )
	{
		if( x < 0 ) {
		return ( -1. ) * pow(  ( -1. ) * x, ( 1. / 3. ) );
		} else {
			return pow(  x, ( 1. / 3. ) );
		}
	};
	
	/*! Comparassion of two floating points, considering a given precision */
	TL_DLL inline bool TeFPEquals(double d1, double d2, double precision)
	{
		double eps1 = fabs(d1), 
			eps2 = fabs(d2), 
			eps;
		eps = (eps1 > eps2) ? eps1 : eps2;
		if (eps == 0.0)
			return true; //both numbers are 0.0
		eps *= precision;
		return (fabs(d1 - d2) < eps);
	}

	//! Swap the bytes of a short value
	static inline short swaps(short value)
	{
		short svalue = ((value & 0x00ff) << 8) | ((value >> 8) & 0x00ff);
		return svalue;
	}
  
  /**
   * Returns the amount of free physical memory (bytes).
   *
   * @return The amount of free physical memory (bytes).
   */
  TL_DLL unsigned long int TeGetFreePhysicalMemory();

  /**
   * Returns the amount of total physical memory (bytes).
   *
   * @return The amount of total physical memory (bytes).
   */
  TL_DLL unsigned long int TeGetTotalPhysicalMemory();
  
  /**
   * Returns the number of physical processors.
   *
   * @return The number of physical processors.
   */
  TL_DLL unsigned int TeGetPhysProcNumber();  
    
  /**
   * Generates a temporary unique file name.
   *
   * @param filename The generated file name.
   * @return true if ok, false errors.
   */
  TL_DLL bool TeGetTempFileName( std::string& filename );    
  
  /**
   * @brief The file size (bytes).
   *
   * @note Throws an exception if file not found. 
   * @param filename The file name.
   * @return The file size (bytes).
   */
  TL_DLL unsigned long int TeGetFileSize( const std::string& filename );  
  
  /**
   * @brief Check the file existence.
   *
   * @param filename The file name.
   * @return true if the file exists, false if not.
   */
  TL_DLL bool TeCheckFileExistence( const std::string& filename );   


/** @} */ 
/** @} */ 

#endif


