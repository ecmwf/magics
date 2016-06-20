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
/*! \file TeDefines.h
    \brief Provides a set of general definitions used by Terralib
*/
#ifndef  __TERRALIB_INTERNAL_DEFINES_H
#define  __TERRALIB_INTERNAL_DEFINES_H

#include <string> 

/** @defgroup Defines Defines and constants
  @{
 */

 
/** @defgroup PlatformDetection Platform detection macros.
  @{
 */
/** @brief Microsoft Windows platform code */ 
#define TePLATFORMCODE_MSWINDOWS 1
/** @brief Generic Linux platform code */ 
#define TePLATFORMCODE_LINUX 2
/** @brief Solaris platform code */ 
#define TePLATFORMCODE_SOLARIS 3
/** @brief Apple platform code */ 
#define TePLATFORMCODE_APPLE 4
/** @brief IBM AIX platform code */ 
#define TePLATFORMCODE_AIX 5

#if defined( WIN32 ) || defined( __WIN32__ ) || defined ( _WIN32 ) || defined( WIN64 ) || defined( _WIN32_WCE )
  #define TePLATFORM TePLATFORMCODE_MSWINDOWS
#elif defined( __LINUX__ ) || defined( linux ) || defined( __linux__ ) ||defined( __FreeBSD__ ) //FAMI
  #define TePLATFORM TePLATFORMCODE_LINUX
#elif defined( __APPLE_CC__ ) || defined( APPLE_CC )
  #define TePLATFORM TePLATFORMCODE_LINUX
  #define __unix__
#elif defined(sparc) && defined(sun) && defined(unix) 
  #define TePLATFORM TePLATFORMCODE_SOLARIS
#elif defined(_AIX)
  #define TePLATFORM TePLATFORMCODE_AIX
  #ifndef __unix__  
   #define __unix__
  #endif 
#else
  #error "ERROR: Unknown platform"
#endif
/** @} */ 

/** @defgroup CompilerDetection Compiler detection macros.
  @{
 */
/** @brief MS Visual Studio c++ code */ 
#define TeCOMPILERCODE_VCPLUSPLUS 1
/** @brief Borland c++ code */ 
#define TeCOMPILERCODE_BCPLUSPLUS 2
/** @brief GNU Compiler code */ 
#define TeCOMPILERCODE_GNUC 3
/** @brief CodeWarrior code */ 
#define TeCOMPILERCODE_CODEWARRIOR 4
/** @brief Sun Forte code */ 
#define TeCOMPILERCODE_SUNFORTE7 5
/** @brief AIX xlC code */ 
#define TeCOMPILERCODE_XLC 6

#if defined( _MSC_VER )
  #define TeCOMPILER TeCOMPILERCODE_VCPLUSPLUS
#elif defined( __BORLANDC__ ) || defined( __BCPLUSPLUS__ )
  #define TeCOMPILER TeCOMPILERCODE_BCPLUSPLUS
#elif defined( __GNUC__ )
  #define TeCOMPILER TeCOMPILERCODE_GNUC
#elif defined( __MWERKS__ )
  #define TeCOMPILER TeCOMPILERCODE_CODEWARRIOR
#elif defined( __SUNPRO_CC)
  #define TeCOMPILER TeCOMPILERCODE_SUNFORTE7
#elif defined( __xlC__ )
  #define TeCOMPILER TeCOMPILERCODE_XLC
#else
  #error "ERROR: Unknown compiler"
#endif
/** @} */ 


/** @defgroup MathConst Mathematical constants and macro definitions
  @{
 */
//! A default name length
const int	TeNAME_LENGTH = 2000;			//!< A default name length
const double	TeMAXFLOAT =	3.4E37;			//!< Maximum float value
const double	TeMINFLOAT =	3.4E-37;		//!< Minimum float value
const double	TePI  =	3.14159265358979323846;		//!< The ratio of the circumference to the diameter of a circle
const double	TeCDR =	0.01745329251994329576;		//!< Conversion factor: degrees to radians
const double	TeCRD = 57.29577951308232087679;	//!< Conversion factor: radians to degrees
const double 	TeEARTHRADIUS   = 6378160.;		//!< Int. Astronomical Union - 1965

#ifndef MAX
#define MAX(a,b) ( (a>b) ? a : b )	//!< Macro that returns max between two values
#endif

#ifndef MIN
#define MIN(a,b) ( (a<b) ? a : b )	//!< Macro that returns min between two values
#endif

#ifndef ABS
#define ABS(x) 	( ((x) >= 0) ? (x) : -(x) )	//!< Macro that returns the absolute value
#endif
/** @} */


/** @defgroup Selection Mode for the objects
	@{
 */
#define TeDEFAULT			0	//!< default selection
#define TePOINTED			1	//!< object pointed
#define TeQUERIED   		2	//!< object queried
#define TePOINTED_QUERIED	3	//!< object pointed and queried
/** @} */	// end Selection Mode


/** @defgroup Orient Ring orientation
	@{
 */
#define TeNOTURN			 0	//!< Unknown orientation
#define TeCOLLINEAR			 0	//!< Collinear
#define TeCLOCKWISE   		-1	//!< Clockwise orientation
#define TeCOUNTERCLOCKWISE	 1	//!< Counter-Clockwise orientation
/** @} */	// end Orientation

/** @defgroup TopDefines Topology related defines
    @{
 */
/** @defgroup InOut Inside/Outside positioning
	@ingroup TopDefines
    @{
 */
#define	TeUNKNOWNPOSITION  0	//!< unknown position
#define	TeINSIDE		   1	//!< inside position
#define	TeOUTSIDE		   2	//!< outside position
#define	TeBOUNDARY		   4	//!< on the boundary position
/** @} */


/** @defgroup Inter Intersection types
	@ingroup TopDefines
    @{
 */
/** @defgroup GeneralInter General intersection
	@ingroup Inter
	@{
 */
#define TeINTERIORINTERIOR 8	//!< interior and interior intersection
#define TeINTERIORBOUNDARY 16	//!< interior and boundary intersection
#define TeINTERIOREXTERIOR 32	//!< interior and exterior intersection
#define TeBOUNDARYINTERIOR 64	//!< boundary and interior intersection
#define TeBOUNDARYBOUNDARY 128	//!< boundary and boundary intersection
#define TeBOUNDARYEXTERIOR 256	//!< boundary and exterior intersection
#define TeEXTERIORINTERIOR 512	//!< exterior and interior intersection
#define TeEXTERIORBOUNDARY 1024	//!< exterior and boundary intersection
#define TeEXTERIOREXTERIOR 2048	//!< exterior and exterior intersection
/** @} */ // end GeneralInter

/** @defgroup SegInter Segments intersection
	@ingroup Inter
	@{
 */
#define TeDONT_INTERSECT 0		//!< segments don't intersects
#define TeDO_INTERSECT   1		//!< segments intersects
#define TeAT_ENDPOINT    4		//!< segments intersects at end points
/** @} */	// end SegInter
/** @} */  // end Inte

/** @defgroup TopOper Topological operations
	@{
 */
#define TeDIFFERENCE   1	//!< Difference operation
#define TeUNION        2	//!< Union operation
#define TeINTERSECTION 4	//!< Intersection operation
/** @} */ // end TopOper
/** @} */ // end TopDefines
/** @} */ // end Defines

//! Current terralib database version
const std::string TeDBVERSION = "3.2.0.1"; 		

/* @brief TeISNAN definition */

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  #include <float.h>
  #define TeISNAN( x ) _isnan( x )
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  #include <math.h>
  #define TeISNAN( x ) isnan( x )
#else
  #include <math.h>  
  #define TeISNAN( x ) isnan( x )
#endif

/** @defgroup TerraLib_AS_DLL macros.
  @{
 */
#if defined( WIN32 ) || defined( __WIN32__ ) || defined ( _WIN32 ) || defined( WIN64 ) || defined( _WIN32_WCE )

#if defined(_MSC_VER) /* MSVC Compiler */
#pragma warning(disable: 4251)
#endif

#ifdef TL_AS_DLL
#define TL_DLL __declspec(dllexport)
#else
#define TL_DLL __declspec(dllimport)
#endif

#else
#define TL_DLL
#endif
/** @} */ 

#endif

