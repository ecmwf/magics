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

/**
 * @file TeAgnostic.h
 * @brief This file contains a set of macros, routines and classes to deal with
 * system checking and logging facility.
 * They should NOT be used by anyone because the support and interfaces 
 * can be changed in future. THIS IS FOR INTERNAL USE ONLY.
 * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
 *
 * @note The following macros will be used:
 * 
 * @param TEAGN_DEBUG_MODE If defined, debug code macros will be compiled and
 * used or if not defined the compiler debug flag will be used.
 * @param TEAGN_ENABLE_STDOUT_LOG If defined, messages also will be logged
 * to STDOUT ( otherwise only TeErrorLog will log messages )
 * or if not defined the compiler debug flag will be used.
 */

#ifndef TEAGNOSTIC_H
  #define TEAGNOSTIC_H

  #include <sstream>
  #include <string>
  #include <iostream>
  
  #include <TeException.h>
  #include <TeErrorLog.h>   

  /**
   * @brief TeAgnostic debug mode selection based on NDEBUG define.
   */
  #ifndef TEAGN_DEBUG_MODE
    #ifndef NDEBUG
      /** @brief Debug mode selection flag. */
      #define TEAGN_DEBUG_MODE
      
      /** @brief STDOUT logging selection flag. */
      #define TEAGN_ENABLE_STDOUT_LOG
    #endif
  #endif   

  /**
   * @brief Logs a message to stdout
   *
   * @param message Message to be logged.
   */
  #ifdef TEAGN_ENABLE_STDOUT_LOG
    #define TEAGN_LOGMSG_STDOUT( message ) \
      std::cout << std::endl << "Message : " \
        << __FILE__ \
        << ":" << __LINE__ \
        << " - " << TeAgnostic::to_string( message ) \
        << std::endl;
  #else
    #define TEAGN_LOGMSG_STDOUT( message ) {};
  #endif

    /**
    * @brief Logs a error message to stderr
    *
    * @param message Message to be logged.
    */
  #ifdef TEAGN_ENABLE_STDOUT_LOG
    #define TEAGN_LOGERR_STDOUT( message ) \
      std::cerr << std::endl << "Error : " \
        << __FILE__ \
        << ":" << __LINE__ \
        << " - " << TeAgnostic::to_string( message ) \
        << std::endl;
  #else
    #define TEAGN_LOGERR_STDOUT( message ) {};
  #endif

    /**
    * @brief Logs a warning message to stdout
    *
    * @param message Message to be logged.
    */
  #ifdef TEAGN_ENABLE_STDOUT_LOG
    #define TEAGN_LOGWARN_STDOUT( message ) \
      std::cout << std::endl << "Warning : " \
        << __FILE__ \
        << ":" << __LINE__ \
        << " - " << TeAgnostic::to_string( message ) \
        << std::endl;
  #else
    #define TEAGN_LOGWARN_STDOUT( message ) {};
  #endif
        

  /**
   * @brief Logs a message.
   *
   * @param message Message to be logged.
   */
  #define TEAGN_LOGMSG( message ) \
  { \
    TeErrorLog::instance().insert( LOG_MESSAGE, \
      TeAgnostic::to_string( message ) ); \
    TEAGN_LOGMSG_STDOUT( message ); \
  };

  /**
   * @brief Logs a message.
   *
   * @param message Message to be logged.
   */
  #define TEAGN_LOGERR( message ) \
  { \
    TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE, \
      TeAgnostic::to_string( message ) ); \
    TEAGN_LOGERR_STDOUT( message ); \
  };
      
  /**
   * @brief Logs a warning message.
   *
   * @param message Message to be logged.
   */
  #define TEAGN_LOGWARN( message ) \
  { \
    TeErrorLog::instance().insert( LOG_MESSAGE, \
     TeAgnostic::to_string( message ) ); \
    TEAGN_LOGWARN_STDOUT( message ); \
  };
      
  /**
   * @brief Logs a message to stderr and throws.
   *
   * @param message Message to be logged.
   */
  #define TEAGN_LOG_AND_THROW( message ) \
  { \
    TEAGN_LOGERR_STDOUT( message ); \
    throw TeException( UNKNOWN_ERROR_TYPE, \
      TeAgnostic::to_string( message ), false ); \
  };
      
  /**
   * @brief Checks if value is true and throws an exception if not.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_TRUE_OR_THROW( value , message ) \
    if( value == 0 ) { \
      TEAGN_LOGERR_STDOUT( TeAgnostic::to_string( message ) + \
        " - " + TeAgnostic::to_string( #value ) ); \
      throw TeException( UNKNOWN_ERROR_TYPE, \
        TeAgnostic::to_string( message ), false ); \
    };      

  /**
   * @brief Variable watching.
   *
   * @param variable Variable to be logged.
   */
  #define TEAGN_WATCH( variable ) \
    { \
      TEAGN_LOGMSG( "WATCH - " + TeAgnostic::to_string( #variable ) + \
        "=" + TeAgnostic::to_string( variable ) ); \
    };

  /**
   * @brief Checks if value is true and logs an warning message if not.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_TRUE_OR_LOG( value , message ) \
    if( value == 0 ) { \
      TEAGN_LOGWARN( TeAgnostic::to_string( message ) + \
        " - " + TeAgnostic::to_string( #value ) ); \
    };

  /**
   * @brief Checks if value is true. For false values a warning message will be logged 
   * and a return of context with false value will be done.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_TRUE_OR_RETURN( value , message ) \
    if( value == 0 ) { \
      TEAGN_LOGWARN( TeAgnostic::to_string( message ) + \
        " - " + TeAgnostic::to_string( #value ) ); \
      return false; \
    };
    
  /**
   * @brief Checks if value is false. For true values a warning message 
   * will be logged 
   * and a return of context with false value will be done.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_FALSE_OR_RETURN( value , message ) \
    if( value != 0 ) { \
      TEAGN_LOGWARN( TeAgnostic::to_string( message ) + \
        " - " + TeAgnostic::to_string( #value ) ); \
      return false; \
    };    

  /**
   * @brief Logs a warning message will and return false.
   *
   * @param message Message to be logged.
   */
  #define TEAGN_LOG_AND_RETURN( message ) \
    { \
      TEAGN_LOGWARN( message ); \
      return false; \
    };

  /**
   * @brief Checks if value is false and logs an warning message if not.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_FALSE_OR_LOG( value , message ) \
    if( value != 0 ) { \
      TEAGN_LOGWARN( TeAgnostic::to_string( message ) + \
        " - " + TeAgnostic::to_string( #value ) ); \
    };

  /**
   * @brief Checks if two values are equal and throws an exception if not.
   *
   * @param value1 Value to be checked.
   * @param value2 Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_CHECK_EQUAL( value1 , value2 , message ) \
    TEAGN_TRUE_OR_THROW( ( ((double)value1) == ((double)value2) ), \
    std::string( "Values must be equal [" ) + \
    TeAgnostic::to_string( value1 ) + "!=" + \
    TeAgnostic::to_string( value2 ) + "] - " + \
    TeAgnostic::to_string( message ) );

  /**
   * @brief Checks if two values are diferent and throws an exception if not.
   *
   * @param value1 Value to be checked.
   * @param value2 Value to be checked.
   * @param message Message to be logged.
   */
  #define TEAGN_CHECK_NOTEQUAL( value1 , value2 , message ) \
    TEAGN_TRUE_OR_THROW( ( ((double)value1) != ((double)value2) ), \
    std::string( "Values can't be equal [" ) + \
    TeAgnostic::to_string( #value1 ) + std::string( "==" ) + \
    TeAgnostic::to_string( #value2 ) + std::string( "==" ) + \
    TeAgnostic::to_string( value1 ) + std::string( "]" ) );

  /**
   * @brief  Checks if two values are equal ( within an EPS ) and
   *  throws an exception if not.
   *
   * @param value1 Value to be checked.
   * @param value2 Value to be checked.
   * @param eps EPS ( threshold )
   * @param message Message to be logged.
   */
  #define TEAGN_CHECK_EPS( value1 , value2 , eps , message ) \
    { \
      TEAGN_TRUE_OR_THROW( ( eps >= 0), "Invalid eps" ); \
      double TEAGN_CHECK_EPS_double_diff = 0; \
      double TEAGN_CHECK_EPS_double_value1 = (double)value1; \
      double TEAGN_CHECK_EPS_double_value2 = (double)value2; \
      double TEAGN_CHECK_EPS_double_eps = (double)eps; \
      if( TEAGN_CHECK_EPS_double_value1 < TEAGN_CHECK_EPS_double_value2 ) { \
        TEAGN_CHECK_EPS_double_diff = ( TEAGN_CHECK_EPS_double_value2 - \
          TEAGN_CHECK_EPS_double_value1 ); \
      } else { \
        TEAGN_CHECK_EPS_double_diff = ( TEAGN_CHECK_EPS_double_value1 - \
          TEAGN_CHECK_EPS_double_value2 ); \
      }; \
      TEAGN_TRUE_OR_THROW( \
        ( TEAGN_CHECK_EPS_double_diff <= TEAGN_CHECK_EPS_double_eps ), \
        std::string( "Values are not equal: " ) + \
        TeAgnostic::to_string( #value1 ) + \
        std::string( "=[") + \
        TeAgnostic::to_string( TEAGN_CHECK_EPS_double_value1 ) + \
        std::string( "] " ) + \
        TeAgnostic::to_string( #value2 ) + \
        std::string( "=[") + \
        TeAgnostic::to_string( TEAGN_CHECK_EPS_double_value2 ) + \
        std::string( "] eps=[") + \
        TeAgnostic::to_string( TEAGN_CHECK_EPS_double_eps ) + \
        std::string( "] diff=[") + \
        TeAgnostic::to_string( TEAGN_CHECK_EPS_double_diff ) + \
        std::string( "] - " ) + \
        TeAgnostic::to_string( message ) \
        ); \
    };

  /**
   * @brief Throws an exception for not implemented source.
   */
  #define TEAGN_NOT_IMPLEMENTED \
    TEAGN_LOG_AND_THROW( "Not Implemented." );

  /**
   *  @brief Checks if Debug mode is enabled and throws an exception if not.
   */
  #define TEAGN_DEBUG_MODE_CHECK \
    TEAGN_TRUE_OR_THROW( TeAgnostic::debugModeCheck() , \
    "Code not compiled with debug" );
    
  /**
   * @brief Checks if value is true and throws an exception if not.
   *
   * @note This macro will be disabled for non debug mode.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #ifdef TEAGN_DEBUG_MODE
    #define TEAGN_DEBUG_CONDITION( value , message ) \
      TEAGN_TRUE_OR_THROW( value , message );
  #else
    #define TEAGN_DEBUG_CONDITION( value , message ) {};
  #endif
  
  /**
   * @brief Checks if value is true. For false values a warning message will be logged 
   * and a return of context with false value will be done.
   *
   * @note This macro will be disabled for non debug mode.
   *
   * @param value Value to be checked.
   * @param message Message to be logged.
   */
  #ifdef TEAGN_DEBUG_MODE
    #define TEAGN_DEBUG_RETURN( value , message ) \
      TEAGN_TRUE_OR_RETURN( value , message );
  #else
    #define TEAGN_DEBUG_RETURN( value , message ) {};
  #endif  

  /**
   * @brief This namespace contains a set of routines and classes to deal with
   *  system checking and logging facility.
   * @ingroup Utils
   */
  namespace TeAgnostic{

    /**
     * @brief Data conversion to string.
     *
     * @param data Data to be converted.
     * @return The converted string.
     */
     template< class T >
     std::string to_string( const T& data )
     {
        std::stringstream temp_ss;
        temp_ss.setf(std::ios_base::fixed);
        temp_ss << data;
        return temp_ss.str();
     }

    /**
     * @brief Checks if the code was compiled with debug mode.
     *
     * @return true if debug mode was used at compilation time. false if not.
     */
     TL_DLL bool debugModeCheck();

  };

#endif
