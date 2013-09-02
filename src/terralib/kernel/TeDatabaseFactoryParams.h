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
/*! \file TeDatabaseFactoryParams.h
    \brief This file contains the definition of parameters necessary to build a TeDatabase
*/

#ifndef TEDATABASEFACTORYPARAMS_H
  #define TEDATABASEFACTORYPARAMS_H

  #include "TeDefines.h"
  #include <string>

  /**
   * @brief This is the class for TeDatabase factory parameters .
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup DatabaseUtils
   */
  class TL_DLL TeDatabaseFactoryParams
  {
    public :
    
      std::string dbms_name_;	//!< DBMS name (like PostgreSQL, PostGIS, MySQL and others).
      std::string host_;		//!< Host name.
      std::string user_;		//!< User name in order to stabilish a connection.
      std::string password_;	//!< User password.
      std::string database_;	//!< Database name.
      int port_;				//!< Port number for network connection.

      //! Default constructor
      TeDatabaseFactoryParams();
      
      //! Default Destructor
      virtual ~TeDatabaseFactoryParams();
      
      //! operator== implementation.
	  /*! \param external External reference.
	      \return true if both instances are equal, false if not.
      */
      bool operator==( const TeDatabaseFactoryParams& external ) const;
      
      //! operator= implementation.
	  /*! \param external External reference.
	      \return A const reference to the external object instance.
      */
      const TeDatabaseFactoryParams& operator=( 
        const TeDatabaseFactoryParams& external );      

      //! This is for TeFactory compatibility.
      std::string decName() const;
  };
#endif
