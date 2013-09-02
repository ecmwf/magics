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
/*! \file TeGTFactory.h
    \brief This file supports the implementation of Geometric transformation factories.
	\author Emiliano Ferreira Castejon <castejon@dpi.inpe.br>
*/
/**
 * @defgroup TeGTFactories Geometric transformation factories.
 * @ingroup Utils
 */  

#include "TeDefines.h"

#ifndef TEGTFACTORIES_REGISTERED
  #define TEGTFACTORIES_REGISTERED

  #include "TeAffineGTFactory.h"
  #include "TeProjectiveGTFactory.h"
#endif 

#ifndef TEGTFACTORY_H
  #define TEGTFACTORY_H

  #include "TeGeometricTransformation.h"
  #include "TeGTParams.h"
  #include "TeFactory.h"
  
  #include <string>
  
  /**
   * @brief This is the class for geometric transformations factory.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup TeGTFactories
   */
  class TL_DLL TeGTFactory : 
    public TeFactory< TeGeometricTransformation, TeGTParams >
  {
    public :
      
      /**
       * Default Destructor
       */
      virtual ~TeGTFactory();
      
    protected :
      
      /**
       * Default constructor
       *
       * @param factoryName Factory name.
       */
      TeGTFactory( const std::string& factoryName );
  };
  
#endif



