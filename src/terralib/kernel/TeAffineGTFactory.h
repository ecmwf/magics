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
/*! \file TeAffineGTFactory.h
    \brief This file contains the affine geometric trasformation factory
*/

#ifndef TEAFFINEGTFACTORY_H
  #define TEAFFINEGTFACTORY_H
  
  #include "TeDefines.h"
  #include <TeGTFactory.h>

  /**
   * @brief This is the affine geometric trasformation factory.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup Utils
   */
  class TL_DLL TeAffineGTFactory : public TeGTFactory
  {
    public :
      
      /**
       * @brief Default constructor
       */
      TeAffineGTFactory();      
      
      /**
       * @brief Default Destructor
       */
      ~TeAffineGTFactory();
      
    protected :  
    
      /**
       * @brief Implementation for the abstract TeFactory::build.
       *
       * @param arg A const reference to the used parameters.
       * @return A pointer to the new generated instance.
       */
      TeGeometricTransformation* build( const TeGTParams& arg );
      
  };

  namespace
  {  
    /** @brief A concrete factory instance for registering into the factory dictionary */
    static TeAffineGTFactory TeAffineGTFactory_instance;
  };
  
#endif

