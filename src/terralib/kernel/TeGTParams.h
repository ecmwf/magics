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
/*! \file TeGTParams.h
    \brief This file contains the definition of parameters necessary to build a    TeGeometricTransformation instance.
*/

#include "TeDefines.h"

#ifndef TEGTPARAMS_H
  #define TEGTPARAMS_H
  
  #include "TeMatrix.h"
  #include "TeCoord2D.h"
  #include "TeSharedPtr.h"

  #include <string>

  /**
   * @brief This is the class for geometric transformation parameters .
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup Utils
   */
  class TL_DLL TeGTParams
  {
    public :
    
      /** 
       * @typedef TeSharedPtr< TeGTParams > pointer
       * Type definition for a TeGTParams instance pointer. 
       */
      typedef TeSharedPtr< TeGTParams > pointer;    
      
      /**
       * @enum OutRemStrategy
       * Outliers remotion strategy.
       */ 
      enum OutRemStrategy {
        /**
         * @brief No outliers remotion applied.
         */
        NoOutRemotion,
        /**
         * @brief Exaustive outliers remotion (all
         * possible tie-points combinations will be
         * tested.
         */
        ExaustiveOutRemotion,
        /**
         * @brief LOA (leave-worse-out) will
         * be performed.
         */
        LWAOutRemotion
      };      
    
      /**
       * @brief Transformation name.
       *
       * @note Default value = affine.
       */
      std::string transformation_name_;
      
      /** 
       * @brief The outliers remotion strategy.  
       *
       * @note Default value = NoOutRemotion.    
       */
      OutRemStrategy out_rem_strat_;
      
      /**
       * @brief The maximum allowed direct mapping error.
       *
       * @note Default value = Positive infinite.    
       */         
      double max_dmap_error_;
      
      /**
       * @brief The maximum allowed inverse mapping error.
       *
       * @note Default value = Positive infinite.    
       */         
      double max_imap_error_;      
    
      /**
       * @brief The current direct mapping transformation parameters.
       * @note Default value = An empty matrix.    
       */    
      TeMatrix direct_parameters_;
      
      /**
       * @brief The current inverse mapping transformation parameters.
       * @note Default value = An empty matrix.    
       */    
      TeMatrix inverse_parameters_;      

      /**
       * @brief The current transformation tie-points.
       * @note Default value = An empty vector.    
       */    
      std::vector< TeCoordPair > tiepoints_;       
      
      /**
       * @brief The weight matrix used by least square method.
       *
       * @note Default value = empty matrix.
       */         
      TeMatrix WMatrix_;
      
      /**
       * @brief The maximun number of iterations while processing.
       *
       * @note Default value = 20.
       */         
      unsigned int maxIters_;
      
      /**
       * @brief The tolerance used while executing an iterative processing.
       *
       * @note Default value = 0.0001.
       */         
      double tolerance_;
      
      /**
       * @brief Indicates whether use adaptive parameters or not.
       *
       * @note Default value = false.
       */         
      bool useAdaptiveParams_;      

      /**
       * @brief Default constructor
       */
      TeGTParams();
      
      /**
       * @brief Alternative constructor
       * @param external External reference.
       */
      TeGTParams( const TeGTParams& external );      
      
      /**
       * @brief Default Destructor
       */
      ~TeGTParams();
      
      /**
       * @brief operator= implementation.
       *
       * @param external External reference.
       * @return A const reference to the external object instance.
       */
      const TeGTParams& operator=( 
        const TeGTParams& external );      

      /**
       * @brief This is for TeFactory compatibility.
       */
      std::string decName() const;
      
      /**
       * @brief Reset to the default parameters.
       */
      void reset();      
            
  };

#endif
