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
/*! \file TeAffineGT.h
   \brief Affine geometric trasformation.
*/


#ifndef TEAFFINEGT_H
  #define TEAFFINEGT_H
  
  #include "TeDefines.h"
  #include <TeGeometricTransformation.h>

  /**
   * @brief This is the class to deal affine geometric trasformation.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup Utils
   */
  class TL_DLL TeAffineGT : public TeGeometricTransformation {
    public :

      /**
       * @brief Default Constructor.
       */
      TeAffineGT();
    
      /**
       * @brief Default Destructor
       */
      ~TeAffineGT();
      
      /**
       * @brief Returns the minimum number of required tie-points for the current
       * transformation.
       *
       * @return The minimum number of required tie-points for the current
       * transformation.
       */       
      unsigned int getMinRequiredTiePoints() const;      
      
    protected :
    
      /**
       * @brief Direct mapping ( from pt1 space into pt2 space ).
       *
       * @param params Transformation parameters.
       * @param pt1 pt1 coordinate.
       * @param pt2 pt2 coordinate.
       */      
      void directMap( const TeGTParams& params, const TeCoord2D& pt1, 
        TeCoord2D& pt2 ) const;
      
      /**
       * @brief Inverse mapping ( from pt2 space into pt1 space ).
       *
       * @param params Transformation parameters.
       * @param pt2 pt2 coordinate.
       * @param pt1 pt1 coordinate.
       */      
      void inverseMap( const TeGTParams& params, const TeCoord2D& pt2, 
        TeCoord2D& pt1 ) const;      
      
      /**
       * @brief Calculate the transformation parameters following the
       * new supplied tie-points.
       *
       * @param params Transformation parameters.
       * @return true if OK, false on errors.
       */       
      bool computeParameters( TeGTParams& params );
  };
  
#endif

