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

#include "TeAffineGT.h"

#include "TeAgnostic.h"


TeAffineGT::TeAffineGT()
{
}


TeAffineGT::~TeAffineGT()
{
}


void TeAffineGT::directMap( const TeGTParams& params, const TeCoord2D& pt1, 
  TeCoord2D& pt2 ) const
{
  TEAGN_DEBUG_CONDITION( ( params.direct_parameters_.Ncol() == 3 ),
    "Invalid number of columns" )
  TEAGN_DEBUG_CONDITION( ( params.direct_parameters_.Nrow() == 3 ),
    "Invalid number of rows" )
  
  double x = pt1.x();
  double y = pt1.y();

  pt2.x( params.direct_parameters_(0,0) * x + 
         params.direct_parameters_(0,1) * y + 
         params.direct_parameters_(0,2) );
  pt2.y( params.direct_parameters_(1,0) * x + 
         params.direct_parameters_(1,1) * y + 
         params.direct_parameters_(1,2) );  
}


void TeAffineGT::inverseMap( const TeGTParams& params, const TeCoord2D& pt2, 
  TeCoord2D& pt1 ) const
{
  TEAGN_DEBUG_CONDITION( ( params.inverse_parameters_.Ncol() == 3 ),
    "Invalid number of columns" )
  TEAGN_DEBUG_CONDITION( ( params.inverse_parameters_.Nrow() == 3 ),
    "Invalid number of rows" )

  double u = pt2.x();
  double v = pt2.y();
  
  pt1.x( params.inverse_parameters_(0,0) * u +
         params.inverse_parameters_(0,1) * v + 
         params.inverse_parameters_(0,2) );
  pt1.y( params.inverse_parameters_(1,0) * u + 
         params.inverse_parameters_(1,1) * v + 
         params.inverse_parameters_(1,2) );  
}


unsigned int TeAffineGT::getMinRequiredTiePoints() const
{
  return 3;
}


bool TeAffineGT::computeParameters( TeGTParams& params )
{
    /*
            u = a1.x + a2.y + a3
            v = b1.x + b2.y + b3

            A . X + L = 0
            N = At . A
            U = At . L
            X = - (N ^ -1) . U

            A = |x1 y1 1  0  0  0|     Xt = |a1 a2 a3 b1 b2 b3|
                |0  0  0  x1 y1 1|
                |x2 y2 1  0  0  0|
                |0  0  0  x2 y2 1|     Lt = |u1 v1 u2 v2 ... un vn|
                |.  .  .  .  .  .|
                |xn yn 1  0  0  0|
                |0  0  0  xn yn 1|

    */

  const unsigned int tiepoints_size = params.tiepoints_.size();
  TEAGN_DEBUG_CONDITION( ( tiepoints_size > 2 ),
    "Invalid tie-points size" )

  TeMatrix A;
  if( ! A.Init( 2*tiepoints_size, 6 ) ) {
    return false;
  }
    
  TeMatrix L;
  if( ! L.Init( 2*tiepoints_size, 1 ) ) {
    return false;
  }

  TeMatrix At;
  TeMatrix N;
  TeMatrix U;
  TeMatrix X;
  TeMatrix N_inv;

  std::vector< TeCoordPair >::const_iterator iterator;

  TeCoord2D x_y;
  TeCoord2D u_v;
  double x = 0;
  double y = 0;
  
  /* L calcule */

  iterator = params.tiepoints_.begin() ;
  for ( unsigned int L_block_offset = 0 ; ( L_block_offset < tiepoints_size ); 
    ++L_block_offset ) {
    
    u_v = iterator->pt2;
    
    L( L_block_offset*2     , 0) = u_v.x();
    L( L_block_offset*2 + 1 , 0) = u_v.y();
    
    ++iterator ;
  }
  
  /* A calcule */

  iterator = params.tiepoints_.begin();
  for ( unsigned int A_block_offset = 0 ; (A_block_offset < tiepoints_size) ; 
        ++A_block_offset) {
        
    x_y = iterator->pt1;    
    x = x_y.x();
    y = x_y.y();
    
    A( A_block_offset*2  , 0 ) = x ;
    A( A_block_offset*2  , 1 ) = y ;
    A( A_block_offset*2  , 2 ) = 1 ;
    A( A_block_offset*2  , 3 ) = 0 ;
    A( A_block_offset*2  , 4 ) = 0 ;
    A( A_block_offset*2  , 5 ) = 0 ;
    A( A_block_offset*2+1, 0 ) = 0 ;
    A( A_block_offset*2+1, 1 ) = 0 ;
    A( A_block_offset*2+1, 2 ) = 0 ;
    A( A_block_offset*2+1, 3 ) = x ;
    A( A_block_offset*2+1, 4 ) = y ;
    A( A_block_offset*2+1, 5 ) = 1 ; 
    
    ++iterator;
  }

  /* At calcule */
  A.Transpose( At );

  /* N calcule */
  N = At * A;

  /* U calcule */
  U = At * L;

  /* N_inv calcule */
  if ( N.Inverse( N_inv ) ) {
    /* X calcule */

    X = N_inv * U;
    
    if( ! params.direct_parameters_.Init( 3, 3 ) ) {
      return false;
    }    

    params.direct_parameters_(0,0) = X(0,0);
    params.direct_parameters_(0,1) = X(1,0);
    params.direct_parameters_(0,2) = X(2,0);
    params.direct_parameters_(1,0) = X(3,0);
    params.direct_parameters_(1,1) = X(4,0);
    params.direct_parameters_(1,2) = X(5,0);
    params.direct_parameters_(2,0) = 0;
    params.direct_parameters_(2,1) = 0;
    params.direct_parameters_(2,2) = 1;

    if( params.direct_parameters_.Inverse( params.inverse_parameters_ ) ) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;   
  }
}

