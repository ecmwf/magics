/*
TerraLib - a library for developing GIS applications.
Copyright  2001, 2002, 2003 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular
purpose. The library provided hereunder is on an "as is" basis, and the
authors have no obligation to provide maintenance, support, updates,
enhancements, or modifications.
In no event shall INPE be held liable to any party
for direct, indirect, special, incidental, or consequential damages arising
out of the use of this library and its documentation.
*/

#include "TeProjectiveGT.h"
#include "TeGeometry.h"

#include <assert.h>
#include <math.h>


TeProjectiveGT::TeProjectiveGT() :
  deltaX_( 0 ), deltaY_( 0 ), deltaU_( 0 ), deltaV_( 0 )
{
}


TeProjectiveGT::~TeProjectiveGT()
{
}


void TeProjectiveGT::directMap( const TeGTParams& params, const TeCoord2D& pt1, 
    TeCoord2D& pt2 ) const
{
  assert( params.direct_parameters_.Ncol() == 3 );
  assert( params.direct_parameters_.Nrow() == 3 );

  double x = pt1.x();
  double y = pt1.y();

  if( ! params.useAdaptiveParams_ ) 
  {
    // denominador da fc projetiva    
    double den = params.direct_parameters_( 2, 0 ) * x + params.direct_parameters_( 2, 1 ) * y + 1;
  
    pt2.x( ( params.direct_parameters_( 0, 0 ) * x + 
      params.direct_parameters_( 0, 1 ) * y + 
      params.direct_parameters_( 0, 2 ) ) / den );
    pt2.y( ( params.direct_parameters_( 1, 0 ) * x + 
      params.direct_parameters_( 1, 1 ) * y + 
      params.direct_parameters_( 1, 2 ) ) / den ); 
  }
  else // use Adaptive parameters
  {
    x -= deltaX_;
    y -= deltaY_;
    
    double den = params.direct_parameters_( 2, 0 ) * x + params.direct_parameters_( 2, 1 ) * y + 1;
    
    pt2.x( ( params.direct_parameters_( 0, 0 ) * x + 
      params.direct_parameters_( 0, 1 ) * y + 
      params.direct_parameters_( 0, 2 ) ) / den 
      + deltaU_ );
    pt2.y( ( params.direct_parameters_( 1, 0 ) * x + 
      params.direct_parameters_( 1, 1 ) * y + 
      params.direct_parameters_( 1, 2 ) ) / den 
      + deltaV_ ); 
  }
}


void TeProjectiveGT::inverseMap( const TeGTParams& params, const TeCoord2D& pt2, 
  TeCoord2D& pt1 ) const
{
  assert( params.inverse_parameters_.Ncol() == 3 );
  assert( params.inverse_parameters_.Nrow() == 3 );

  double u = pt2.x();
  double v = pt2.y();

  if( ! params.useAdaptiveParams_  ) 
  {
    // denominador da fc projetiva
    double den = params.inverse_parameters_( 2, 0 ) * u + params.inverse_parameters_( 2, 1 ) * v + 1;

    pt1.x( ( params.inverse_parameters_( 0, 0 ) * u + 
      params.inverse_parameters_( 0, 1 ) * v + 
      params.inverse_parameters_( 0, 2 ) ) / den );
    pt1.y( ( params.inverse_parameters_( 1, 0 ) * u + 
      params.inverse_parameters_( 1, 1 ) * v + 
      params.inverse_parameters_( 1, 2 ) ) / den );
  }
  else // use Adaptive parameters
  {
    u -= deltaU_;
    v -= deltaV_;
    
    double den = params.inverse_parameters_( 2, 0 ) * u + params.inverse_parameters_( 2, 1 ) * v + 1;

    pt1.x( ( params.inverse_parameters_( 0, 0 ) * u + 
      params.inverse_parameters_( 0, 1 ) * v + 
      params.inverse_parameters_( 0, 2 ) ) / den 
      + deltaX_ );
    pt1.y( ( params.inverse_parameters_( 1, 0 ) * u + 
      params.inverse_parameters_( 1, 1 ) * v + 
      params.inverse_parameters_( 1, 2 ) ) / den 
      + deltaY_ );
  }
}


unsigned int TeProjectiveGT::getMinRequiredTiePoints() const
{
  // At least four points
  return 4;
}


bool TeProjectiveGT::computeParameters( TeGTParams& params )
{
  TEAGN_DEBUG_CONDITION( ( params.tiepoints_.size() > 3 ),
    "Invalid tie-points size" )
    
  // calcula os parametros adaptativos
  // caso contrario, reinicia o estado
  if( params.useAdaptiveParams_ )
    setAdaptativeParameters( params.tiepoints_ );
  else
    deltaX_ = deltaY_ = deltaU_ = deltaV_ = 0;

  // a forma do calculo depende da qtd pontos
  const unsigned int tiepoints_size = params.tiepoints_.size();

  if( tiepoints_size > 4 ) 
  {
    // Solucao com sobredeterminacao -> ajustamento
    // mapeamento direto
    if( ! useAdjustment( params, true ) )
      return false;

    // mapeamento inverso
    return useAdjustment( params, false );
  } 
  else 
  {
    // Solucao deterministica
    // mapeamento direto
    if( ! useDeterministic( params, true ) )
      return false;

    // mapeamento inverso
    return useDeterministic( params, false );
    
  } // fim if tiepoints

}

bool TeProjectiveGT::useAdjustment( TeGTParams& params, bool isDirectMapping )
{
    /* 
        u = b11.x + b12.y + b13
            -------------------
               b31.x + b32.y
            
        v = b21.x + b22.y + b23
            -------------------
               b31.x + b32.y
               
        Method adapted from Albertz and Kreiling (1989).
        Reference: Albertz, J.; Kreiling, W. Photogrametriches taschenbuch. 
        Karlsruhe: Wichmann, 1989.
        
        Adaptive parameters by Xavier et al. (2007).
        Reference: Xavier, E.; Fonseca, L.; d'Alge, J.; Castejon, E. Implementacao 
        da transformacao projetiva na TerraLib: uma analise comparativa com a 
        transformacao afim. Anais do XIII Simposio Brasileiro de Sensoriamento 
        Remoto. <http://www.dsr.inpe.br/sbsr2007/>, 2007.
    */

  // inicializacao das variaveis
  
  // tamanho de Lb
  // Lb : vetor das observacoes brutas (originais)
  const unsigned int tiepoints_size = params.tiepoints_.size();

  // A: matriz do sistema de equacoes
  // inicializa com zeros
  TeMatrix A;
  if( ! A.Init( 2*tiepoints_size, 8, 0.0 ) ) 
  {
    return false;
  }

  // L: vetor das correcoes das observacoes
  TeMatrix L;
  if( ! L.Init( 2*tiepoints_size, 1 ) ) 
  {
    return false;
  }

  // At: transposta de A
  TeMatrix At;

  // N = At*P*A
  TeMatrix N;

  // N_inv : inversa de N
  TeMatrix N_inv;

  // U = At*P*Lb
  TeMatrix U;

  // Xa : vetor dos parametros ajustados
  TeMatrix Xa;

  // X0 : vetor dos parametros iniciais 
  TeMatrix X0;

  // X = Xa-X0 : vetor das correcoes 
  TeMatrix X;

  // W: matriz peso das observacoes
  bool hasWMatrix = ( ( params.WMatrix_.Nrow() == 
    (int)( tiepoints_size * 2 ) ) && ( params.WMatrix_.Ncol() == 
    (int)( tiepoints_size * 2 ) ) ) ? true : false;

  // V: residuos das observacoes

  // iterador p/ correr as observacoes
  std::vector< TeCoordPair >::const_iterator iterator;  

  // Inicializacao de X0
  // [1 0 0 0 1 0 0 0]t
  X0.Init( 8, 1, 0.0 );
  X0(0, 0) = 1;
  X0(4, 0) = 1;


  /**************************************************************\
  Solucao com sobredeterminacao -> ajustamento
  \**************************************************************/

  // 0) Montando o laco while ate q o valor convirja (isTolReached) 
  // ou alcance o Nr max de iteracoes (maxIters_)

  // contagem das iteracoes
  unsigned int iter = 0;

  // alcancou a tolerancia?
  bool isTolReached = false;

  while( iter++ < params.maxIters_ && ! isTolReached ) 
  {
    // inicializando o iterador dos pontos observados (u/v)
    iterator = params.tiepoints_.begin();

    for ( unsigned int i = 0; i < tiepoints_size; ++i ) 
    {
      // pontos usados em todo o laco
      // xi,yi : from
      // ui,ui : to
      double xi, yi; 
      double ui, vi;
      
      if( isDirectMapping ) 
      {
        xi = iterator->pt1.x() - deltaX_;
        yi = iterator->pt1.y() - deltaY_;
        ui = iterator->pt2.x() - deltaU_;
        vi = iterator->pt2.y() - deltaV_;
      } 
      else 
      {
        xi = iterator->pt2.x() - deltaU_;
        yi = iterator->pt2.y() - deltaV_;
        ui = iterator->pt1.x() - deltaX_;
        vi = iterator->pt1.y() - deltaY_;
      }

      // denominador da fc projetiva
      // den = b31 * xi + b32 * yi + 1
      double den = X0( 6, 0 ) * xi + X0( 7, 0 ) * yi + 1;

      // funcao projetiva p/ U e V
      // U = ( b11 * xi + b12 * yi + b13 ) / den
      double funcU = ( X0( 0, 0 ) * xi + X0( 1, 0 ) * yi + X0( 2, 0 ) - xi * ui * X0( 6, 0 ) -yi * ui * X0( 7, 0 ) );

      // V = ( b21 * xi + b22 * yi + b23 ) / den
      double funcV = ( X0( 3, 0 ) * xi + X0( 4, 0 ) * yi + X0( 5, 0 ) - xi * vi * X0( 6, 0 ) -yi * vi * X0( 7, 0 ) );

      // 1) Calculo de L
      // L = L0 - Lb 
      L( 2*i  , 0 ) = (funcU - ui) / den;
      L( 2*i+1, 0 ) = (funcV - vi) / den;

      // 2) Montagem de A
      // lembrando q jah tem zero em tudo
      A( 2*i   , 0 ) = xi / den;
      A( 2*i   , 1 ) = yi / den;
      A( 2*i   , 2 ) = 1  / den;
      A( 2*i   , 6 ) = -xi*ui/den;
      A( 2*i   , 7 ) = -yi*ui/den;

      A( 2*i+1 , 3 ) = A( 2*i, 0 );
      A( 2*i+1 , 4 ) = A( 2*i, 1 );
      A( 2*i+1 , 5 ) = A( 2*i, 2 );
      A( 2*i+1 , 6 ) = -xi*vi/den;
      A( 2*i+1 , 7 ) = -yi*vi/den;

      ++iterator;
    } // fim for_tiepoints

    // 3) Calculo de N
    A.Transpose( At );

    // Se W_ esta definida
    if( hasWMatrix ) 
    {
      N = At * params.WMatrix_;
      N = N * A;
    } 
    else 
      N = At * A;

    // 4) Calculo de U
    // Se W_ esta definida
    if( hasWMatrix ) 
    {
      U = At * params.WMatrix_;
      U = U * L;
    } 
    else 
      U = At * L;

    // 5) Calculo de X
    if( ! N.Inverse( N_inv ) )
      return false;

    X = - N_inv * U;

    // 6) Calculo de Xa
    Xa = X0 + X;

    // 7) Atualiza o X0 p/ proxima iteracao
    X0 = Xa;

    // 8) Checando se a tolerancia foi atingida

    isTolReached = true;
    for( unsigned int j = 0; j < 8; ++j ) 
    {
      if( fabs( X( j, 0 ) ) > params.tolerance_ ) 
      {
        isTolReached = false;
        break;
      }
    }

  } // fim while

  // final - montagem da resposta
  

  if( isDirectMapping ) 
  {
    if( ! params.direct_parameters_.Init( 3, 3 ) )
      return false;
    
    params.direct_parameters_(0,0) = Xa(0,0);
    params.direct_parameters_(0,1) = Xa(1,0);
    params.direct_parameters_(0,2) = Xa(2,0);
  
    params.direct_parameters_(1,0) = Xa(3,0);
    params.direct_parameters_(1,1) = Xa(4,0);
    params.direct_parameters_(1,2) = Xa(5,0);
  
    params.direct_parameters_(2,0) = Xa(6,0);
    params.direct_parameters_(2,1) = Xa(7,0);
    params.direct_parameters_(2,2) = 1;
  }
  else
  {
    if( ! params.inverse_parameters_.Init( 3, 3 ) )
      return false;
    
    params.inverse_parameters_(0,0) = Xa(0,0);
    params.inverse_parameters_(0,1) = Xa(1,0);
    params.inverse_parameters_(0,2) = Xa(2,0);
  
    params.inverse_parameters_(1,0) = Xa(3,0);
    params.inverse_parameters_(1,1) = Xa(4,0);
    params.inverse_parameters_(1,2) = Xa(5,0);
  
    params.inverse_parameters_(2,0) = Xa(6,0);
    params.inverse_parameters_(2,1) = Xa(7,0);
    params.inverse_parameters_(2,2) = 1;
  }

  return true;
}


bool TeProjectiveGT::useDeterministic( TeGTParams& params, bool isDirectMapping )
{

  // Solucao deterministica, da forma:
  // X = A^-1 . L

  // o vetor de ptos sempre tera tamanho 4
  const unsigned int tiepoints_size = 4;

  // A: matriz do sistema de equacoes
  // inicializa com zeros
  TeMatrix A;
  if( ! A.Init( 2*tiepoints_size, 8, 0.0 ) ) 
  {
    return false;
  }

  // L: vetor das observacoes
  TeMatrix L;
  if( ! L.Init( 2*tiepoints_size, 1 ) ) 
  {
    return false;
  }

  // inversa de A
  TeMatrix A_inv;

  // resultado
  TeMatrix X;

  // iterador p/ correr as observacoes
  std::vector< TeCoordPair >::const_iterator iterator = params.tiepoints_.begin();;  

  // montagem de A e L
  for ( unsigned int i = 0; i < tiepoints_size; ++i ) {

    // pontos usados em todo o laco
    // xi,yi : from
    // ui,ui : to
    double xi, yi; 
    double ui, vi;

    if( isDirectMapping ) 
    {
      xi = iterator->pt1.x();
      yi = iterator->pt1.y();
      ui = iterator->pt2.x();
      vi = iterator->pt2.y();
    } 
    else 
    {
      xi = iterator->pt2.x();
      yi = iterator->pt2.y();
      ui = iterator->pt1.x();
      vi = iterator->pt1.y();
    }

    // 1) Calculo de L
    L( 2*i  , 0 ) = ui;
    L( 2*i+1, 0 ) = vi;

    // 2) Montagem de A
    // lembrando q jah tem zero em tudo
    A( 2*i   , 0 ) = xi;
    A( 2*i   , 1 ) = yi;
    A( 2*i   , 2 ) = 1;
    A( 2*i   , 6 ) = - xi * ui;
    A( 2*i   , 7 ) = - yi * ui;

    A( 2*i+1 , 3 ) = xi;
    A( 2*i+1 , 4 ) = yi;
    A( 2*i+1 , 5 ) = 1;
    A( 2*i+1 , 6 ) = - xi * vi;
    A( 2*i+1 , 7 ) = - yi * vi;

    ++iterator;

  } // fim for_tiepoints

  // 3) Calculo de A_inv
  A.Inverse( A_inv );

  // 4) Calculo de X
  X = A_inv * L;

  // final - montagem da resposta
  
    if( isDirectMapping ) 
  {
    if( ! params.direct_parameters_.Init( 3, 3 ) )
      return false;
    
    params.direct_parameters_(0,0) = X(0,0);
    params.direct_parameters_(0,1) = X(1,0);
    params.direct_parameters_(0,2) = X(2,0);
  
    params.direct_parameters_(1,0) = X(3,0);
    params.direct_parameters_(1,1) = X(4,0);
    params.direct_parameters_(1,2) = X(5,0);
  
    params.direct_parameters_(2,0) = X(6,0);
    params.direct_parameters_(2,1) = X(7,0);
    params.direct_parameters_(2,2) = 1;
  }
  else
  {
    if( ! params.inverse_parameters_.Init( 3, 3 ) )
      return false;
    
    params.inverse_parameters_(0,0) = X(0,0);
    params.inverse_parameters_(0,1) = X(1,0);
    params.inverse_parameters_(0,2) = X(2,0);
  
    params.inverse_parameters_(1,0) = X(3,0);
    params.inverse_parameters_(1,1) = X(4,0);
    params.inverse_parameters_(1,2) = X(5,0);
  
    params.inverse_parameters_(2,0) = X(6,0);
    params.inverse_parameters_(2,1) = X(7,0);
    params.inverse_parameters_(2,2) = 1;
  }

  return true;
}

void TeProjectiveGT::setAdaptativeParameters( const std::vector< TeCoordPair >& tiepoints )
{
  deltaX_ = TeMAXFLOAT;
  deltaY_ = TeMAXFLOAT;
  deltaU_ = TeMAXFLOAT;
  deltaV_ = TeMAXFLOAT;
  
  const unsigned int tiepoints_size = tiepoints.size();
  
  // iterador p/ correr as observacoes
  std::vector< TeCoordPair >::const_iterator iterator = tiepoints.begin();
  
  for ( unsigned int i = 0; i < tiepoints_size; ++i ) 
  {
    double xi, yi; 
    double ui, vi;
      
    // obtem os valores
    xi = iterator->pt1.x();
    yi = iterator->pt1.y();
    ui = iterator->pt2.x();
    vi = iterator->pt2.y();
    
    // checa se eh o menor - cada um
    if( xi < deltaX_ ) deltaX_ = xi;
    if( yi < deltaY_ ) deltaY_ = yi;
    if( ui < deltaU_ ) deltaU_ = ui;
    if( vi < deltaV_ ) deltaV_ = vi;
    
  }

}

