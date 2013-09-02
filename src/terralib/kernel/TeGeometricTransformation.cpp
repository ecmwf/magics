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

#include "TeGeometricTransformation.h"

#include <TeAgnostic.h>

#include <math.h>
#include <float.h>

#include <iostream>


TeGeometricTransformation::TeGeometricTransformation()
{
}


TeGeometricTransformation::~TeGeometricTransformation()
{
}


void TeGeometricTransformation::getParameters( TeGTParams& params ) const
{
  params = internal_params_;
}


bool TeGeometricTransformation::reset( const TeGTParams& newparams )
{
  /* If previous calculated parameters were supplied, no need to do calcules */

  if( ( newparams.direct_parameters_.Nrow() > 0 ) && 
    ( newparams.direct_parameters_.Ncol() > 0 ) &&
    ( newparams.inverse_parameters_.Nrow() > 0 ) &&
    ( newparams.inverse_parameters_.Ncol() > 0 ) 
    ) {
    
    internal_params_ = newparams;
    return true;
  } else {
    /* No previous parameters given - Need to calculate the new transformation
       parameters */
       
    TEAGN_TRUE_OR_THROW( ( newparams.max_dmap_error_ >= 0 ),
      "Invalid maximum allowed direct mapping error" );
    TEAGN_TRUE_OR_THROW( ( newparams.max_imap_error_ >= 0 ),
      "Invalid maximum allowed inverse mapping error" );
       
    const unsigned int req_tie_pts_nmb = getMinRequiredTiePoints();

    if( newparams.tiepoints_.size() < req_tie_pts_nmb ) {
      return false;
    } else {
      internal_params_ = newparams;
      
      switch( newparams.out_rem_strat_ ) {
        case TeGTParams::NoOutRemotion :
        {
          if( computeParameters( internal_params_ ) ) {
            
            if( ( newparams.max_dmap_error_ >= 
              getDirectMappingError( internal_params_ ) ) &&
              ( newparams.max_imap_error_ >= 
              getInverseMappingError( internal_params_ ) ) ) {
              
              return true;
            }
          }   
        
          break;
        }
        case TeGTParams::ExaustiveOutRemotion :
        {
          if( exaustiveOutRemotion( internal_params_ ) ) {
            return true;
          }
        
          break;
        }
        case TeGTParams::LWAOutRemotion :
        {
          if( LWAOutRemotion( internal_params_ ) ) {
            return true;
          }
                  
          break;
        }        
        default : 
        {
          TEAGN_LOG_AND_THROW( 
            "Invalid outliers remotion strategy" )
          break;
        }
      
      }
    }
  }
  
  internal_params_.reset();
  
  return false;
}


double TeGeometricTransformation::getDirectMappingError(  
  const TeGTParams& params ) const
{
  unsigned int tiepoints_size = params.tiepoints_.size();
  
  TEAGN_DEBUG_CONDITION( ( tiepoints_size > 0 ),
    "Invalid tie-points size" )
  
  double max_error = 0;
  double current_error = 0;
  
  for( unsigned int tpindex = 0 ; tpindex < tiepoints_size ; ++tpindex ) {
    current_error = getDirectMappingError( params.tiepoints_[ tpindex ], 
      params );
    
    if( current_error > max_error ) {
      max_error = current_error;
    }
  }
  
  return max_error;
}


double TeGeometricTransformation::getInverseMappingError(  
  const TeGTParams& params ) const
{
  unsigned int tiepoints_size = params.tiepoints_.size();
  
  TEAGN_DEBUG_CONDITION( ( tiepoints_size > 0 ),
    "Invalid tie-points size" )

  
  double max_error = 0;
  double current_error = 0;
  
  for( unsigned int tpindex = 0 ; tpindex < tiepoints_size ; ++tpindex ) {
    current_error = getInverseMappingError( params.tiepoints_[ tpindex ], 
      params );
    
    if( current_error > max_error ) {
      max_error = current_error;
    }
  }
  
  return max_error;
}


double TeGeometricTransformation::getDirectMappingError( 
  const TeCoordPair& tie_point, const TeGTParams& params ) const
{
  TeCoord2D direct_mapped_point;

  directMap( params, tie_point.pt1, direct_mapped_point );
    
  double diff_x = tie_point.pt2.x() - direct_mapped_point.x();
  double diff_y = tie_point.pt2.y() - direct_mapped_point.y();
    
  return hypot( diff_x, diff_y );
}


double TeGeometricTransformation::getInverseMappingError( 
  const TeCoordPair& tie_point, const TeGTParams& params ) const
{
  TeCoord2D inverse_mapped_point;

  inverseMap( params, tie_point.pt2, inverse_mapped_point );
    
  double diff_x = tie_point.pt1.x() - inverse_mapped_point.x();
  double diff_y = tie_point.pt1.y() - inverse_mapped_point.y();
    
  return hypot( diff_x, diff_y );
}


bool TeGeometricTransformation::recombineSeed( std::vector<unsigned int>& seed, 
  const unsigned int& seedpos, const unsigned int& elements_nmb ) const
{
  unsigned int seed_size = seed.size();
  
  if( seedpos >= seed_size ) {
    return false;
  }

  if( seed[ seedpos ]  >= ( elements_nmb - seed_size + seedpos + 1 ) ) {
    if( seedpos == seed_size - 1 ) {
      return false;
    } else if( seedpos == 0 ) {
      return recombineSeed( seed, seedpos + 1, elements_nmb ) ;
    } else {
      return recombineSeed( seed, seedpos + 1, elements_nmb ) ;
    };
  } else if( seed[ seedpos ]  == 0 ) {
    if( seedpos == 0 ) {
      seed[ seedpos ] = 1 ;
      return recombineSeed( seed, seedpos + 1, elements_nmb );
    } else if( seedpos == seed_size - 1 ) {
      seed[ seedpos ] = seed[ seedpos - 1 ] + 1;
      return true;
    } else {
      seed[ seedpos ] = seed[ seedpos - 1 ] + 1;
      seed[ seedpos + 1 ] = 0;
      return recombineSeed( seed, seedpos + 1, elements_nmb );
    }
  } else {
    if( seedpos == seed_size - 1 ) {
      seed[ seedpos ] = seed[ seedpos ] + 1;
      return true;
    } else if( seedpos == 0 ) {
      if( recombineSeed( seed, seedpos + 1, elements_nmb ) ) {
        return true;
      } else {
        seed[ seedpos ] = seed[ seedpos ] + 1;
        seed[ seedpos + 1 ] = 0;
        return recombineSeed( seed, seedpos + 1, elements_nmb );
      }
    } else {
      if( recombineSeed( seed, seedpos + 1, elements_nmb ) ) {
        return true;
      } else {
        seed[ seedpos ] = seed[ seedpos ] + 1;
        seed[ seedpos + 1 ] = 0;
        return recombineSeed( seed, seedpos + 1, elements_nmb );
      }
    }
  }
}


TeGeometricTransformation* TeGeometricTransformation::DefaultObject( 
  const TeGTParams& )
{ 
  throw TeException( UNKNOWN_ERROR_TYPE, 
      "Trying to create an invalid TeGemetricTransformation instance" );
  
  return 0;
}; 


bool TeGeometricTransformation::exaustiveOutRemotion( 
  TeGTParams& params )
{
  TEAGN_DEBUG_CONDITION( ( params.out_rem_strat_ == 
    TeGTParams::ExaustiveOutRemotion ), 
    "Inconsistent outliers remotion strategy" )
    
  const unsigned int req_tie_pts_nmb = getMinRequiredTiePoints();
    
  /* Initiating seed */
  
  std::vector<unsigned int> comb_seed_vec;
  
  for( unsigned int comb_seed_vec_index = 0 ; 
    comb_seed_vec_index < req_tie_pts_nmb ;
    ++comb_seed_vec_index ) {
    
    comb_seed_vec.push_back( 0 );
  }
  
  /* Trying to find the best tie-points by building 
    the transformation with the highest number of
    tie-points, but with an acceptable mapping error */
    
  std::vector< TeCoordPair > best_tie_points;
  
  const unsigned int tiepoints_size = params.tiepoints_.size();
  unsigned int tiepoints_index = 0;
  bool point_already_present = false;
  TeGTParams curr_params = params;
  
  while( recombineSeed( comb_seed_vec, 0, tiepoints_size ) ) {
    /* Extracting tie-points from the original vector */
    
    curr_params.tiepoints_.clear();
    
    for( unsigned int comb_seed_vec_index_2 = 0 ; 
      comb_seed_vec_index_2 < req_tie_pts_nmb ;
      ++comb_seed_vec_index_2 ) {
      
      curr_params.tiepoints_.push_back( 
        params.tiepoints_[ comb_seed_vec[ comb_seed_vec_index_2 ] - 
        1 ] );
    }
    
    /* Trying to generate a valid transformation */
    
    if( computeParameters( curr_params ) ) {
    
      if( ( params.max_dmap_error_ >= getDirectMappingError( 
        curr_params ) ) &&
        ( params.max_imap_error_ >= getInverseMappingError( 
        curr_params ) ) ) {
      
        /* Trying to insert more tie-points into current 
            transformation */
        
        for( tiepoints_index = 0 ; tiepoints_index < tiepoints_size ;
          ++tiepoints_index ) {
          
          /* Verifying if the current tie-point is already present */
          
          point_already_present = false;
          
          for( unsigned int comb_seed_vec_index_3 = 0 ; 
            comb_seed_vec_index_3 < req_tie_pts_nmb ;
            ++comb_seed_vec_index_3 ) {
          
            if( tiepoints_index == ( 
              comb_seed_vec[ comb_seed_vec_index_3 ] - 1 ) ) {
            
              point_already_present = true;
              break;
            }
          }
          
          if( ! point_already_present ) {
            curr_params.tiepoints_.push_back( 
              params.tiepoints_[ tiepoints_index ] );
            
            /* Verifying if the new tie-point insertion does not generate 
              an invalid transformation */
            
            if( computeParameters( curr_params ) ) {
                
              if( ( params.max_dmap_error_ < 
                getDirectMappingError( curr_params ) ) ||
                ( params.max_imap_error_ < 
                getInverseMappingError( curr_params ) ) ) {
                
                curr_params.tiepoints_.pop_back();
              }
            } else {
              curr_params.tiepoints_.pop_back();
            }
          }
        }
        
        /* A valid transformation was generated, now verifying if the
          number of tie-poits is greater then the current best 
          transformation
        */            
        
        if( curr_params.tiepoints_.size() > best_tie_points.size() ) {
          best_tie_points = curr_params.tiepoints_;
        }          
      }
    }
  }
  
  if( best_tie_points.size() >= req_tie_pts_nmb ) {
    curr_params.tiepoints_ = best_tie_points;
  
    if( computeParameters( curr_params ) ) {
      if( ( params.max_dmap_error_ >= getDirectMappingError( 
        curr_params ) ) &&
        ( params.max_imap_error_ >= getInverseMappingError( 
        curr_params ) ) ) {
        
        params = curr_params;
        
        return true;
      }
    }
  }
  
  return false;      
}


bool TeGeometricTransformation::LWAOutRemotion( 
  TeGTParams& params )
{
  TEAGN_DEBUG_CONDITION( ( params.out_rem_strat_ == 
    TeGTParams::LWAOutRemotion ), 
    "Inconsistent outliers remotion strategy" )
    
  const unsigned int req_tie_pts_nmb = getMinRequiredTiePoints();
  
  if( params.tiepoints_.size() == req_tie_pts_nmb ) {
    return computeParameters( params );
  } else if( params.tiepoints_.size() > req_tie_pts_nmb ) {
  
    /* Computing the initial global transformation */
    
    if( ! computeParameters( params ) ) {
      return false;
    }    
    
    if( ( getDirectMappingError( params ) <= 
      params.max_dmap_error_ ) && 
      ( getInverseMappingError( params ) <= 
      params.max_imap_error_ ) ) {
    
      /* This transformation has no outliers */
      
      return true;
    }
    
    /* Global vars */
    
    const double max_dmap_error = params.max_dmap_error_;
    const double max_imap_error = params.max_imap_error_;
      
    TeGTParams params_aux = params;
    
    TeGTParams best_params;
    double best_params_dmap_error = DBL_MAX;
    double best_params_imap_error = DBL_MAX;
    
    unsigned int tpindex = 0;
    unsigned int tpsize = 0;
    double curr_dmap_error = 0;
    double curr_imap_error = 0;
    TeCoordPair* curr_tp_ptr = 0;
    TeCoordPair* curr_canditate_tp_ptr = 0;
    bool transformation_not_updated = false;
    
    /* vectors with direct and inverse mapping errors for
       the current transformation */
    std::vector< double > curr_trans_dmap_err_vec;
    curr_trans_dmap_err_vec.reserve( params.tiepoints_.size() );
    std::vector< double > curr_trans_imap_err_vec;
    curr_trans_imap_err_vec.reserve( params.tiepoints_.size() );
    
    /* The min and max values from the vectors above */
    
    double curr_trans_dmap_err_vec_min = 0;
    double curr_trans_dmap_err_vec_max = 0;
    double curr_trans_dmap_err_vec_range = 0;
    double curr_trans_imap_err_vec_min = 0;
    double curr_trans_imap_err_vec_max = 0;
    double curr_trans_imap_err_vec_range = 0;
    
    /* A map of tie-points pointers ordered by the mean of
       normalized direct and inverse mapping errors */
    std::map< double, TeCoordPair* > aux_tps_map;
    
    std::map< double, TeCoordPair* >::const_reverse_iterator 
      aux_tps_map_it;
    std::map< double, TeCoordPair* >::const_reverse_iterator 
      aux_tps_map_it_end;      
    
    /* Iterating over the current transformation tie-points */
  
    while( params.tiepoints_.size() > req_tie_pts_nmb ) {
      tpsize = params.tiepoints_.size();
      transformation_not_updated = true;
      
      /* Calculating the two mapping errors vectors */
      
      curr_trans_dmap_err_vec_min = DBL_MAX;
      curr_trans_dmap_err_vec_max = ( -1.0 ) * DBL_MAX;
      curr_trans_imap_err_vec_min = DBL_MAX;
      curr_trans_imap_err_vec_max = ( -1.0 ) * DBL_MAX;      
      
      for( tpindex = 0 ; tpindex < tpsize ; ++tpindex ) {
        curr_trans_dmap_err_vec[ tpindex ] = 
          getDirectMappingError( params.tiepoints_[ tpindex ], 
          params );
        if( curr_trans_dmap_err_vec_min > 
          curr_trans_dmap_err_vec[ tpindex ] ) {
          
          curr_trans_dmap_err_vec_min = 
            curr_trans_dmap_err_vec[ tpindex ];
        }
        if( curr_trans_dmap_err_vec_max <
          curr_trans_dmap_err_vec[ tpindex ] ) {
          
          curr_trans_dmap_err_vec_max = 
            curr_trans_dmap_err_vec[ tpindex ];
        }        
        
        curr_trans_imap_err_vec[ tpindex ] = 
          getInverseMappingError( params.tiepoints_[ tpindex ],
          params );
        if( curr_trans_imap_err_vec_min > 
          curr_trans_imap_err_vec[ tpindex ] ) {
          
          curr_trans_imap_err_vec_min = 
            curr_trans_imap_err_vec[ tpindex ];
        }
        if( curr_trans_imap_err_vec_max <
          curr_trans_imap_err_vec[ tpindex ] ) {
          
          curr_trans_imap_err_vec_max = 
            curr_trans_imap_err_vec[ tpindex ];
        }            
      }
      
      curr_trans_dmap_err_vec_range = curr_trans_dmap_err_vec_max -
        curr_trans_dmap_err_vec_min;
      if( curr_trans_dmap_err_vec_range == 0.0 ) {
        curr_trans_dmap_err_vec_range = 1.0;
      }
              
      curr_trans_imap_err_vec_range = curr_trans_imap_err_vec_max -
        curr_trans_imap_err_vec_min;
      if( curr_trans_imap_err_vec_range == 0 ) {
        curr_trans_imap_err_vec_range = 1.0;
      }        
      
      /* Creating a map with the current tie-points ordered
         by the normalized mapping errors means */
         
      aux_tps_map.clear();

      for( tpindex = 0 ; tpindex < tpsize ; ++tpindex ) {
        aux_tps_map[ 
          ( 
            ( 
              ( 
                curr_trans_dmap_err_vec[ tpindex ] - 
                curr_trans_dmap_err_vec_min
              ) 
              / curr_trans_dmap_err_vec_range 
            ) 
            + 
            ( 
              (  
                curr_trans_imap_err_vec[ tpindex ] - 
                curr_trans_imap_err_vec_min              
              ) 
              / curr_trans_imap_err_vec_range 
            ) 
          )
        ] = &(params.tiepoints_[ tpindex ]);
      }
      
      /* Generating all possible transformations without 
         each tie-point */
      
      aux_tps_map_it = aux_tps_map.rbegin();
      aux_tps_map_it_end = aux_tps_map.rend();
      
      while( aux_tps_map_it != aux_tps_map_it_end ) {
        curr_canditate_tp_ptr = aux_tps_map_it->second;
        
        /* Generating a transformation parameters without the
           current tie-point */
           
        params_aux.tiepoints_.clear();
        params_aux.direct_parameters_.Clear();
        params_aux.inverse_parameters_.Clear();
            
        for( tpindex = 0 ; tpindex < tpsize ; ++tpindex ) {
          curr_tp_ptr = &(params.tiepoints_[ tpindex ]);
          
          if( curr_tp_ptr != curr_canditate_tp_ptr ) {
            params_aux.tiepoints_.push_back( *curr_tp_ptr );
          }
        }            
      
        /* Trying to generate a transformation without the current
            tie-point with tpindex index */      
        
        if( computeParameters( params_aux ) ) {
          curr_dmap_error = getDirectMappingError( params_aux );
          curr_imap_error = getInverseMappingError( params_aux );
          
          if( ( best_params_dmap_error > curr_dmap_error ) && 
            ( best_params_imap_error > curr_imap_error ) ) {
            
            best_params_dmap_error = curr_dmap_error;
            best_params_imap_error = curr_imap_error;
            best_params = params_aux;
            
            params = params_aux;
            
            transformation_not_updated = false;
            
            break; /* break the current tie-points loop */
          }
        }      
        
        ++aux_tps_map_it;
      }//while( aux_tps_map_it != aux_tps_map_it_end ) {
      
      if( transformation_not_updated ) {
        /* There is no way to improve the current transformation
           since all tie-points were tested */
        
        break; /* break the loop */
      } else {
        if( ( max_dmap_error >= best_params_dmap_error ) && 
            ( max_imap_error >= best_params_imap_error ) ) {
            
          /* A valid transformation was found */
          break;
        }
      }
    }//while( params.tiepoints_.size() > req_tie_pts_nmb ) {
    
    if( ( best_params.tiepoints_.size() >= req_tie_pts_nmb ) &&
        ( max_dmap_error >= best_params_dmap_error ) && 
        ( max_imap_error >= best_params_imap_error ) ) {
        
      params = best_params;
      
      return true;
    } else {
      return false;
    }    
  }
  
  return false;
}


