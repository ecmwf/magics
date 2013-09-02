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
#include "TeRasterMemManager.h"

#include "TeUtils.h"
#include "TeErrorLog.h"
#include "TeAgnostic.h"


TeRasterMemManager::TeRasterMemManager()
{
  init();
}


TeRasterMemManager::~TeRasterMemManager()
{
  clear();
}


void TeRasterMemManager::clear()
{
  /* Cleaning the ram tiles and related structures */
  
  TilesPtrsStlVecT::iterator ram_tiles_it = ram_tiles_vec_.begin();
  TilesPtrsStlVecT::iterator ram_tiles_it_end = ram_tiles_vec_.end();
  
  while( ram_tiles_it != ram_tiles_it_end ) {
    delete[] ( (TilePtrT)(*ram_tiles_it) );
    ++ram_tiles_it;
  }
  
  if( all_tiles_ptrs_vec_ ) {
    delete[] all_tiles_ptrs_vec_;
  }
  
  /* cleaning containers */
  
  bands_tiles_sizes_.clear();
  ram_tiles_vec_.clear();
  mm_objs_vec_.clear();
  tile2mm_vec_.clear();
  mm2tile_map_.clear();

  /* Reseting the the default values */
  
  init();
}

bool TeRasterMemManager::reset( unsigned int bands, 
  unsigned int tiles_per_band, const std::vector< unsigned int >& tiles_sizes,
  MemoryPolicy mem_pol )
{
  clear();
  
  TEAGN_TRUE_OR_RETURN( ( ( bands != 0 ) && ( tiles_per_band != 0 ) &&
    ( tiles_sizes.size() == bands ) ), "Invalid parameters" );

  tiles_per_band_ = tiles_per_band;
  bands_nmb_ = bands;
  bands_tiles_sizes_ = tiles_sizes;
  
  const unsigned int total_tiles_number = tiles_per_band_ * bands_nmb_;
  
  /* Allocating the tiles vector */
  
  all_tiles_ptrs_vec_ = new TilePtrT[ total_tiles_number ];
  TEAGN_TRUE_OR_RETURN( all_tiles_ptrs_vec_,
    "Unable to allocate the tiles pointers vector" )
  
  for( unsigned int tiles_vects_index = 0 ; 
    tiles_vects_index < total_tiles_number ;
    ++tiles_vects_index ) {
      
    all_tiles_ptrs_vec_[ tiles_vects_index ] = 0;
  }     
  
  /* Allocating tiles */
  
  if( mem_pol == DiskMemPol ) {
    return allocateMMTiles( 0, 0 );
  } else {
    unsigned int curr_band = 0;
    unsigned int curr_tile = 0;
    TilePtrT newtile_ptr = 0;
    const unsigned long int max_ram_tiles = getMaxRAMTiles( bands_nmb_,
      tiles_per_band_, bands_tiles_sizes_ );
    unsigned int curr_ram_tiles_nmb = 0; 
    
    for( curr_band = 0 ; curr_band < bands ; ++curr_band ) {
      unsigned int tile_bytes = bands_tiles_sizes_[ curr_band ];
      TEAGN_TRUE_OR_THROW( ( tile_bytes > 0 ), "Invalid tile bytes" );      
      
      unsigned long int curr_tile_index = 0;
      
      for( curr_tile = 0 ; curr_tile < tiles_per_band_ ; ++curr_tile ) {
        curr_tile_index = ( curr_band * tiles_per_band_ ) + curr_tile;
        
        switch( mem_pol ) {
          case RAMMemPol :
          {
            newtile_ptr = (TilePtrT) new TileDataT[ tile_bytes ];
              
            if( newtile_ptr == 0 ) {
              clear();
              
              return false;
            } else {
              all_tiles_ptrs_vec_[ curr_tile_index ] = 
                newtile_ptr;
                
              ram_tiles_vec_.push_back( newtile_ptr );
            }
              
            break;
          }
          case AutoMemPol :
          {
            if( curr_ram_tiles_nmb <= max_ram_tiles ) {
              newtile_ptr = (TilePtrT) new TileDataT[ tile_bytes ];
                
              if( newtile_ptr == 0 ) {
                return allocateMMTiles( curr_band, curr_tile );
              } else {
                all_tiles_ptrs_vec_[ curr_tile_index ] = 
                  newtile_ptr;
                  
                ram_tiles_vec_.push_back( newtile_ptr );
                
                ++curr_ram_tiles_nmb;
              }
            } else {
              return allocateMMTiles( curr_band, curr_tile );
            }
                            
            break;
          }
          default :
          {
            clear();
            
            TEAGN_LOG_AND_THROW( "Invalid memory policy" );
      
            break;
          }
        }
      }
    }
  }
  
  return true;
}


void TeRasterMemManager::init()
{
  tiles_per_band_ = 0;
  bands_nmb_ = 0;
  max_mm_file_file_size_ = 1024 * 1024 * 10;
  current_active_mmobj_ptr_ = 0;
  all_tiles_ptrs_vec_ = 0;
}


bool TeRasterMemManager::allocateMMTiles( 
  unsigned int starting_band_index, unsigned int starting_tile_index )
{
  TEAGN_TRUE_OR_THROW( ( bands_tiles_sizes_.size() == bands_nmb_ ),
    "Invalid tile sizes vector" );    
  TEAGN_TRUE_OR_THROW( ( starting_band_index < bands_nmb_ ),
    "Invalid starting_band_index" );    
  TEAGN_TRUE_OR_THROW( ( starting_tile_index < tiles_per_band_ ),
    "Invalid starting_tile_index" );    
    
  /* Resizing the tiles vector */
  
  const unsigned long int total_tiles_number = tiles_per_band_ * bands_nmb_;
  
  tile2mm_vec_.resize( total_tiles_number );
  
  for( unsigned int tile2mm_vec_index = 0 ; 
    tile2mm_vec_index < total_tiles_number ;
    ++tile2mm_vec_index ) {
      
    tile2mm_vec_[ tile2mm_vec_index ] = 0;
  } 
  
  /* Allocating mapped memory objects */    
  
  for( unsigned int curr_band = starting_band_index ; curr_band < bands_nmb_ ; 
    ++curr_band ) {
  
    const unsigned int tile_size = bands_tiles_sizes_[ curr_band ];
    TEAGN_TRUE_OR_THROW( ( tile_size <= max_mm_file_file_size_ ), 
      "Invalid tile size" );
    
    const unsigned long int tiles_per_file = ( unsigned long int )
      floor( ( (double)max_mm_file_file_size_ ) / ( (double) tile_size ) );
      
    const unsigned long int file_size = (unsigned long int)
      ( tiles_per_file * tile_size );
    
    unsigned int curr_tiles_in_file = tiles_per_file + 1;
      
    unsigned int curr_tile = 0;
    if( curr_band == starting_band_index ) {
      curr_tile = starting_tile_index;
    }
    
    unsigned long int curr_tile_index = 0;
    
    TeMappedMemory* last_mm_ptr = 0;
      
    while( curr_tile < tiles_per_band_ ) {
      curr_tile_index = ( curr_band * tiles_per_band_ ) + curr_tile;
    
      if( curr_tiles_in_file >= tiles_per_file ) {
        TeSharedPtr< TeMappedMemory > mm_ptr( 
          new TeMappedMemory() );   
             
        if( ! mm_ptr.isActive() ) {
          TEAGN_LOGERR( "Unable to create mapped memory object" );
          
          clear();
          
          return false;        
        }
        
        if( ! mm_ptr->reset( file_size, false ) ) {
          TEAGN_LOGERR( "Unable to allocate memory mapped file" );

          clear();
          
          return false;        
        }
        
        last_mm_ptr = mm_ptr.nakedPointer();
        
        mm_objs_vec_.push_back( mm_ptr );
        
        MM2TileMapNodeT new_mm2tile_node;
        new_mm2tile_node.first = curr_tile_index;
        new_mm2tile_node.second = curr_tile_index;
        
        mm2tile_map_[ last_mm_ptr ] = new_mm2tile_node;
        
        curr_tiles_in_file = 0;
      }
      
      tile2mm_vec_[ curr_tile_index ] = last_mm_ptr;
      
      mm2tile_map_[ last_mm_ptr ].second = curr_tile_index;
      
      ++curr_tiles_in_file;
      
      ++curr_tile;
    }
    
  }
  
  return true;
}


unsigned long int TeRasterMemManager::getMaxRAMTiles( unsigned int bands, 
  unsigned int tiles_per_band,
  const std::vector< unsigned int >& tiles_sizes)
{
  unsigned long int max_ram_tiles = 0;
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    const unsigned int max_ram  = (unsigned int)
      ( 0.90 * ( (double)TeGetFreePhysicalMemory() ) );
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX  
    const unsigned int max_ram  = (unsigned int)
      ( 0.75 * ( (double)TeGetTotalPhysicalMemory() ) );    
  #else
    #error "Unsupported platform"
  #endif    
  
  unsigned long int curr_used_ram = 0;
  
  for( unsigned int curr_band = 0 ; curr_band < bands ; 
    ++curr_band ) {
  
    const unsigned int tile_size = tiles_sizes[ curr_band ];
    
    for( unsigned int curr_tile = 0 ; curr_tile < tiles_per_band ;
      ++curr_tile ) {
      
      if( ( curr_used_ram + tile_size ) > max_ram ) {
        curr_tile = tiles_per_band;
        curr_band = bands;
      } else {
        ++max_ram_tiles;
        curr_used_ram += tile_size;
      }
    }
  }

  return max_ram_tiles;
}


void* TeRasterMemManager::getTilePointer( const unsigned int& band, 
  const unsigned int& tile)
{
  const unsigned int tileindex = ( band * tiles_per_band_ ) + tile;
  
  TEAGN_DEBUG_CONDITION( ( tileindex < ( tiles_per_band_ * bands_nmb_ ) ),
    "The required band/tile out of range" );
    
  TilePtrT tileptr = all_tiles_ptrs_vec_[ tileindex ];
  
  if( tileptr == 0 ) {
    /* Disabling the current mapping - only one active mapping allowed */
  
    if( current_active_mmobj_ptr_ ) {
      MM2TilesMapT::iterator it = 
        mm2tile_map_.find( current_active_mmobj_ptr_ );
        
      TEAGN_DEBUG_CONDITION( ( it != mm2tile_map_.end() ), 
        "Cannot find tile into mapped memory to tiles map" );
    
      current_active_mmobj_ptr_->toggle( false );
      
      const unsigned long int first_tile_index = it->second.first;
      const unsigned long int last_tile_index_bound = it->second.second + 1;
      
      for( unsigned long int all_tiles_ptrs_vec_index = first_tile_index ;
        all_tiles_ptrs_vec_index < last_tile_index_bound ; 
        ++all_tiles_ptrs_vec_index ) {
        
        all_tiles_ptrs_vec_[ all_tiles_ptrs_vec_index ] = 0;
      }
    }
    
    /* Enabling the required mapping */
    
    TEAGN_DEBUG_CONDITION( ( tileindex < tile2mm_vec_.size() ),
      "Tiles to mapped memory objects vector index mismatch" );
    
    current_active_mmobj_ptr_ = tile2mm_vec_[ tileindex ];
    TEAGN_DEBUG_CONDITION( ( current_active_mmobj_ptr_ != 0 ),
      "Invalid active mapping object" );
    TEAGN_TRUE_OR_THROW( current_active_mmobj_ptr_->toggle( true ),
      "Unable to activate mapped memory" );
    
    /* Updating main vactor mapped memory tiles pointers */
    
    TilePtrT mm_start_ptr = ( TilePtrT )
      current_active_mmobj_ptr_->getPointer();
      
    TEAGN_DEBUG_CONDITION( ( band < bands_tiles_sizes_.size() ),
      "Invalid band" );
    const unsigned long int tile_size = bands_tiles_sizes_[ band ];    
    
    MM2TilesMapT::iterator it = 
      mm2tile_map_.find( current_active_mmobj_ptr_ );
    TEAGN_DEBUG_CONDITION( ( it != mm2tile_map_.end() ),
      "Cannot find tile into mapped memory to tiles map" );     
      
    const unsigned long int first_tile_index = it->second.first;
    const unsigned long int last_tile_index_bound = it->second.second + 1;
    
    for( unsigned long int all_tiles_ptrs_vec_index = first_tile_index ;
      all_tiles_ptrs_vec_index < last_tile_index_bound ; 
      ++all_tiles_ptrs_vec_index ) {
      
      all_tiles_ptrs_vec_[ all_tiles_ptrs_vec_index ] = mm_start_ptr;
      
      mm_start_ptr += tile_size;
    }    
    
    tileptr = all_tiles_ptrs_vec_[ tileindex ];
  }
  
  TEAGN_DEBUG_CONDITION( ( tileptr != 0 ), "Invalid tile pointer" )
  
  return tileptr;
}


