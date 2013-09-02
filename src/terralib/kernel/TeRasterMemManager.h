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
/*! \file TeRasterMemManager.h
    \brief This file contains a class that deals with a raster memory management
*/

#ifndef TERASTERMEMMANAGER_H
  #define TERASTERMEMMANAGER_H

  #include "TeMappedMemory.h"
  #include "TeSharedPtr.h"
  
  #include <vector>
  #include <map>
  
  /**
   * @brief This class deals with a raster memory management.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   */
  class TL_DLL TeRasterMemManager {
  
    public :
    
      /**
       * @enum Memory policy.
       */ 
      enum MemoryPolicy {
        /**
         * @brief Automatic memory policy ( Try to use RAM or DISK, 
         * if there is no avaliable RAM ).
         */
        AutoMemPol = 1,
        /**
         * @brief RAM memory policy.
         */
        RAMMemPol = 2,
        /**
         * @brief Disk memory policy ( virtual mapped memory ).
         */
        DiskMemPol = 3
      };    
    
      /**
       * @brief Default Constructor.
       */
      TeRasterMemManager();    

      /**
       * @brief Default Destructor
       */
      ~TeRasterMemManager();
      
      /**
       * @brief Clear all data structures.
       */
      void clear();
      
      /**
       * @brief Reset the instance following new raster parameters.
       * @param bands The number of bands (channels).
       * @param tiles_per_band The tiles inside each band.
       * @param tiles_sizes The tile size (bytes) for each band.
       * @param mem_pol Memory policy.
       * @return true if OK, false on errors.
       */
      bool reset( unsigned int bands, unsigned int tiles_per_band,
        const std::vector< unsigned int >& tiles_sizes,
         MemoryPolicy mem_pol );

      /**
       * @brief Returnas a pointer to a internal allocated tile.
       * @param band Band index.
       * @param tile Tile index.
       * @return a pointer to a internal allocated tile. 
       * @note This pointer is only valid until the next call to this function.
       */
      void* getTilePointer( const unsigned int& band, 
        const unsigned int& tile );      
      
    protected :
    
      /**
       * @typedef unsigned char TileDataT 
       * Tile data type.
       */    
      typedef unsigned char TileDataT;    
    
      /**
       * @typedef TileDataT* TilePtrT
       * Tile pointer type.
       */    
      typedef TileDataT* TilePtrT;
      
      /**
       * @typedef std::vector< TilePtrT > TilesPtrsStlVecT 
       * RAM Tiles vector type.
       */    
      typedef std::vector< TilePtrT > TilesPtrsStlVecT;   
      
      /**
       * @typedef std::vector< TeSharedPtr< TeMappedMemory > > MMemsVecT 
       * Mapped memory objects vector type.
       */      
      typedef std::vector< TeSharedPtr< TeMappedMemory > > MMemsVecT;
      
      /**
       * @typedef std::pair< unsigned long int, unsigned long int > MM2TileMapNodeT
       * Mapped memory to first and last tiles indexes map node type.
       */       
      typedef std::pair< unsigned long int, unsigned long int > 
        MM2TileMapNodeT;
      
      /**
       * @typedef std::multimap< TeMappedMemory*, std::pair< unsigned long int, unsigned long int > MM2TilesMapT
       * Mapped memory to first and last tiles indexes map type.
       */        
      typedef std::map< TeMappedMemory*, MM2TileMapNodeT > MM2TilesMapT;
      
      /**
       * @brief Tiles per band.
       */    
      unsigned int tiles_per_band_;
      
      /**
       * @brief The number of bands.
       */    
      unsigned int bands_nmb_;
      
      /**
       * @brief The maximum mapped memory file size.
       */     
      unsigned long int max_mm_file_file_size_;      
      
      /**
       * @brief A pointer to the current active memory map object.
       */     
      TeMappedMemory* current_active_mmobj_ptr_;
      
      /**
       * @brief A vector of tiles sizes for each band.
       */        
      std::vector< unsigned int > bands_tiles_sizes_;
    
      /**
       * @brief A vector of pointers to all allocated tiles.
       * @note Declared as a simple vector to optimize the
       * tile access.
       */     
      TilePtrT* all_tiles_ptrs_vec_;
      
      /**
       * @brief RAM bands pointers vector.
       */    
      TilesPtrsStlVecT ram_tiles_vec_;
      
      /**
       * @brief Mapped memory objects pointers vector.
       */    
      MMemsVecT mm_objs_vec_;      
      
      /**
       * @brief A vector mapping tiles indexes to their respective mapped 
       * memory object.
       */        
      std::vector< TeMappedMemory* > tile2mm_vec_;

      /**
       * @brief A map from mapped memory object pointers to their respective
       * tile indexes.
       */        
      MM2TilesMapT mm2tile_map_;
            
      /**
       * @brief Initiates the internal variables to their default values.
       */    
      void init();
      
      /**
       * @brief Allocate mapped memory tiles starting at a specific
       * band and tile number.
       * @param starting_band_index Starting Band index.
       * @param starting_tile_index Starting tile index.
       * @return true if OK, false on errors.
       * @note bands_ptr_ must be previously allocated.
       */    
      bool allocateMMTiles( 
        unsigned int starting_band_index,
        unsigned int starting_tile_index );
        
      /**
       * @brief Return the maximum RAM tiles that can fit in the
       * current RAM for all bands.
       * @param bands The number of bands (channels).
       * @param tiles_per_band The tiles inside each band.
       * @param tiles_sizes The tile size (bytes) for each band.       
       * @return Return the maximum RAM tiles.
       */    
      unsigned long int getMaxRAMTiles( unsigned int bands, 
        unsigned int tiles_per_band,
        const std::vector< unsigned int >& tiles_sizes );        
      
    private :
    
      /**
       * @brief Alternative Constructor.
       * @param ext External reference.
       */    
      TeRasterMemManager( const TeRasterMemManager& ) {};
       
      /**
       * @brief =operator implementation.
       * @param ext External reference.
       */    
      const TeRasterMemManager& operator=( const TeRasterMemManager& )
        { return *this; };
      
  };
  
#endif

