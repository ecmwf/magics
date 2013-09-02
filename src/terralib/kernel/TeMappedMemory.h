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
/*! \file TeMappedMemory.h
    \brief This file deals with mapped memory resources
*/

#ifndef TEMAPPEDMEMORY_H
  #define TEMAPPEDMEMORY_H
  
  #include "TeSharedPtr.h"
  #include "TeDefines.h"
  
  #include <string>
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    #include <windows.h>
    #include <winbase.h>
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    #include <sys/mman.h>
  #else
    #error "ERROR: Unsupported platform"
  #endif  

  /**
    * @brief This is class deals with mapped memory resources.
    * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
    * @ingroup Utils
    */
  class TL_DLL TeMappedMemory {
    public :
      typedef TeSharedPtr< TeMappedMemory > pointer;
      typedef const TeSharedPtr< TeMappedMemory > const_pointer;    
      
      /**
       * @brief Default Constructor.
       */
      TeMappedMemory();          
      
      /**
       * @brief Default Destructor.
       */
      ~TeMappedMemory();
      
      /**
       * @brief Reset the current instance (free all allocated resources).
       */
      void reset();
      
      /**
       * @brief Reset the current instance to new size.
       * @note The mapped memory file will be automatically created.
       * @param size The new mapped memory size (bytes).
       * @param enabled Mapping enabled/disabled state.
       * @return true if OK. false on errors.
       */
      bool reset( unsigned long int size, bool enabled );
      
      /**
       * @brief Reset the current instance using a new created disk file.
       *
       * @note The mapped memory size will follow the disk file size.
       *
       * @param filename The mapped memory disk file name.
       * @param size The new mapped memory size (bytes).
       * @param keep_disk_file if true, the file will not be deleted.
       * @param enabled Mapping enabled/disabled state.
       * @return true if OK. false on errors.
       */
      bool reset( const std::string& filename, unsigned long int size,
        bool keep_disk_file, bool enabled );      
      
      /**
       * @brief Reset the current instance using an exising disk file.
       * @note The mapped memory size will follow the disk file size.
       * @param filename The mapped memory disk file name.
       * @param enabled Mapping enabled/disabled state.       
       * @return true if OK. false on errors.
       */
      bool reset( const std::string& filename, bool enabled );      
      
      /**
       * @brief Enable / disable the current mapping instance.
       *
       * @return true if OK. false on errors.
       */
      bool toggle( bool enabled );         
      
      /**
       * @brief Returns a pointer reference to the mapped memory.
       *
       * @note Throws an exception if no mapped memory is active.
       */
      void* getPointer() const;
      
      /**
       * @brief Returns the current mapped file name.
       * @return The current mapped file name.
       * @note Throws an exception if no mapped memory is active.
       */
      std::string getFileName() const;
      
      /**
       * @brief The current allocated mapped memory size (bytes).
       *
       * @return The current allocated mapped memory size (bytes).
       */
      unsigned long int size() const;      
      
    protected :
    
      #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
      
        /**
         * @brief Handle to file we're currently zapping
         */
        HANDLE m_hFile_;
        
        /**
         * @brief Handle to memory-mapping of that file
         */
        HANDLE m_hMapping_;
        
        /**
         * @brief Pointer to mapped to memory
         */
        LPVOID m_lpszFile_;
        
      #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  
        /**
         * @brief Handle to file we're currently zapping
         */
        int m_hFile_;
        
        /**
         * @brief Pointer to mapped to memory
         */
        void* m_lpszFile_;
        
      #else
        #error "Unsuported platform"
      #endif
      
      /**
       * @brief A flag indicating when this map is active.
       * @note Default value : false.
       */        
      bool mapping_is_active_;
      
      /**
       * @brief The disk file name.
       * @note Default value : empty string.
       *
       */        
      std::string disk_file_name_;
      
      /**
       * @brief The current mapped memory size.
       * @note Default value : 0.
       */    
      unsigned long int curr_size_;
     
      /**
       * @brief True if the mapped file must be deleted.
       * @note Default value : false.
       */    
      bool must_delete_file_;
      
      /**
       * @brief Initiates the global initial state.
       *
       */      
      void init();
      
      /**
       * @brief Create a new disk file suitable for memory mapping.
       * @param filename The file name.
       * @param size The file size.
       * @return true if OK. false on errors.
       */
      bool createNewDiskFile( const std::string& filename, 
        unsigned long int size ) const;
        
      /**
       * @brief Generate the last error string.
       * @return The last error string.
       */        
      std::string getLastErrorStr();
      
    private :
    
      /**
       * @brief Alternative Constructor.
       *
       */
      TeMappedMemory( const TeMappedMemory& ) {}; 
          
      /**
       * @brief Operator= overload.
       *
       */
      const TeMappedMemory& operator=( const TeMappedMemory& )
      {
        return *this;
      };
  };
  
/** @example TeMappedMemory_test.cpp
 *    Shows how to use this class.
 */    

#endif
