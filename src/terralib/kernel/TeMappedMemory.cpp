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

#include "TeMappedMemory.h"
#include "TeException.h"
#include "TeErrorLog.h"
#include "TeUtils.h"
#include "TeAgnostic.h"
#include "TeDefines.h"

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  #include <io.h>
  #include <fcntl.h>
  #include <sys/stat.h>  
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <fcntl.h>
  #include <unistd.h>
#else
  #error "Unsupported platform"
#endif


void TeMappedMemory::init()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    m_hFile_ = 0;
    m_hMapping_ = 0;
    m_lpszFile_ = 0;
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    m_hFile_ = 0;
    m_lpszFile_ = 0;
  #else
    #error "Unsupported platform"
  #endif
  
  mapping_is_active_ = false;
  curr_size_ = 0;
  must_delete_file_ = false;
};


TeMappedMemory::TeMappedMemory()
{
  init();
}


TeMappedMemory::~TeMappedMemory()
{
  reset();
}


void TeMappedMemory::reset()
{
  TEAGN_TRUE_OR_THROW( toggle( false ), "Error disabling mapped memory" );
  
  if( must_delete_file_ ) {
    remove( disk_file_name_.c_str() );
  }
  
  disk_file_name_.clear();
  
  init();
}


bool TeMappedMemory::reset( unsigned long int size, bool enabled )
{
  reset();
  
  std::string disk_file_name;
      
  TEAGN_TRUE_OR_RETURN( TeGetTempFileName( disk_file_name ),
    "Unable to get temporary file name" );
    
  return( reset( disk_file_name, size, false, enabled ) );
}


bool TeMappedMemory::reset( const std::string& filename, 
  unsigned long int size, bool keep_disk_file, bool enabled )
{
  reset();

  if( filename.empty() || ( size == 0 ) ) {
    TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
      "Invalid mapped file name or invalid file size" ); 
            
    return false;
  } else {
    TEAGN_TRUE_OR_RETURN( createNewDiskFile( filename, size ),
      "Unable to create memory mapped disk file" );
      
    disk_file_name_ = filename;
    must_delete_file_ = ( ! keep_disk_file );
    
    if( toggle( enabled ) ) {
      return true;
    } else {
      reset();
      return false;
    }
  }
}


bool TeMappedMemory::reset( const std::string& filename, bool enabled )
{
  reset();
  
  disk_file_name_ = filename;
  
  if( toggle( enabled ) ) {
    return true;
  } else {
    reset();
    return false;
  }  
}


bool TeMappedMemory::toggle( bool enabled )
{
  if( enabled  ) {
    if( mapping_is_active_ ) {
      return true;    
    } else {
      curr_size_ = 0;
      
      /* Activating mapping */    
      
      if( ! TeCheckFileExistence( disk_file_name_.c_str() ) ) {
        return false;
      }
      
      unsigned long int filesize = TeGetFileSize( disk_file_name_ );
  
      if( filesize > 0 ) {
        #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
      
          HANDLE m_hFile = CreateFileA(
            disk_file_name_.c_str(), GENERIC_READ | GENERIC_WRITE, 
            FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
            0, NULL);
        
          if( m_hFile == INVALID_HANDLE_VALUE ) {
            TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
              "Temporary file creation error" );        
       
            return false;
          }
  
          HANDLE m_hMapping = CreateFileMapping( m_hFile, NULL, PAGE_READWRITE,
            0, 0, NULL );
              
          if( m_hMapping == NULL ) {
            CloseHandle(m_hFile);
        
            TEAGN_LOGERR( "Mapping creation error - " +
              getLastErrorStr() );
              
            return false;        
          }
        
          LPVOID m_lpszFile = (LPVOID) MapViewOfFile( m_hMapping, 
            FILE_MAP_ALL_ACCESS, 0, 0, 0);
      
          if( m_lpszFile == 0 ) {
            CloseHandle( m_hMapping );
            CloseHandle( m_hFile );
            
            TEAGN_LOGERR( "Mapping view creation error - " +
              getLastErrorStr() );
            
            return false;        
          }        
      
          m_hFile_ = m_hFile;
          m_hMapping_ = m_hMapping;
          m_lpszFile_ = m_lpszFile;
          
        #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
        
          int m_hFile = open( disk_file_name_.c_str(), O_RDWR );

          if( m_hFile == -1 ) {
            TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
              "Temporary file creation error" );
            
            return false;         
          }
     
          void* m_lpszFile  =  mmap(0, (long)filesize, (PROT_READ | PROT_WRITE), 
            MAP_SHARED, m_hFile, 0);
      
          if( m_lpszFile == ((void*)-1) ) {
            close( m_hFile );
        
            TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
              "Mapping creation error" );
            
            return false;            
          }
        
          m_hFile_ = m_hFile;
          m_lpszFile_ = m_lpszFile;
        #else
          #error "Unsupported platform"
        #endif
    
        mapping_is_active_ = true;
        curr_size_ = filesize;
        
        return true;
      } else {     
        return false;
      }
    }
  } else {
    if( mapping_is_active_ ) {
      /* Disabling mapping */
      
      #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
        if( ! UnmapViewOfFile( m_lpszFile_ ) ) {
          throw TeException( UNKNOWN_ERROR_TYPE, "Unable to unmap disk file",
            false );
        }
          
        CloseHandle( m_hMapping_ );
        CloseHandle( m_hFile_ );
      #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
        if( munmap( m_lpszFile_ , (long)curr_size_ ) == -1) {
          throw TeException( UNKNOWN_ERROR_TYPE, "Unable to unmap disk file",
            false );
        }
            
        close(m_hFile_);
      #else
        #error "Unsupported platform"
      #endif
      
      mapping_is_active_ = false;
      curr_size_ = 0;
    }
        
    return true;
  }
}


void* TeMappedMemory::getPointer() const
{
  TEAGN_TRUE_OR_THROW( mapping_is_active_,
    "Trying to access an inactive mapping" );
    
  return m_lpszFile_;
}


std::string TeMappedMemory::getFileName() const
{
  TEAGN_TRUE_OR_THROW( mapping_is_active_,
    "Trying to access an inactive mapping" );
    
  return disk_file_name_;
}


bool TeMappedMemory::createNewDiskFile( const std::string& filename,
  unsigned long int size ) const
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    int m_hFile = open( filename.c_str(),_O_RDWR|_O_CREAT,
      _S_IREAD | _S_IWRITE);

    if( m_hFile == -1 ) {
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file creation error" );
              
      return false;         
    }
    
    off_t seek_off = ( off_t )( size - 1 );
          
    if( lseek(m_hFile, seek_off, SEEK_SET) == -1 ) {
      close( m_hFile );
          
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file seek error" );
              
      return false;           
    }
        
    unsigned char c = '\0';
    

    if( write( m_hFile, (void*)&c, sizeof( unsigned char ) ) == -1 ) {
      close( m_hFile );
          
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file write error" );
              
      return false;            
    }  
    
    close( m_hFile );  
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    int m_hFile = open( filename.c_str(),O_RDWR|O_CREAT,S_IRWXU);

    if( m_hFile == -1 ) {
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file creation error" );
              
      return false;         
    }
    
    off_t seek_off = ( off_t )( size - 1 );
          
    if( lseek(m_hFile, seek_off, SEEK_SET) == -1 ) {
      close( m_hFile );
          
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file seek error" );
              
      return false;           
    }
        
    unsigned char c = '\0';
    

    if( write( m_hFile, (void*)&c, sizeof( unsigned char ) ) == -1 ) {
      close( m_hFile );
          
      TeErrorLog::instance().insert( UNKNOWN_ERROR_TYPE,
        "Temporary file write error" );
              
      return false;            
    }  
    
    close( m_hFile );
  #else
    #error "Unsupported platform"
  #endif
  
  return true;
}


std::string TeMappedMemory::getLastErrorStr()
{
  std::string error_string;
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    LPVOID lpMsgBuf = 0;    
    DWORD dw = GetLastError();
    int written_chars_nmb = 0;
    
    written_chars_nmb = FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_FROM_SYSTEM | 
            FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,
          dw,
          0,
          (LPTSTR) &lpMsgBuf,
          1024, 
          NULL );
          
    if( written_chars_nmb > 0 ) {
      int str_size = MIN( 1024, 2 * written_chars_nmb );
      error_string = std::string( ( (char*)lpMsgBuf ), 1024 );
    }
    
    if( lpMsgBuf ) {
      LocalFree(lpMsgBuf);
    }
    
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  
  #else
    #error "Unsuported platform"
  #endif
  
  return error_string;
}


unsigned long int TeMappedMemory::size() const
{
  return curr_size_;
}
