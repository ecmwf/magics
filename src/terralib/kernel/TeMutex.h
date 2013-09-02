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
/*! \file TeMutex.h
  \brief This file contains definitions about a class to deal with critical 
  region locking.
*/


#ifndef TEMUTEX_H
  #define TEMUTEX_H
  
  #include "TeAgnostic.h"
  #include "TeDefines.h"
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    #include <windows.h>
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    #include <pthread.h>
    #include <errno.h>
  #else
    #error "ERROR: Unsupported platform"
  #endif  
 
  /**
   * @brief A class to deal with critical region locking.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   */
  class TL_DLL TeMutex
  {
    public :
    
      /**
       * @brief Default constructor.
       */    
      TeMutex();

      /**
       * @brief Default destructor.
       */       
      ~TeMutex();
      
      /**
       * @brief Lock the current object instance.
       * @note If section is already busy then the current thread will be 
       * blocked until it's ready again.
       */       
      inline void lock()
      {
        #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
        
          DWORD return_value = 0;
          return_value = ::WaitForSingleObject( m_access_, INFINITE );
          TEAGN_DEBUG_CONDITION( ( ( return_value == WAIT_ABANDONED ) ||
            ( return_value == WAIT_OBJECT_0 ) ),
            "Unable to get mutex lock" );
        
        #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
        
          pthread_mutex_lock( &m_access_ );
        
        #else
          #error "Unsuported plataform"
        #endif  
      };      
      
      /**
       * @brief Try to lock the current object instance.
       * @return true if OK, false if unable to lock.
       * @note If section is busy, this method will return false 
       * without blocking the current thread.
       */       
      inline bool tryLock()
      {
        #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
        
          DWORD return_value = ::WaitForSingleObject( m_access_, 
            10 );

          if( ( return_value == WAIT_OBJECT_0 ) ||
            ( return_value == WAIT_ABANDONED ) ) {

            return true;
          } else {
            return false;
          }
        
        #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
        
          if( pthread_mutex_trylock( &m_access_ ) == EBUSY ) {
            return false;
          } else {
            return true;
          }
        
        #else
          #error "Unsuported plataform"
        #endif  
      };      
      
      /**
       * @brief Unlock the current object instance.
       */       
      inline void unLock()
      {
        #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
        
          ::ReleaseMutex( m_access_ );
        
        #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
        
          pthread_mutex_unlock( &m_access_ );
        
        #else
          #error "Unsuported plataform"
        #endif 
      };      
            
    protected :
   
      #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    
        /**
        * @brief The mutex instance.
        */      
        HANDLE m_access_;
        
        /**
        * @brief The mutex instance attributes.
        */           
        SECURITY_ATTRIBUTES m_sa_;
      
      #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
      
        /**
        * @brief The mutex instance.
        */        
        pthread_mutex_t m_access_;
      
      #else
        #error "Unsuported plataform"
      #endif   
   
    private :
    
      /**
       * Alternative constructor.
       */    
      TeMutex(  const TeMutex& ) {};
    
   
      /**
       * operator= overload.
       * @return A const reference to the current instance.
       */      
      const TeMutex& operator=( const TeMutex& ) { return *this; };
 
  };

#endif
