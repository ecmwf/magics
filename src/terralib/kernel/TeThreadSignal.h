/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeThreadSignal.h
  \brief This file contains definitions about a class to deal thread 
  signals.
*/


#ifndef TETHREADSIGNAL_H
  #define TETHREADSIGNAL_H
  
  #include "TeDefines.h"
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    #include <windows.h>
    #include "TeMutex.h"
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    #include <pthread.h>
    #include  <errno.h>
  #else
    #error "ERROR: Unsupported platform"
  #endif  
 
  /**
   * @brief A class to deal with thread signals.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   */
  class TL_DLL TeThreadSignal
  {
    public :
    
      /**
       * @brief Default constructor.
       */    
      TeThreadSignal();

      /**
       * @brief Default destructor.
       */       
      ~TeThreadSignal();
      
      /**
       * @brief Emit a broadcast signal unblocking all waiting threads.
       */       
      void emit();      
      
      /**
       * @brief Block the current thread waiting for a signal.
       * @param waiting_time The maximim waiting time in milliseconds
       * ( 0 == INFINITE ).
       * @return true if a signal was received, false if the waiting
       * time has finished or an error occurred.
       */       
      bool wait( unsigned int waiting_time = 0 );      
      
    protected :
   
      #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    
        /**
        * @brief Count of the number of waiters.
        */      
        int waiters_count_;

        /**
        * @brief Serialize access to waiters_count_.
        */  
        TeMutex waiters_count_lock_;
        
        /**
        * @brief Number of threads to release via a 
        * signal broadcast.
        */ 
        int release_count_;

        /**
        * @brief Keeps track of the current "generation" so that we 
        * don't allow one thread to steal all the "releases" from the 
        * broadcast.
        */ 
        int wait_generation_count_;

        /**
        * @brief A manual-reset event that's used to block and release 
        * waiting threads.
        */ 
        HANDLE event_;
      
      #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX

        /**
        * @brief The mutex instance.
        */        
        pthread_mutex_t m_access_;
        
        /**
        * @brief The condition var instance.
        */        
        pthread_cond_t condition_var_;

      
      #else
        #error "Unsuported plataform"
      #endif   
   
    private :
    
      /**
       * @brief Alternative constructor.
       */    
      TeThreadSignal(  const TeThreadSignal& ) {};
   
      /**
       * @brief operator= overload.
       * @return A const reference to the current instance.
       */      
      const TeThreadSignal& operator=( const TeThreadSignal& ) { return *this; };
 
  };

#endif
