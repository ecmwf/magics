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
/*! \file TeCommunicator.h
    \brief This file contains  class to deal with inter-class communication.
	\author Emiliano F. Castejon <castejon@dpi.inpe.br>
*/

#ifndef TECOMMUNICATOR_H
  #define TECOMMUNICATOR_H
  
  #include "TeMutex.h"
  #include "TeAgnostic.h"

  #include <list>
  #include <algorithm>
  
  /**
   * @brief A macro for use when declaring class wrapper functions that will
   * be connected to a TeCommunicator instance.
   *
   * @param function_name The function name.
   * @param param_type The function pamrameter data type.
   * @param class_name The function's host class name.
   * @note The function body must be implemented.
   */
  #define TECOMMWRAPPER( function_name, param_type, class_name ) \
    protected : void function_name( const param_type& x ); \
    public : static void function_name( const param_type& x, void* objptr ) \
    { \
      class_name * casted_ptr = ( class_name * ) objptr; \
      casted_ptr->function_name( x ); \
    };
    
  /**
   * @brief This is the template class to deal with inter-class communication.
   * @note This is a thread-safe class.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup Utils
   */
  template< typename DataType >
  class TeCommunicator 
  {
    public :
    
      /**
       * @brief The objects wrapper function pointer.
       *
       * @param DataType The data type.
       * @param void A cast pointer to the active object instance.
       */        
      typedef void (*WrapperFuncPtrT)( const DataType&, void* );
            
      /**
       * @brief Default Constructor.
       *
       * @param host_obj_ptr A pointer to the active class instance.
       * @param wrapper_func_ptr A pointer to the class wrapper static
       * function.
       * @param enabled_flag The emission/reception enabling/disabling flag.
       */
      TeCommunicator( void* host_obj_ptr = 0,
        WrapperFuncPtrT wrapper_func_ptr = 0,
        bool enabled_flag = true );

      /**
       * @brief Default Destructor
       */
      ~TeCommunicator();
      
      /**
       * @brief Connects the current instance to a class wrapper function.
       *
       * @param host_obj_ptr A pointer to the active class instance.
       * @param wrapper_func_ptr A pointer to the class wrapper static
       * function.
       */      
      void setHostObj( void* host_obj_ptr = 0, 
        WrapperFuncPtrT wrapper_func_ptr = 0 );
      
      /**
       * @brief Connects the current instance to another active instance.
       *
       * @param external The external active instance reference.
       * @return true if OK, false on errors.
       */         
      bool connect( TeCommunicator< DataType>& external );
      
      /**
       * @brief Disconnects the current instance from all connected instances.
       */         
      void disconnect();
      
      /**
       * @brief Disconnects the current instance from another instance.
       *
       * @param external The external active instance reference.
       */        
      void disconnect( TeCommunicator< DataType>& external );
      
      /**
       * @brief Send data to all connected instances.
       *
       * @param data The data to be sent.
       */        
      void send( const DataType& data ) const;
      
      /**
       * @brief Enables/disables the data emission and rececption.
       *
       * @param enabled_flag The emission/reception enabling/disabling flag.
       */        
      void toggle( bool enabled_flag );      
      
      /**
       * @brief Check if this instance emission/reception is enabled.
       * @return true if enabled, false if disabled.
       */        
      bool isEnabled();          
      
    protected :
      
      /** @typedef TeCommunicator< DataType >* ComPtrT
       *  Communicator pointer type definition.
       */
      typedef TeCommunicator< DataType >* ComPtrT;
      
      /** @typedef std::list< ComPtrT > ContainerT
       *  Connected communicators pointers container type definition.
       */ 
      typedef std::list< ComPtrT > ContainerT;
      
      /** @typedef typename ContainerT::iterator ContItT
       *  Connected communicators pointers container iterator type definition.
       */       
      typedef typename ContainerT::iterator ContItT;      
      
      /**
       * @brief Used when another active instance requests to be connected to this
       * instance.
       *
       * @param my_pointer The pointer of the active instance.
       */       
      void ConnectMe( ComPtrT my_pointer );
      
      /**
       * @brief Used when another active instance requests to be disconnected from 
       * this instance.
       *
       * @param my_pointer The pointer of the active instance.
       */       
      void DisconnectMe( ComPtrT my_pointer );
      
      /**
       * @brief Used when another active instance sends data to this instance.
       *
       * @param data The sent data.
       */        
      inline void Receive( const DataType& data ) const; 
      
    private :
    
      /**
       * @brief A flag indication for enabled/disabled.
       */     
      bool emission_reception_enabled_;
    
      /**
       * @brief A pointer to the host object wrapper function.
       */      
      WrapperFuncPtrT wrapper_func_ptr_;
      
      /**
       * @brief A pointer to the host object active instance.
       */
      void* host_obj_ptr_;
      
      /**
       * @brief A list of pointers to the connected active instances.
       */
      mutable ContainerT connections_;
      
      /** @brief Thread lock instance. */
      mutable TeMutex lock_instance_;      
      
      /**
       * @brief Copy constructor not allowed.
       */
      TeCommunicator( const TeCommunicator< DataType >& ) {};
      
      /**
       * @brief Copy operation not allowed.
       */
      const TeCommunicator< DataType >& operator=( 
        const TeCommunicator< DataType >& ) {};
      
  };
  

  template< typename DataType >  
  TeCommunicator< DataType >::TeCommunicator( void* host_obj_ptr,
    WrapperFuncPtrT wrapper_func_ptr, bool enabled_flag )
  {
    wrapper_func_ptr_ = 0;
    host_obj_ptr_ = 0;
    emission_reception_enabled_ = enabled_flag;
    
    setHostObj( host_obj_ptr, wrapper_func_ptr );
  }
  
  
  template< typename DataType >  
  TeCommunicator< DataType >::~TeCommunicator()
  {
    disconnect();
  }
  
  
  template< typename DataType >  
  void TeCommunicator< DataType >::setHostObj( void* host_obj_ptr,
    WrapperFuncPtrT wrapper_func_ptr )
  {
    lock_instance_.lock();
    
//   TEAGN_TRUE_OR_THROW( (wrapper_func_ptr != 0), 
//      ( ( wrapper_func_ptr == 0 ) && ( host_obj_ptr == 0 ) ) || 
//      ( ( wrapper_func_ptr != 0 ) && ( host_obj_ptr != 0 ) ) ), 
//      "Invalid receiver_func_ptr/host_obj_ptr parameters" );
      
    wrapper_func_ptr_ = wrapper_func_ptr;
    host_obj_ptr_ = host_obj_ptr;
    
    lock_instance_.unLock();
  }
  
  
  template< typename DataType >
  bool TeCommunicator< DataType >::connect( 
    TeCommunicator< DataType>& external )
  {
    lock_instance_.lock();
    
//    if( ( wrapper_func_ptr_ == 0 ) || ( host_obj_ptr_ == 0 ) ) {
	  if( wrapper_func_ptr_ == 0 ) {
      lock_instance_.unLock();
      
      TEAGN_LOGERR( "Connecting a inactive communicator instance" );
      
      return false;
    }
    
    ContItT it = find( connections_.begin(), connections_.end(),
      &external );

    if( it == connections_.end() ) {
      connections_.push_back( &external );
      external.ConnectMe( this );
    }
    
    lock_instance_.unLock();
    
    return true;
  }
  

  template< typename DataType >
  void TeCommunicator< DataType >::disconnect()
  {
    lock_instance_.lock();
    
    ContItT it = connections_.begin();
    ContItT it_end = connections_.end();

    while( it != it_end ) {
      (*it)->DisconnectMe( this );
      
      ++it;
    }
    
    connections_.clear();
    
    lock_instance_.unLock();
  }
  
  
  template< typename DataType >
  void TeCommunicator< DataType >::disconnect( 
    TeCommunicator< DataType>& external )
  {
    lock_instance_.lock();
    
    ContItT it = find( connections_.begin(), connections_.end(),
      &external );
      
    if( it != connections_.end() ) {
      (*it)->DisconnectMe( this );
      connections_.erase( it );
    }
    
    lock_instance_.unLock();
  }

  
  template< typename DataType >
  void TeCommunicator< DataType >::ConnectMe( 
    ComPtrT my_pointer )
  {
    lock_instance_.lock();
    
    ContItT it = find( connections_.begin(), connections_.end(),
      my_pointer );
      
    if( it == connections_.end() ) {
      connections_.push_back( my_pointer );
    }
    
    lock_instance_.unLock();
  }

  
  template< typename DataType >
  void TeCommunicator< DataType >::DisconnectMe( 
    ComPtrT my_pointer )
  {
    lock_instance_.lock();
    
    ContItT it = find( connections_.begin(), connections_.end(),
      my_pointer );
      
    if( it != connections_.end() ) {
      connections_.erase( it );
    }
    
    lock_instance_.unLock();
  }
  
  
  template< typename DataType >
  void  TeCommunicator< DataType >::send( const DataType& data ) const
  {
    lock_instance_.lock();
    
    if( emission_reception_enabled_ ) {
      ContItT it = connections_.begin();
      ContItT it_end = connections_.end();
      
      while( it != it_end ) {
        (*it)->Receive( data );
        
        ++it;  
      }
    }
    
    lock_instance_.unLock();
  }
  
  
  template< typename DataType >
  void  TeCommunicator< DataType >::toggle( bool enabled_flag )
  {
    lock_instance_.lock();
    
    emission_reception_enabled_ = enabled_flag;
    
    lock_instance_.unLock();
  }
  
  
  template< typename DataType >
  bool TeCommunicator< DataType >::isEnabled()
  {
    return emission_reception_enabled_;
  }  

  
  template< typename DataType >
  inline void  TeCommunicator< DataType >::Receive( 
    const DataType& data ) const
  {
    lock_instance_.lock();
    
    if( emission_reception_enabled_ && ( wrapper_func_ptr_ != 0 ) ) {
      wrapper_func_ptr_( data, host_obj_ptr_ );
    }
    
    lock_instance_.unLock();
  }
  
/** @example TeCommunicator_test.cpp
 *    Shows how to use this class.
 */  
    
#endif

