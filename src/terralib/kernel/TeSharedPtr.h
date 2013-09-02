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
/*! \file TeSharedPtr.h
    \brief This file contains a class to deal with automatic dynamic memory allocation / desallocationn
*/
#ifndef TESHAREDPTR_H
  #define TESHAREDPTR_H

  #include "TeMutex.h"
  #include "TeAgnostic.h"

  /**
   * @brief This is the class to deal with automatic dynamic memory
   * allocation / desallocation.
   * @note This is a thread-safe class.
   * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
   * @ingroup Utils
   */
  template< class T >
  class TeSharedPtr {
    public :
      /**
       * @brief Default Constructor( Shared ).
       *
       * @param pointer A pointer the the active object.
       */
      explicit TeSharedPtr( T* pointer = 0 );
        
      /**
       * @brief Alternative Constructor.
       *
       * @param external External object reference.
       * @param not_shared_flag A flag indication for a static
       * assignment ( the pointed object will not be deleted at the
       * this object destruction ).       
       */
      TeSharedPtr( const TeSharedPtr< T >& external );        
        
      /**
       * @brief Alternative Constructor.
       *
       * @param pointer A pointer the the active object.
       * @param not_shared_flag A flag indication for a static
       * assignment ( the pointed object will not be deleted at the
       * this object destruction ).
       */
      explicit TeSharedPtr( T* pointer,  bool not_shared_flag );        

      /**
       * @brief Default Destructor
       */
      ~TeSharedPtr();

      /**
       * @brief Verifies if the current pointer points to an active object.
       *
       * @return true if active, false if not.
       */
      inline bool isActive() const;
      
      /**
       * @brief Verifies if the current pointer is shared.
       *
       * @return true if shared, false if not.
       */
      inline bool isShared() const;      

      /**
       * @brief Reset the active instance the the new pointer.
       *
       * @param pointer A pointer the the active object.
       * @param not_shared_flag A flag indication for a static
       * assignment ( the pointed object will not be deleted at the
       * this object destruction ).
       */
      void reset( T* pointer = 0, bool not_shared_flag = false );

      /**
       * @brief Operator = overload.
       *
       * @param external External instance reference.
       * @return A reference to the current pointer.
       */
      TeSharedPtr< T >& operator=( const TeSharedPtr< T >& external );

      /**
       * @brief Operator * overload.
       *
       * @return The internal instance reference.
       */
      inline T& operator*() const;

      /**
       * @brief Operator -> overload.
       *
       * @return The internal instance pointer.
       */
      inline T* operator->() const;

      /**
       * @brief Operator == overload.
       *
       * @param external The external instance reference.
       * @return true if the current pointer and the external pointer
       * are pointing to the same object.
       */
      inline bool operator==( const TeSharedPtr< T >& external ) const;
      
      /**
       * @brief Operator< overload.
       *
       * @param external The external instance reference.
       * @return true if the current pointer pointed address has a lower value 
       * then the external shared pointer pointed address.
       */
      inline bool operator<( const TeSharedPtr< T >& external ) const;      

      
      /**
       * @brief Operator> overload.
       *
       * @param external The external instance reference.
       * @return true if the current pointer pointed address has a lower value 
       * then the external shared pointer pointed address.
       */
      inline bool operator>( const TeSharedPtr< T >& external ) const;      

            
      /**
       * @brief Operator != overload.
       *
       * @param external The external instance reference.
       * @return true if the current pointer and the external pointer
       * are not pointing to the same object.
       */
      inline bool operator!=( const TeSharedPtr< T >& external ) const;

      /**
       * @brief A Naked Pointer to the encapsulated object.
       *
       * @return The internal instance pointer.
       */
      inline T* nakedPointer() const;
      
      /**
       * @brief Builds a non-shared pointer copy from the current instance.
       *
       * @return The non-shared pointer copy.
       */
      TeSharedPtr< T > getNonSharedCopy() const;      
      
      /**
       * @brief The number of current references to the pointed object.
       *
       * @return The number of current references to the pointed object.
       */
      unsigned long int getRefCount() const;            

    protected :
    
      /**
       * @brief This instance locking mutex.
       */    
      mutable TeMutex this_lock_instance_;
      
      /**
       * @brief A pointer to the shared counter locking mutex.
       */    
      mutable TeMutex* counter_lock_instance_ptr_;      

      /**
       * @brief A pointer to the current number of active users of this pointer.
       */
      mutable unsigned long int* counter_;

      /**
       * @brief A pointer to the current object encapsulated by this
       * shared pointer.
       */
      mutable T* reference_;

      /**
       * @brief A flag indicating if this shared pointer was created by a
       * dynamic assignment ( false value ) or by a static
       * assignment( true value - the encapsulated object will not
       * be destroyed at the end.
       */
      mutable bool not_shared_flag_;
      
      /**
       * @brief Set all internal variables to default values.
       *
       */
      void init();
  };

  
  template< class T >
  TeSharedPtr< T >::TeSharedPtr( T* pointer )
  {
    init();
    reset( pointer, not_shared_flag_ );
  }
 
   
  template< class T >
  TeSharedPtr< T >::TeSharedPtr( const TeSharedPtr< T >& external )
  {
    init();
    operator=( external );
  } 

    
  template< class T >
  TeSharedPtr< T >::TeSharedPtr( T* pointer, bool not_shared_flag )
  {
    init();
    reset( pointer, not_shared_flag );
  }  


  template< class T >
    TeSharedPtr< T >::~TeSharedPtr()
  {
    reset( 0, 0 );
  }


  template< class T >
  inline bool TeSharedPtr< T >::isActive() const
  {
    return ( ( reference_ == 0 ) ? false : true );
  }
  
  
  template< class T >
  inline bool TeSharedPtr< T >::isShared() const
  {
    return ( ! not_shared_flag_ );
  }  


  template< class T >
  void TeSharedPtr< T >::reset( T* pointer, bool not_shared_flag )
  {
    this_lock_instance_.lock();
    
    /* updating the shared objects */
    
    if( ( reference_ != 0 ) && ( ! not_shared_flag_ ) ) {
      counter_lock_instance_ptr_->lock();
    
      --(*counter_);
      
      /* Cleanning the shared pointed objects if necessary */

      if( (*counter_) == 0 ) {
        delete counter_;
        delete reference_;
        delete counter_lock_instance_ptr_;
      } else {
        counter_lock_instance_ptr_->unLock();
      }
    }
     
    init();
    
    /* Acquiring the pointed object */

    if( pointer != 0 ) {
      if( ! not_shared_flag ) {
        counter_ = new unsigned long int;
        (*counter_) = 1;
        counter_lock_instance_ptr_ = new TeMutex;
      }

      reference_ = pointer;
      not_shared_flag_ = not_shared_flag;
    }
    
    this_lock_instance_.unLock();
  }


  template< class T >
  TeSharedPtr< T >& TeSharedPtr< T >::operator=(
    const TeSharedPtr< T >& external )
  {
    if( ( &external ) != this ) {
      reset();
  
      this_lock_instance_.lock();
      
      if( (&external) != 0 ) {
        external.this_lock_instance_.lock();
      
        if( external.isActive() ) {
          reference_ = external.reference_;
        
          if( ( ! not_shared_flag_ ) && ( ! external.not_shared_flag_ ) ) {
            
            counter_ = external.counter_;
            (*counter_) = (*counter_) + 1;
            
            counter_lock_instance_ptr_ = external.counter_lock_instance_ptr_;
          } else {
            not_shared_flag_ = true;
          }
        }
        
        external.this_lock_instance_.unLock();
      }
      
      this_lock_instance_.unLock();
    }

    return *this;
  }


  template< class T >
  inline T& TeSharedPtr< T >::operator*() const
  {
    TEAGN_DEBUG_CONDITION( isActive(), 
      "Trying to use an inactive shared pointer instance" );

    return *reference_;
  }


  template< class T >
  inline T* TeSharedPtr< T >::operator->() const
  {
    TEAGN_DEBUG_CONDITION( isActive(), 
      "Trying to use an inactive shared pointer instance" );
    
    return reference_;
  }


  template< class T >
  inline bool TeSharedPtr< T >::operator==(
    const TeSharedPtr< T >& external ) const
  {
    return ( ( reference_ == external.reference_ ) ? true : false );
  }
  
  
  template< class T >
  inline bool TeSharedPtr< T >::operator<(
    const TeSharedPtr< T >& external ) const
  {
    return ( ( reference_ < external.reference_ ) ? true : false );
  }  


  template< class T >
  inline bool TeSharedPtr< T >::operator>(
    const TeSharedPtr< T >& external ) const
  {
    return ( ( reference_ > external.reference_ ) ? true : false );
  }  
    

  template< class T >
  inline bool TeSharedPtr< T >::operator!=(
    const TeSharedPtr< T >& external ) const
  {
    return ( ( reference_ == external.reference_ ) ? false : true );
  }


  template< class T >
  inline T* TeSharedPtr< T >::nakedPointer() const
  {
    return reference_;
  }
  
  template< class T >
  TeSharedPtr< T > TeSharedPtr< T >::getNonSharedCopy() const
  {
    TeSharedPtr< T > outptr;
    outptr.reset( reference_, true );
    return outptr;
  }
  
  template< class T >
  unsigned long int TeSharedPtr< T >::getRefCount() const
  {
    if( reference_ ) {
      if( counter_) {
        return (*counter_);
      } else {
        return 1;
      }
    } else {
      return 0;
    }
  }  
  
  template< class T >
  void TeSharedPtr< T >::init()
  {
    counter_ = 0;
    reference_ = 0;
    counter_lock_instance_ptr_ = 0;
    not_shared_flag_ = false;  
  }  

/** @example TeSharedPtr_test.cpp
 *    Shows how to use this class.
 */  
  
#endif

