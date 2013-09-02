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

/*! \file TeMultiContainer.h
   \brief A container class to store multiple types os object copies.
*/


#ifndef TEMULTICONTAINER_H
  #define TEMULTICONTAINER_H

  #include "TeSharedPtr.h"
  #include "TeMutex.h"
  #include "TeAgnostic.h"
  
  #include <time.h>
  
  #include <vector>
  #include <typeinfo>

  /**
    * @brief Multi-container node interface.
    * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
    * @ingroup Utils
    */    
  template< typename TeMultiContainerKeyT >
  class TeMCNodeInterface {
    public :
    
      /**
        * @brief Default Constructor.
        */        
      TeMCNodeInterface() {};
      
      /**
        * @brief Default Destructor.
        */        
      virtual ~TeMCNodeInterface() {};
      
      /**
        * @brief Copies the the current node instance by calling
        * the operator= from the contained object instance.
        * @return A pointer to the new node copy.
        */            
      virtual TeMCNodeInterface< TeMultiContainerKeyT >* clone() 
        const = 0;
        
      /**
        * @brief Get the internal object type info.
        * @return The internal object type info.
        */        
      virtual const std::string& getObjTypeInfo() const = 0;
      
    private :
    
      /**
        * @brief Alternative constructor.
        */        
      TeMCNodeInterface( 
        const TeMCNodeInterface< TeMultiContainerKeyT >& ) {};    
    
      /**
      * @brief operator= overload.
      *
      * @param ext_instance External instance reference.
      * @return The external instance reference.
      */
      const TeMCNodeInterface< TeMultiContainerKeyT >& operator=( 
        const TeMCNodeInterface< TeMultiContainerKeyT >& ) {};        
  };


  /**
    * @brief Multi-container node class.
    * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
    * @ingroup Utils
    */       
  template< typename ObjectT, typename TeMultiContainerKeyT >
  class TeMCNode : public TeMCNodeInterface< TeMultiContainerKeyT > {
    public :
    
      /**
        * @brief Default Constructor.
        */        
      TeMCNode() 
      {
        obj_ptr_ = 0;
      };
      
      /**
        * @brief Default Destructor.
        */        
      ~TeMCNode()
      {
        if( obj_ptr_ ) {
          delete obj_ptr_;
        }
      };
      
      /**
        * @brief Copies the the current node instance by calling
        * the operator= from the contained object instance.
        * @return A pointer to the new node copy.
        */              
      TeMCNodeInterface< TeMultiContainerKeyT >* clone() const
      {
        TeMCNode< ObjectT, TeMultiContainerKeyT >* new_node_ptr =
          new TeMCNode< ObjectT, TeMultiContainerKeyT >;
          
        if( obj_ptr_ ) {
          new_node_ptr->obj_ptr_ = new ObjectT;
          ( *( new_node_ptr->obj_ptr_ ) ) = ( *obj_ptr_ );
          
          new_node_ptr->obj_type_str_ = obj_type_str_;
        }
        
        return new_node_ptr;
      };
      
      /**
        * @brief Set the internal object pointer.
        * @param ptr Object pointer.
        */        
      void setObjPtr( ObjectT* ptr ) 
      { 
        TEAGN_DEBUG_CONDITION( ptr, "Invalid pointer" )
        
        if( obj_ptr_ ) {
          delete obj_ptr_;
        }      
      
        obj_ptr_ = ptr;
        obj_type_str_ = std::string( typeid( *ptr ).name() );
      };
      
      /**
        * @brief Get the internal object pointer.
        * @return The object pointer.
        */        
      ObjectT* getObjPtr() const
      { 
        return obj_ptr_; 
      };
      
      /**
        * @brief Get the internal object type info.
        * @return The internal object type info.
        */        
      const std::string& getObjTypeInfo() const
      { 
        return obj_type_str_; 
      };      
      
    protected :
      
      /**
        * @brief The internal object pointer.
        */
      ObjectT* obj_ptr_;
      
      /**
        * @brief The internal object type.
        */
      std::string obj_type_str_;      
      
  }; 

 /**
  * @brief A container class to store multiple types os object copies.
  * @author Emiliano F. Castejon <castejon@dpi.inpe.br>
  * @note This is a thread-safe class.
  * @note Do not use this class with polymorphic types !!
  * @ingroup Utils
  */
  template< typename TeMultiContainerKeyT >
  class TeMultiContainer {
  
    public :
    
      /** @typedef TeSharedPtr< TeMultiContainer< TeMultiContainerKeyT > > pointer 
          Type definition for a instance pointer */
      typedef TeSharedPtr< TeMultiContainer< TeMultiContainerKeyT > > pointer;
      
      /** @typedef const TeSharedPtr< TeMultiContainer< TeMultiContainerKeyT > > const_pointer 
          Type definition for a const instance pointer */
      typedef const TeSharedPtr< TeMultiContainer< TeMultiContainerKeyT > > const_pointer;
      
      /**
       * @brief Default Constructor
       */
      TeMultiContainer();
      
      /**
       * @brief Alternative Constructor
       * @param external External reference.
       */
      TeMultiContainer( 
        const TeMultiContainer< TeMultiContainerKeyT >& external );      

      /**
       * @brief  Default Destructor
       */
      ~TeMultiContainer();
                           
      /**
       * @brief Operator == overload.
       *
       * @param ext_instance External instance reference.
       * @return true if this instance have the same internal
       * update time that the external instance.
       */
      bool operator==( 
        const TeMultiContainer< TeMultiContainerKeyT >& ext_instance ) const;
      
      /**
       * @brief Operator != overload.
       *
       * @param ext_instance External instance reference.
       * @return true if this instance don't have the same internal
       * update time that the external instance.
       */
      bool operator!=( 
        const TeMultiContainer< TeMultiContainerKeyT >& ext_instance ) const;
      
      /**
       * @brief operator= overload.
       *
       * @param ext_instance External instance reference.
       * @return The external instance reference.
       */
      const TeMultiContainer< TeMultiContainerKeyT >& operator=( 
        const TeMultiContainer< TeMultiContainerKeyT >& ext_instance );
      
      /**
       * @brief Clear all contents.
       *
       */
      void clear();
                         
      /**
       * @brief Store a object copy.
       *
       * @param obj_key Object key.
       * @param obj_reference Object instance.
       */                         
      template< typename ObjectT >
      void store( const TeMultiContainerKeyT& obj_key,
                         const ObjectT& obj_reference );
      
      /**
       * @brief Retrive a copy of a stored object.
       *
       * @param obj_key Object key.
       * @param obj_reference Object instance.
       * @return true if OK, false if the parameter was no found or error.
       */      
      template< typename ObjectT >
      bool retrive( const TeMultiContainerKeyT& obj_key,
        ObjectT& obj_reference ) const;
        
      /**
       * @brief Retrive copies of all stored objects of a defined type.
       *
       * @param objs_vector A vector with copies of all found objects.
       * @note An empty vector will be returned if no object was found.
       */      
      template< typename ObjectT >
      void multiRetrive( std::vector< std::pair< TeMultiContainerKeyT, 
        ObjectT > >& objs_vector ) const;        
     
      /** 
       * @brief Checks if a object is stored.
       * 
       * @param obj_key Object key.
       * @note The object type must be specified.
       * @return true if OK, false if the object is not stored.
       */
      template< typename ObjectT >
      bool isStored( const TeMultiContainerKeyT& obj_key ) const;
      
      /** 
       * @brief Remove a stored object.
       * 
       * @param obj_key Object key.
       */
      void remove( const TeMultiContainerKeyT& obj_key );      
      
    protected :

      /** @brief Internal container type definition. */
      typedef typename std::map< TeMultiContainerKeyT,
        TeMCNodeInterface< TeMultiContainerKeyT >* > IntContainerT;
      
      /** @brief The nodes container instance. */
      IntContainerT container_instance_;    
      
      /**
       * @brief The last update time.
       */
      time_t last_up_time_;
      
      /**
       * @brief This instance locking mutex.
       */    
      mutable TeMutex lock_instance_;      

      /**
       * @brief Updates the internal last update time.
       *
       * @note Needed by comparison between two multi containers.
       */
      void update_time();            

};


template< typename TeMultiContainerKeyT >
TeMultiContainer< TeMultiContainerKeyT >::TeMultiContainer()
{
  last_up_time_ = 0;
}


template< typename TeMultiContainerKeyT >
TeMultiContainer< TeMultiContainerKeyT >::TeMultiContainer( 
  const TeMultiContainer< TeMultiContainerKeyT >& external )
{
  last_up_time_ = 0;
  
  operator=( external );
}


template< typename TeMultiContainerKeyT >
TeMultiContainer< TeMultiContainerKeyT >::~TeMultiContainer()
{
  clear();
}


template< typename TeMultiContainerKeyT >
void TeMultiContainer< TeMultiContainerKeyT >::clear()
{
  lock_instance_.lock();
  
  typename IntContainerT::iterator it = container_instance_.begin();
  typename IntContainerT::iterator it_end = container_instance_.end();
  
  while( it != it_end ) {
    delete (it->second);
    
    ++it;
  }
  
  container_instance_.clear();
  
  lock_instance_.unLock();
}


template< typename TeMultiContainerKeyT >
bool TeMultiContainer< TeMultiContainerKeyT >::operator==( 
  const TeMultiContainer< TeMultiContainerKeyT >& ext_instance ) const
{
  if( last_up_time_ == ext_instance.last_up_time_ ) {
    return true;
  } else {
    return false;
  }
}


template< typename TeMultiContainerKeyT >
bool TeMultiContainer< TeMultiContainerKeyT >::operator!=( 
  const TeMultiContainer< TeMultiContainerKeyT >& ext_instance ) 
  const
{
  if( last_up_time_ == ext_instance.last_up_time_ ) {
    return false;
  } else {
    return true;
  }
}


template< typename TeMultiContainerKeyT >
const TeMultiContainer< TeMultiContainerKeyT >& 
TeMultiContainer< TeMultiContainerKeyT >::operator=( 
  const TeMultiContainer< TeMultiContainerKeyT >& ext_instance )
{
  if( ( &ext_instance ) != this ) {
    lock_instance_.lock();
    
    /* Clearing the current objects */
    
    typename IntContainerT::iterator my_container_it = 
      container_instance_.begin();
    typename IntContainerT::iterator my_container_it_end = 
      container_instance_.end();
    
    while( my_container_it != my_container_it_end ) {
      delete (my_container_it->second);
      
      ++my_container_it;
    }    
    
    container_instance_.clear();
    
    /* Cloning external objects */
    
    ext_instance.lock_instance_.lock();
  
    typename IntContainerT::const_iterator container_it = 
      ext_instance.container_instance_.begin();
    typename IntContainerT::const_iterator container_it_end = 
      ext_instance.container_instance_.end();
          
    while( container_it != container_it_end ) {
      container_instance_[ container_it->first ] = 
        container_it->second->clone();
          
      ++container_it;
    }
  
    last_up_time_ = ext_instance.last_up_time_;
  
    ext_instance.lock_instance_.unLock();
    
    lock_instance_.unLock();
  }

  return *this;
}


template< typename TeMultiContainerKeyT >
void TeMultiContainer< TeMultiContainerKeyT >::update_time()
{
  last_up_time_ = time( 0 );
}


template< typename TeMultiContainerKeyT >
template< typename ObjectT >
void TeMultiContainer< TeMultiContainerKeyT >::store( 
  const TeMultiContainerKeyT& obj_key, const ObjectT& obj_reference )
{
  lock_instance_.lock();

  /* Creating a new node */
  
  ObjectT* newobjptr = new ObjectT;
  ( *newobjptr ) = obj_reference;
  
  TeMCNode< ObjectT, TeMultiContainerKeyT >* newnodeptr =
    new TeMCNode< ObjectT, TeMultiContainerKeyT >;  
  newnodeptr->setObjPtr( newobjptr );
  
  typename IntContainerT::iterator container_it = 
    container_instance_.find( obj_key );
    
  /* If a old node with the same key exists, it will be deleted */
    
  if( container_it == container_instance_.end() ) {
    container_instance_[ obj_key ] = newnodeptr;
  } else {
    delete (container_it->second);
    
    container_it->second = newnodeptr;
  }
      
  update_time();
  
  lock_instance_.unLock();
}


template< typename TeMultiContainerKeyT >
template< typename ObjectT >
bool TeMultiContainer< TeMultiContainerKeyT >::retrive(
  const TeMultiContainerKeyT& obj_key, ObjectT& obj_reference ) const
{
  lock_instance_.lock();
  
  typename IntContainerT::const_iterator container_it = 
    container_instance_.find( obj_key );
    
  if( container_it == container_instance_.end() ) {
    lock_instance_.unLock();
    
    return false;
  } else {
    if( typeid( ObjectT ).name() == 
      container_it->second->getObjTypeInfo() ) {
      
      obj_reference = 
        ( *( ( (TeMCNode< ObjectT, TeMultiContainerKeyT >* )
        container_it->second )->getObjPtr() ) );
    
      lock_instance_.unLock();
    
      return true;
    } else {
      lock_instance_.unLock();
    
      return false;
    }
  }
}


template< typename TeMultiContainerKeyT >
template< typename ObjectT >
void TeMultiContainer< TeMultiContainerKeyT >::multiRetrive(
  std::vector< std::pair< TeMultiContainerKeyT, 
  ObjectT > >& objs_vector ) const
{
  objs_vector.clear();
  
  lock_instance_.lock();
  
  typename IntContainerT::const_iterator container_it = 
    container_instance_.begin();
  typename IntContainerT::const_iterator container_it_end = 
    container_instance_.end();
  
  std::pair< TeMultiContainerKeyT, ObjectT > temp_pair;
  
  while( container_it != container_it_end ) {
    if( typeid( ObjectT ).name() == 
      container_it->second->getObjTypeInfo() ) {  
      
      temp_pair.first = container_it->first;
      temp_pair.second = 
        ( *( ( ( TeMCNode< ObjectT, TeMultiContainerKeyT >* )
        container_it->second )->getObjPtr() ) );
    
      objs_vector.push_back( temp_pair );      
    }  
  
    ++container_it;
  }
  
  lock_instance_.unLock();
}


template< typename TeMultiContainerKeyT >
template< typename ObjectT >
bool TeMultiContainer< TeMultiContainerKeyT >::isStored( 
  const TeMultiContainerKeyT& obj_key ) const
{
  lock_instance_.lock();
  
  typename IntContainerT::const_iterator container_it = 
    container_instance_.find( obj_key );
    
  if( container_it == container_instance_.end() ) {
    lock_instance_.unLock();
    
    return false;
  } else {
    if( typeid( ObjectT ).name() == 
      container_it->second->getObjTypeInfo() ) {
    
      lock_instance_.unLock();
    
      return true;
    } else {
      lock_instance_.unLock();
    
      return false;
    }
  }
}


template< typename TeMultiContainerKeyT >
void TeMultiContainer< TeMultiContainerKeyT >::remove( 
  const TeMultiContainerKeyT& obj_key )
{
  lock_instance_.lock();
  
  typename IntContainerT::iterator container_it = 
    container_instance_.find( obj_key );
    
  /* If a old node with the same key exists, it will be deleted */
    
  if( container_it != container_instance_.end() ) {
    delete (container_it->second);
    
    container_instance_.erase( container_it );
  }
      
  update_time();
  
  lock_instance_.unLock();
}

/** @example TeMultiContainer_test.cpp
 *    Shows how to use this class.
 */  

#endif

