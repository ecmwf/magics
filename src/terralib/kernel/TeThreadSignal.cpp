#include "TeThreadSignal.h"

#include "TeAgnostic.h"

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  #include  <stdio.h>
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  #include <sys/time.h>
  #include <time.h>
#else
  #error "Unsuported plataform"
#endif   

TeThreadSignal::TeThreadSignal()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
   waiters_count_ = 0;
   wait_generation_count_ = 0;
   release_count_ = 0;

    // Create a manual-reset event.
    event_ = CreateEvent (NULL,  // no security
                          TRUE,  // manual-reset
                          FALSE, // non-signaled initially
                          NULL); // unnamed
    
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX

    /* Creating mutex */

    pthread_mutexattr_t access_attr;
    TEAGN_TRUE_OR_THROW( ( pthread_mutexattr_init( &access_attr ) == 0 ),
      "Unable to init mutex attributes" );

    pthread_mutexattr_settype( &access_attr, PTHREAD_MUTEX_DEFAULT );
    TEAGN_TRUE_OR_THROW( 
      ( pthread_mutex_init( &m_access_, &access_attr ) == 0 ),
      "Unable to init mutex" )

    /* Creating condition variable */
  
    TEAGN_TRUE_OR_THROW( ( pthread_cond_init( &condition_var_, 0 ) == 0 ),
      "Unable to create a condition variable" );
  
  #else
    #error "Unsuported plataform"
  #endif   
}


TeThreadSignal::~TeThreadSignal()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    CloseHandle( event_ );
    
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX

    pthread_cond_destroy( &condition_var_ );
  
    pthread_mutex_destroy( &m_access_ );
  
  #else
    #error "Unsuported plataform"
  #endif   
}


void TeThreadSignal::emit()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    waiters_count_lock_.lock();
    
    if ( waiters_count_ > 0) {  
      SetEvent ( event_);
      // Release all the threads in this generation.
      release_count_ =  waiters_count_;

      // Start a new generation.
      wait_generation_count_++;
    }
    
    waiters_count_lock_.unLock();

  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX

    TEAGN_TRUE_OR_THROW( ( pthread_cond_broadcast( &condition_var_ ) == 0 ),
      "Error emiting signal" );

  #else
    #error "Unsupported platform"
  #endif   
}


bool TeThreadSignal::wait( unsigned int waiting_time )
{
  bool return_value = true;
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    // Avoid race conditions.
    waiters_count_lock_.lock();

    // Increment count of waiters.
    waiters_count_++;

    // Store current generation in our activation record.
    int my_generation =  wait_generation_count_;

    waiters_count_lock_.unLock();

    for (;;) {
      // Wait until the event is signaled.
      if( waiting_time == 0 ) {
        if( WaitForSingleObject ( event_, INFINITE ) != WAIT_OBJECT_0 ) {
          return_value = false;
          break;
        }
      } else {
        if( WaitForSingleObject ( event_, waiting_time ) != WAIT_OBJECT_0 ) {
          return_value = false;
          break;
        }      
      }

      waiters_count_lock_.lock();
      // Exit the loop when the < event_> is signaled and
      // there are still waiting threads from this <wait_generation>
      // that haven't been released from this wait yet.
      int wait_done =  release_count_ > 0
                      &&  wait_generation_count_ != my_generation;
      waiters_count_lock_.unLock();

      if (wait_done)
        break;
    }

    waiters_count_lock_.lock();
    waiters_count_--;
    release_count_--;
    int last_waiter =  release_count_ == 0;
    waiters_count_lock_.unLock();

    if (last_waiter)
      // We're the last waiter to be notified, so reset the manual event.
      ResetEvent ( event_);

  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX

    TEAGN_TRUE_OR_THROW( ( pthread_mutex_lock( &m_access_ ) == 0 ),
      "Unable to lock mutex" );
    
    if( waiting_time == 0 ) {
      if( pthread_cond_wait( &condition_var_, &m_access_ ) != 0 ) {
        return_value = false;
      }
    } else {
      struct timeval timevalstr;

      if( gettimeofday( &timevalstr, 0 ) == 0 ) {
        struct timespec timespecstr;

        /* seconds */
        timespecstr.tv_sec = timevalstr.tv_sec + ( waiting_time / 1000 );
        /* nano-seconds */
        timespecstr.tv_nsec = ( timevalstr.tv_usec * 1000 ) +
          ( ( waiting_time % 1000 ) * 1000000 );

        if( pthread_cond_timedwait( &condition_var_, &m_access_, 
          &timespecstr ) != 0 ) {
  
          return_value = false;
        }
      } else {
        TEAGN_LOGWARN( "Unable to get the current time" );
        return_value = false;
      }
    }

    TEAGN_TRUE_OR_THROW( ( pthread_mutex_unlock( &m_access_ ) == 0 ),
      "Unable to unlock mutex" );
  
  #else
    #error "Unsuported plataform"
  #endif   
  
  return return_value;
}

