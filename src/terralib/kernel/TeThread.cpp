#include "TeThread.h"
#include "TeAgnostic.h"

TeThread::TeThread()
{
	init();
}

TeThread::~TeThread()
{
	TEAGN_TRUE_OR_THROW( ( threadStatus_ == TeThreadStopped ),
		                 "Trying to delete a running thread handler instance" )
  freeResources();
}

bool TeThread::start()
{
  if( threadStatus_ != TeThreadStopped )
	{
        return false;
  }
	else
	{
    freeResources();
    
#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
		threadId_ = 0;
        threadHandler_ = 0;
		threadHandler_ = CreateThread(NULL, 0, 
      (LPTHREAD_START_ROUTINE)TeThread::startThread,
      (LPVOID)(this), 0, (LPDWORD) &threadId_);

    TEAGN_TRUE_OR_RETURN( ( threadHandler_ != 0 ),
      "Thread creation failed" )
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
		TEAGN_TRUE_OR_RETURN( ( pthread_attr_init(&threadAttr_) == 0 ),
      "Unable to init thread attributes" )
		TEAGN_TRUE_OR_RETURN( ( pthread_attr_setdetachstate( &threadAttr_, 
      PTHREAD_CREATE_JOINABLE ) == 0 ),
      "Unable to set thread detach attribute" );

    TEAGN_TRUE_OR_RETURN( ( pthread_create( &threadId_, &threadAttr_, 
      TeThread::startThread, (void *) this ) == 0 ),
      "Thread creation failed" )
#else
#error "ERROR: Unsupported platform"
#endif

		threadStatus_ = TeThreadRunning;

    if( threadCurrPriority_ != TeThreadNormalPriority )
		{
      TEAGN_TRUE_OR_LOG( setPriority(threadCurrPriority_ ), 
        "Unable to set thread priority" );
    }

    return true;
  }
}


const TeThreadStatus& TeThread::getStatus() const
{
  return threadStatus_;
}

const TeThreadPriority& TeThread::getPriority() const
{
  return threadCurrPriority_;
}


bool TeThread::setPriority( const TeThreadPriority& newPriority )
{
  if( threadStatus_ == TeThreadRunning ) {
    #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
      int new_prio_value = THREAD_PRIORITY_NORMAL;
      
      switch( newPriority ) {
        case TeThreadIdlePriority :
        {
          new_prio_value = THREAD_PRIORITY_IDLE;
          break;
        }
        case TeThreadBelowNormalPriority :
        {
          new_prio_value = THREAD_PRIORITY_BELOW_NORMAL;
          break;
        }
        case TeThreadNormalPriority :
        {
          new_prio_value = THREAD_PRIORITY_NORMAL;
          break;
        }
        case TeThreadAboveNormalPriority :
        {
          new_prio_value = THREAD_PRIORITY_ABOVE_NORMAL;
          break;
        }
        case TeThreadTimeCriticalPriority :
        {
          new_prio_value = THREAD_PRIORITY_TIME_CRITICAL;
          break;
        }
        default :
        {
          TEAGN_LOG_AND_THROW( "Invalid thread priority" )
          break;
        }                                      
      }
      
      if( SetThreadPriority( threadHandler_, new_prio_value ) ) {
        threadCurrPriority_ = newPriority;
        return true;
      } else {
        return false;
      }      
    #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
      int curr_policy = 0;
      struct sched_param curr_sched_param; 
      
      TEAGN_TRUE_OR_THROW( ( pthread_getschedparam( threadId_, &curr_policy,
        &curr_sched_param ) == 0 ),
        "Unable to get thread scheduler parameters" );

	    int min_prio = sched_get_priority_min( curr_policy );
      int max_prio = sched_get_priority_max( curr_policy );
      
      switch( newPriority ) {
        case TeThreadIdlePriority :
        {
          curr_sched_param.sched_priority = min_prio;
          break;
        }
        case TeThreadBelowNormalPriority :
        {
          curr_sched_param.sched_priority = (int)
            ( ( max_prio - min_prio ) / 4 );
          break;
        }
        case TeThreadNormalPriority :
        {
          curr_sched_param.sched_priority = (int)
            ( 2 * ( max_prio - min_prio ) / 4 );
          break;
        }
        case TeThreadAboveNormalPriority :
        {
          curr_sched_param.sched_priority = (int)
            ( 3 * ( max_prio - min_prio ) / 4 );
          break;
        }
        case TeThreadTimeCriticalPriority :
        {
          curr_sched_param.sched_priority = max_prio;
          break;
        }
        default :
        {
          TEAGN_LOG_AND_THROW( "Invalid thread priority" )
          break;
        }                                      
      }
      
      if( pthread_setschedparam( threadId_, curr_policy,
        &curr_sched_param ) == 0 ) {
        
        threadCurrPriority_ = newPriority;
        return true;        
      } else {
        return false;
      }  
    #else
      #error "ERROR: Unsupported platform"
    #endif    
  } else {
    /* Thread not running */
    
    threadCurrPriority_ = newPriority;
    
    return true;
  }
}

bool TeThread::waitToFinish()
{
  if( threadStatus_ != TeThreadStopped ) {
    #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
      TEAGN_TRUE_OR_RETURN( ( WaitForSingleObjectEx( 
							threadHandler_, INFINITE, false ) == WAIT_OBJECT_0 ),
							"Thread joinning failed" )
    #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
      TEAGN_TRUE_OR_RETURN( ( pthread_join( threadId_, 0 ) == 0 ),
							"Thread joinning failed" )
    #else
      #error "ERROR: Unsupported platform"
    #endif  

    freeResources();
  }
  
  return true;
}

void TeThread::init()
{
	threadStatus_ = TeThreadStopped;

	threadCurrPriority_ = TeThreadNormalPriority;

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
	threadHandler_ = 0;
	threadId_ = 0;
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  threadId_ = 0;
#else
#error "ERROR: Unsupported platform"
#endif 
} 

void TeThread::freeResources()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    if( threadHandler_ != 0 ) {
      WaitForSingleObjectEx( threadHandler_, INFINITE, false );
            
      TEAGN_TRUE_OR_THROW( CloseHandle( threadHandler_ ),
        "Error closing thread handle" );
        
      threadHandler_ = 0;
      threadId_ = 0;

    }
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    if( threadId_ != 0 ) {
      pthread_join( threadId_, 0 );
      
      threadId_ = 0;
      
      TEAGN_TRUE_OR_THROW( ( pthread_attr_destroy( &threadAttr_ ) == 0 ),
        "Unable to destroy thread attributes" )        
    } 
  #else
    #error "ERROR: Unsupported platform"
  #endif  
}

void* TeThread::startThread( void* threadPtr )
{
  TEAGN_DEBUG_CONDITION( ( threadPtr != 0 ),
    "Invalid thread parameter pointer" )

	TeThread* thread = (TeThread*)threadPtr;

	thread->run();
	thread->threadStatus_ = TeThreadStopped;

	return 0;
}




