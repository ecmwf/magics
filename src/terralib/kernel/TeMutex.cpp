#include "TeMutex.h"

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS

  #include  <stdio.h>

#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
    
#else
    #error "Unsuported plataform"
#endif   

TeMutex::TeMutex()
{
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    m_sa_.nLength = sizeof( m_sa_ );
    m_sa_.lpSecurityDescriptor = NULL;
    m_sa_.bInheritHandle = TRUE;

    m_access_ = ::CreateMutex( &m_sa_, false, 0 );

    TEAGN_TRUE_OR_THROW( ( m_access_ != 0 ),
      "Unable to create mutex object instance" )
  
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  
    pthread_mutexattr_t access_attr;
    TEAGN_TRUE_OR_THROW( ( pthread_mutexattr_init( &access_attr ) == 0 ),
      "Unable to init mutex attributes" );

    pthread_mutexattr_settype( &access_attr, PTHREAD_MUTEX_DEFAULT );
    TEAGN_TRUE_OR_THROW( 
      ( pthread_mutex_init( &m_access_, &access_attr ) == 0 ),
      "Unable to init mutex" )
  
  #else
    #error "Unsuported plataform"
  #endif   
}


TeMutex::~TeMutex()
{
  unLock();
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  
    ::CloseHandle( m_access_ );
  
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  
    pthread_mutex_destroy( &m_access_ );
  
  #else
    #error "Unsuported plataform"
  #endif   
}

