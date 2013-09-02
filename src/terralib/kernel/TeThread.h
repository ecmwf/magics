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
/** \file TeThread.h
 *  \brief This file contains the base thread class.
 *  \author Emiliano F. Castejon <castejon@dpi.inpe.br>
 *  \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 */

#ifndef __TERRALIB_INTERNAL_TETRHEAD_H
#define __TERRALIB_INTERNAL_TETRHEAD_H
  
#include "TeThreadDatatypes.h"
#include "TeDefines.h"
  
#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
#include <windows.h>
#define TeDelayThread( seconds ) Sleep( seconds * 1000)
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
#include <pthread.h>
#include <unistd.h>
#define TeDelayThread( seconds ) sleep( seconds )
#else
#error "ERROR: Unsupported platform"
#endif


/** \class TeThead
 *  \brief xxxxx
 *
 *
 *  If you are interested in using TerraLib thread
 *  support in a object-oriented way, you should create
 *  a subclass of TeThread in order to creata a threadable
 *  class. Subclasses must implement the method "run".
 *  Than in the main thread, you can call the start method,
 *  it is non-blocking.<br>
 *  If you have a function and want to make it a thread,
 *  you should look TeThreadFunctor class.
 *  
 *
 *  \sa TeThreadFunctor
 *  \author Emiliano F. Castejon <castejon@dpi.inpe.br>
 *  \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 *  \ingroup Utils
 *
 */  
class TL_DLL TeThread
{
    public :
      
      
      /** \brief Default constructor.
       */
      TeThread();             
      
      /** \brief Virtual destructor
       */
      virtual ~TeThread();

      /** \brief Starts the current thread.
       *  \return true if OK, false on errors.
       */      
      bool start();
      
      /** \brief Gets the current thread status.
	     *  \return The current thread status.
       */
      const TeThreadStatus& getStatus() const;

      /** \brief Returns the current thread priority.
	     *  \return The current thread priority.
       */
      const TeThreadPriority& getPriority() const;

      /** \brief Changes the current thread priority.
       *  \param newPriority The new thread priority.
       *  \return true if OK, false on errors.
       */      
      bool setPriority(const TeThreadPriority& newPriority);
            
      /** \brief Block the current thread until the thread
	     *  handled by this instance terminates.
       *  \return true if OK, false if wait failed.
       */
      bool waitToFinish();

	protected:

		/** \brief Thread execution method: this method will be called when the thread starts.
		 *         Subclasses must reimplement it.
     */
		virtual void run() = 0;

		/** \brief Initialize the internal default state.
		 */
		void init();

		/** \brief Free all thread allocated resources.
		 */      
   void freeResources();

	private:

    /** \brief The thread start function.
     *  \param threadPtr The thread class pointer.
     */
		static void* startThread(void* threadPtr );        

		/** \brief Copy constructor not allowed.
		 *  \param rhs External reference.
		 */
		TeThread(const TeThread& rhs);      
    
		/** \brief Assignment operator not aloowed.
		 *  \param rhs External reference.
		 *  \return A const reference.
		 */
		const TeThread& operator=(const TeThread& rhs);

	protected:

#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    HANDLE threadHandler_;			//!< Win32 thread handler.
		LPDWORD threadId_;				//!< Win32 thread ID
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
		pthread_t threadId_;			//!< Pthread thread ID.
		pthread_attr_t threadAttr_;		//!< Pthread thread attributes.
#else
#error "ERROR: Unsupported platform"
#endif

    TeThreadStatus threadStatus_;			//!< The current thread status variable.
    TeThreadPriority threadCurrPriority_;   //!< The current thread priority.
};
  
#endif	// __TERRALIB_INTERNAL_TETRHEAD_H

