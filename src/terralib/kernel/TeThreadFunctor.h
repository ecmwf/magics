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
/** \file TeThreadFunctor.h
 *  \brief This file contains a base class for thread in function style manner.
 *  \author Emiliano F. Castejon <castejon@dpi.inpe.br>
 *  \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 */

#ifndef __TERRALIB_INTERNAL_TETRHEADFUNCTOR_H
#define __TERRALIB_INTERNAL_TETRHEADFUNCTOR_H
  
#include "TeThread.h"
#include "TeThreadParameters.h"
#include "TeSharedPtr.h"

/** \class TeThreadFunctor
 *  \brief A base class for thread in function style manner.
 *
 *
 *  If you are interested in using TerraLib thread
 *  support in a function style model, you should create
 *  objects of this class by specifying a function to be called by
 *  the thread when it starts.<br>
 *  To start the thread, call start method,
 *  it is non-blocking.<br>
 *  If you want to use the thread support
 *  in a object oriented way, see TeThread class.
 *  
 *
 *  \sa TeThread
 *  \author Emiliano F. Castejon <castejon@dpi.inpe.br>
 *  \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 *  \ingroup Utils
 *
 */  
class TL_DLL TeThreadFunctor : public TeThread
{
	public:

        /** \brief Type definition for a thread start function pointer.
		 *  \param params The thread parameters.
         *  \return true if OK, false on erros.
         */
        typedef bool (*TeThreadStartFunctT)( const TeThreadParameters& params );
        
        /** @typedef TeSharedPtr< TeThread > pointer
        * Type definition for an thread instance pointer. 
        */
        typedef TeSharedPtr< TeThreadFunctor > pointer;         

		/** \brief Default Constructor.
         */
        TeThreadFunctor();             

        /** \brief Alternate Constructor.
		 *  \param startFuncPtr The thread start function pointer.
         */
        TeThreadFunctor( TeThreadStartFunctT startFuncPtr );

        /** \brief Default Destructor
		 */
        ~TeThreadFunctor();

        /** \brief Returns the current thread execution return value.
         */      
        const bool& getReturnValue() const;

		/** \brief Change the internal thread start function pointer.
		 *  \param startFuncPtr The new thread start function pointer.
		 *  \note The thread mus be stopped for calling this method.
         */      
        void setStartFunctPtr( TeThreadStartFunctT startFuncPtr );

		/** \brief Sets the parameter that will be passed to the function after
		 *         the thread startup.
		 *  \param params The parameters to the thread function.
		 *  \note The thread mus be stopped for calling this method.
		 */
		void setParameters(const TeThreadParameters& params);

	protected:

		/** \brief This is the implementation of the thread execution method: this method
		 *         will be called when the thread starts.
         */
		void run();

	protected:

		bool threadReturnValue_;					//!< User function return value.
        TeThreadParameters threadUserParams_;		//!< The current user parameters instance.
        TeThreadStartFunctT threadStartFuncPtr_;	//!< A pointer to the current user thread start function.
};
  
#endif	// __TERRALIB_INTERNAL_TETRHEADFUNCTOR_H

