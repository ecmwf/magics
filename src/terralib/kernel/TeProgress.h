/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file  TeProgress.h
    \brief This file contains a singleton for progress information.
*/

#ifndef  __TERRALIB_INTERNAL_PROGRESS_H
#define  __TERRALIB_INTERNAL_PROGRESS_H

#include "TeProgressBase.h"

//! Provides the suport to implement an instance of a progress bar interface
/*
	This class is based on a Singleton Design Pattern (See "Design Patterns" book, page 127).
	Applications should set the Progress Interface that will be used by the TerraLib routines
	able to indicate progress in executing a task.
*/
class TL_DLL TeProgress
{
public:

	//! Returns the unique instance of a progress interface
	static TeProgressBase* instance();

	//! Sets the unique instance of a progress interface
	static void setProgressInterf( TeProgressBase* interf );

	//! Virtual destructor
	virtual ~TeProgress() {}  

protected:

	//! Empty constructor
	TeProgress() {}

private:
	
	static TeProgressBase* instance_;	//!< The unique instance of a progress interface

	//! No copy allowed
	TeProgress(const TeProgress&);

	//! No copy allowed
	TeProgress& operator=(const TeProgress&){return *this;}
};
#endif

