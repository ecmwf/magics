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
/*! \file  TePrecision.h
    \brief This file contains a singleton to manage precision in TerraLib.
*/
#ifndef  __TERRALIB_INTERNAL_PROGRESSBASE_H
#define  __TERRALIB_INTERNAL_PROGRESSBASE_H

#include "TeDefines.h"
#include <string>
using std::string;

//! A generic Progress Interface.
/*
	It defines the methods that should be implemented by concrete classes.
	Assumes that a progress interface has a Caption to indicate the task
	being monitored and a Message to be displayed along with the progress 
	indication. A progress interface also know the total number of steps 
	required to finish the task being monitored.
*/
class TL_DLL TeProgressBase
{
public:

	//! Constructor
	TeProgressBase(){}

	//! Destructor
	virtual ~TeProgressBase() {}

	//! Sets the total number of steps to n 
	virtual void setTotalSteps(int n) =0;

	//! Sets the current amount of progress made to n
	virtual void setProgress(int n) = 0;

	//! Resets the progress interface
	virtual void reset() = 0;

	//! Resets the progress dialog
	virtual void cancel() = 0;

	//! Sets the label's text
	virtual void setMessage(const string& text) = 0;

	//!Returns the label's text
	virtual string getMessage() { return ""; }

	//! Returns true whether the process was cancelled
	virtual bool wasCancelled() = 0;
	
	//! Sets the caption associated to the progress interface
	virtual void setCaption(const string& cap) = 0;

};
#endif
