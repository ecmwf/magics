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
/*! \file TeSlice.h
    \brief This file provides support for a slice structure
*/
#ifndef  __TERRALIB_INTERNAL_SLICE_H
#define  __TERRALIB_INTERNAL_SLICE_H

#include "TeUtils.h"

#include <string>
#include <vector>
#include <map>
#include <iostream>
using namespace std;

/*! 
A Slice is a structure that defines an interval of values and associates a
number of objects that have a certain property, or attribute, whithin this
interval.
*/ 
class TL_DLL TeSlice
{
public:
	int			count_;			//! number of objects container in the interval
	string		from_;			//! interval lower value 
	string		to_;			//! interval upper value
	
	//! Constructor
	TeSlice () : count_(0),from_(""),to_("") {}

	//! Constructor
	TeSlice(const string& from, const string& to, int count=0) :
     count_(count),
		from_(from),
		to_(to) {}

	TeSlice(const TeSlice& slice)
	{
		from_ = slice.from_;
		to_ = slice.to_;
		count_ = slice.count_;
	}

	TeSlice& operator=(const TeSlice& slice)
	{
		from_ = slice.from_;
		to_ = slice.to_;
		count_ = slice.count_;
		return *this;
	}
};

//! A vector of slices
typedef vector<TeSlice> TeSliceVector;
#endif

