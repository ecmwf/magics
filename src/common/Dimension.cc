/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Dimension.h
    \brief Implementation of the Template class Dimension.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/

#include "Dimension.h"
#include "MagLog.h"

using namespace magics;

template<class T>
inline void helper(const string& in, T& out)
{
	std::stringstream is(in);
	is >> out;
}

Dimension::Dimension(const string& value, double parent, double def)
{
	if ( magCompare(value, "undef") ) {
		percent_ = def;
		absolute_ = parent*def*0.01;
		return;		
	}
	// Look for % ..
	string::size_type pos = value.find("%");
	if ( pos == 0 ) {
		MagLog::error() << "format(" << pos << ") is not valid\n";
		percent_ = def;
		absolute_ = parent*def*0.01;
		return;
	}		
	if (pos != string::npos) {		
		helper(value, percent_);
		absolute_ = parent*percent_*0.01;
		return;
	}

	// By default it is cm...
	helper(value, absolute_);
	percent_ = absolute_*100/parent;
}
