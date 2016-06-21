/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagFont.cc
    \brief Implementation of the Template class MagFont.
    
    Magics Team - ECMWF 2006
    
    Started: Mon 14-Aug-2006
    
    Changes:
    
*/

#include "MagFont.h"

using namespace magics;

MagFont::MagFont(const string& name, const string& style, double size) :
	name_(name), size_(size)
{
	styles_.insert(lowerCase(style));
}

MagFont::MagFont(const string& name) :
	name_(name), size_(0.5)
{
}

MagFont::MagFont() : name_("sansserif"), size_(0.5)
{
}

MagFont::~MagFont() 
{
	styles_.clear();
}

/*!
 Class information are given to the output-stream.
*/		
void MagFont::print(ostream& out)  const
{
	out << "MagFont[" << name_ << ", " << colour_ << ", ";
	for ( set<string>::const_iterator style = styles_.begin(); style !=  styles_.end(); ++style) 
		out << *style << ", ";
	out << size_ << "]"; 
}

void MagFont::style(const string& style) 
{ 
	string lowstyle = lowerCase(style);
	if ( styles_.size() == 1 && styles_.begin()->empty() )
		styles_.clear();
	styles_.insert(lowstyle);
}
