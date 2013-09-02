/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
