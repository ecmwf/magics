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

/*! \file MagFont.h
    \brief Definition of the Template class MagFont.
    
    Magics Team - ECMWF 2006
    
    Started: Mon 14-Aug-2006
    
    Changes:
    
*/

#ifndef MagFont_H
#define MagFont_H

#include "magics.h"
#include "MagTranslator.h"
#include "Colour.h"

namespace magics {

class MagFont {

public:
	MagFont(const string&, const string&, double size);
	MagFont(const string&);
	MagFont();
	~MagFont();

	const string& name() const      { return name_; }
	void name(const string& name)   { name_ = name; }

	const string& style() const     { return *styles_.begin(); }
	void style(const string& style);

	const set<string>& styles() const { return styles_; }

	double size() const             { return size_; }
	void size(double size)          { size_ = size; }

	const Colour& colour() const      { return colour_; }
	void colour(const Colour& colour)   {colour_ = colour; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const; 

	string name_;
	set<string> styles_;
	double size_;
	Colour colour_;

private: 
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagFont& p)
		{ p.print(s); return s; }
};

template<>
class MagTranslator<string,magics::MagFont> { 
public:
	MagFont* operator()(string s)
	{
		return new MagFont(s);
	}

	MagFont* magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

} // namespace magics
#endif
