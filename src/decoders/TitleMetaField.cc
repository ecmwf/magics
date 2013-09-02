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

/*! \file TitleMetaField.cc
    \brief Implementation of the Template class TitleMetaField.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/



#include "TitleMetaField.h"

using namespace magics;

TitleMetaField::TitleMetaField(const string& token) : token_(token) 
{
}


TitleMetaField::~TitleMetaField() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void TitleMetaField::print(ostream& out)  const
{
    if ( token_ == "newline") {
        //out << ends;
        return;
    }
	out << "TitleMetaField[";
    out << "token=" << token_;
    for (const_iterator attribute = begin(); attribute != end(); ++attribute) 
        out << ", " <<  attribute->first << "=" << attribute->second;
	out << "]";
}

void TitleMetaField::operator()(vector<string>& title ) const
{
	
	 ostringstream out;
		out << "TitleMetaField[";
	    out << "token=" << token_;
	    for (const_iterator attribute = begin(); attribute != end(); ++attribute) 
	        out << ", " <<  attribute->first << "=" << attribute->second;
		out << "]";
	    
	    title.back() += out.str();
}

