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

// File Tokenizer.h
// Manuel Fuentes - ECMWF Jan 97

#ifndef Tokenizer_H
#define Tokenizer_H

// Headers

#include "magics.h"


// 

class Tokenizer {
public:

// -- Contructors

    Tokenizer(const string&);

// -- Destructor

	~Tokenizer(); // Change to virtual if base class

// -- Methods
	
	void operator()(const string&, vector<string>&);
	void operator()(std::istream&,vector<string>&);

private:

// No copy allowed

	Tokenizer(const Tokenizer&);
	Tokenizer& operator=(const Tokenizer&);

// -- Members

	set<char,std::less<char> > separator_;     // To make searching faster

// -- Methods

	void print(ostream&) const;

	friend ostream& operator<<(ostream& s,const Tokenizer& p)
		{ p.print(s); return s; }

};



#endif
