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

#ifndef   Tokenizer_H
#include "Tokenizer.h"
#endif

#include <algorithm>
#include <fstream>

Tokenizer::Tokenizer(const string& separators)
{
	for(unsigned int i=0; i<separators.length(); i++)
		separator_.insert(separators[i]);
}

Tokenizer::~Tokenizer()
{
}


void Tokenizer::operator()(const string& raw, vector<string>& v)
{
	int    index  = 0;
	int    length = raw.length();
	string token  = "";

	while(index < length)
	{
		char c = raw[index];
		if(find(separator_.begin(),separator_.end(),c) != separator_.end())
		{
			if(token.length()>0)
				v.push_back(token);
			token ="";
		}
		else
			token += c;

		index++;
	}

	if(token.length()>0)
		v.push_back(token);
}

void Tokenizer::operator()(std::istream& in, vector<string>& v)
{
	string raw;
	char c;

	while(in.get(c) && c != EOF && c != '\n')
		raw += c;

	operator()(raw,v);
}
