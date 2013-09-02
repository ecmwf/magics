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

#include "WebFormat.h"
#include <unistd.h>
#include <signal.h>
#include <new>
#include "Timer.h"

using namespace magics;

namespace magml
{

pair<string, string> cut(const string& in)
{
	string::const_iterator c = in.begin();
	string var;
	string current;
	bool  param = true;

	while ( c != in.end() )
	{
		if ( *c == '='  && param )
		{			
			var = current;
			current = "";
			param= false;
		}
		else {
			current.push_back(*c);
		}
		c++;
	}
	return make_pair(var, current);
}

void setVariable(const string& in, map<string, string>& variables)
{
	if(in[0] != '-')
	{
		MagLog::warning() << "argument " << in << " ignored.\n";
		return;
	}
	variables.insert(magml::cut(in.substr(1, in.size())));
}

} //end of namespace magml

void catch_alarm(int)
{
	printf("MagPlus ERROR: Operation timed out. Exiting...\n");
	abort();   
}

void no_memory ()
{
	printf("MagPlus ERROR: Failed to allocate memory!\n");
	abort();   
}


int main(int argc, char **argv)
{
  set_new_handler(no_memory);
  try
  {
	if(argc<2)
	{
		MagLog::error() << " Usage: " << argv[0] << " file.magml [-varname=varvalue] -timeout=seconds)\n";		   
		exit (1);
	}

	map<string, string> variables;

	for( int i = 2; i < argc; i++ )
		magml::setVariable(argv[i], WebInterpretor::parameters());

	//for ( int i = 0; i < 10; i++) 
	{
	try
	{
		map<string, string>::const_iterator out = WebInterpretor::parameters().find("timeout");
		int timeout = (out == WebInterpretor::parameters().end() ) ? 0 : tonumber(out->second);
		MagLog::info() << "Time out armed for " << timeout << endl; 
		signal(SIGALRM, catch_alarm);
		alarm(timeout); 
		WebInterpretor::magml(argv[1]);
		alarm(0);
	}
	catch (...) {
		abort();
	}
	}
  }
  catch (MagicsException& e)
  {
	printf("MagPlus ERROR: Fatal!\n");
	abort();
  }
}
