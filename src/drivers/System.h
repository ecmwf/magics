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

/*! \file System.h
    \brief In this file wrappers are defined for system calls
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004
    
*/
#ifndef SystemInfo_H
#define SystemInfo_H

#include <magics.h>
#include <cstdlib>
#include <pwd.h>
#include <unistd.h>

namespace magics {

/*! \class SystemInfo
    \brief This class gives information about the system.
    
     This class is very much platform dependent!!!.
     
     \todo move this file in a more central directory?
*/
class SystemInfo{

public:
	SystemInfo(){};
	~SystemInfo(){}; // not virtual - so do not inherit!
	
	MAGICS_NO_EXPORT string getHostName() const
	{
		char host[256];
		if(gethostname(host, sizeof(host)) == -1)
			strcpy(host, "unknown-host");
		string tmp(host);
		return tmp;
	};//end getHostName()
	
	MAGICS_NO_EXPORT string getTime() const
	{
		time_t when;
		time(&when);
		string tmp(ctime(&when));
		tmp.erase(25);  // need to cut \n\0
		tmp.erase(24);
		return tmp;
	};
	
	MAGICS_NO_EXPORT string getUserID() const
	{
		struct passwd* who = getpwuid(getuid());
		string tmp(who->pw_name);
		return tmp;
	};

	MAGICS_NO_EXPORT string getUserName() const
	{
		struct passwd* who = getpwuid(getuid());
		string tmp(who->pw_gecos);
		return tmp;
	};		
private:
// No copy allowed
	SystemInfo(const SystemInfo&);
	SystemInfo& operator=(const SystemInfo&);
};

} // namespace magics
#endif
