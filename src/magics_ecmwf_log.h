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

#ifndef magicsplusplus_ecmwf_H
#define magicsplusplus_ecmwf_H

#include "magics.h"
#include <fstream>
#include <unistd.h>

namespace magics {
  
/*!
  \brief Writes logging information for ECMWF users

  \note Designed to work at ECMWF only! Some MetVis Section members are excluded 
  from the list.
  \warning The log file can grow very quickly and needs constantly checking.
*/
inline MAGICS_NO_EXPORT void writeMagLog(const std::string &interface)
{
  if(magCompare(MAGICS_SITE,"ecmwf"))
  {
//   string logpath = getEnvVariable("MAG_LOGGING_FILE");
//   if(logpath.empty()) logpath = "/vol/netlog/magics/usage_log";
//   FILE* filePtr_ = fopen(logpath.c_str(),"a");
   FILE* filePtr_ = fopen("/vol/netlog/magics/usage_log","a");

   if(filePtr_)
   {
	string id = getEnvVariable("USER"); //user-id

	if(! (id=="cgm" || id=="cgs" || id=="cgi" || id=="cgk" || id=="cgr" || id=="cgjd" ) )
	{
		char            host[64];
		char            mytime[24];
		time_t          when;

		time(&when);
		strncpy(mytime,ctime(&when),24); 
		string smytime(mytime,24);      // date time

		string arch = getEnvVariable("ARCH");
		if(arch.empty()) arch = "NoArch";
		
		if(gethostname(host, sizeof(host)) == -1) strcpy(host, "NoHost");

		string inter;
		if(magCompare(interface,"magml")) inter = "x"; 
		else if(magCompare(interface,"fortran")) inter = "*"; 
		else inter = "?";

		string logline = inter + id + "/" +  smytime + "/"+ getMagicsVersionString() + "D/" + host + "/" + arch;
		fprintf(filePtr_,"%s\n",logline.c_str());

		fflush(filePtr_);

	}//endif NOT cgm or cgs
	fclose(filePtr_);
   }
  }
}

}
#endif
