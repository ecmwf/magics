/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
     string id = getEnvVariable("USER"); //user-id

     if(! (id=="cgm" || id=="cgs" || id=="cgi" || id=="cgk" || id=="cgr" ) )
     {
//     string logpath = getEnvVariable("MAG_LOGGING_FILE");
//     if(logpath.empty()) logpath = "/vol/netlog/magics/usage_log";
//     FILE* filePtr_ = fopen(logpath.c_str(),"a");
       FILE* filePtr_ = fopen("/vol/netlog/magics/usage_log","a");
       if(filePtr_)
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
		fclose(filePtr_);
       }
     }//endif NOT cgm or cgs
  }
}

}
#endif
