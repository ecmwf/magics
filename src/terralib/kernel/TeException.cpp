/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeException.h"
#include "TeErrorLog.h"
#include <cstring>
#include <errno.h>

TeException::TeException ( TeErrorType msgCode, const string& userText, bool haserrno ):
		userText_ ( userText )
{			
		if ( haserrno )
			userText_ = userText + " " + strerror ( errno );
		
		TeErrorLog::instance().insert( msgCode, userText_ );
}

string 
TeException::message() const
{
	return TeErrorLog::instance().message ( msgCode_ ) + " " + userText_;
}

TeErrorType
TeException::code () const
{
	return msgCode_;
}
