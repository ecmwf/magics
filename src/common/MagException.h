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

#ifndef MagicsException_H
#define MagicsException_H

#include "magics.h"


namespace magics {

class MagicsException : public exception
{
 public:
	MagicsException( const string& why) : what_(why) {
        if (::getenv("MAGICS_ABORT_EXCEPTION")) {
            ::abort();
        }
    } 

    MagicsException() : what_("") {
        if (::getenv("MAGICS_ABORT_EXCEPTION")) {
            ::abort();
        }
    } 

	virtual const char *what() const throw() {  return what_.c_str(); }
	virtual ~MagicsException() throw() {}
 protected:
    virtual void print(ostream& out) const 
    {
        out << what_;
    }
    string what_;
    // -- Friends
	friend ostream& operator<<(ostream& s,const MagicsException& p)
		{ p.print(s); return s; }

};


class NoSuchFileException : public MagicsException
{
public:
	 NoSuchFileException( const string& file ):
		MagicsException("No Such File: " +  file){}
};     

class NoWritePermissionException : public MagicsException
{
public:
	 NoWritePermissionException( const string& file ):
		MagicsException("No write permission to write file: " +  file){}
};     
    
class NotYetImplemented : public MagicsException
{
public:
	 NotYetImplemented(const string& type, const string& method ):
		MagicsException( type + " " + method  + " : not yet implemented... "){}
};         

class MethodNotYetImplemented : public NotYetImplemented
{
public:
	 MethodNotYetImplemented(const string& method):
		NotYetImplemented("Method",  method) {}  
};    
     
class ParameterNotYetImplemented : public NotYetImplemented
{
public:
	 ParameterNotYetImplemented(const string& param):
		NotYetImplemented("Parameter", param){}
}; 

class AssertionFailed : public MagicsException {
public:
    AssertionFailed(const string&);
    AssertionFailed(const char*,int,const char*,const char*);
};


inline void Assert(int code,const char *msg,int line,const char *file,
    const char *proc)
{
    if(code != 0)
        throw AssertionFailed(msg,line,file,proc);
}


class IgnoreException : public MagicsException
{
public:
	 IgnoreException():
		MagicsException("Just Ignore...") {}
};

} // end namespace magics

#define ASSERT(a)  Assert(!(a),#a,__LINE__,__FILE__,__FUNCTION__)

#endif
// EXCEPTION_H_

