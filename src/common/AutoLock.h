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

// File AutoLock.h
// Baudouin Raoult - ECMWF May 96

#ifndef AutoLock_H
#define AutoLock_H

// The class AutoLock is used to AutoLock a mutex in a multi-threaded
// environment. AutoLocks are MagExceptions safe.

class AutoLocker {
public:
	static void want(void*);
	static void got(void*);
	static void release(void*);
	static void analyse(void*);
};

template<class T> class AutoLock : public AutoLocker {
public:

// -- Contructors
	
    AutoLock(T& resource) : resource_(resource) 
							{ want(&resource); resource_.lock(); got(&resource);}
    AutoLock(T* resource) : resource_(*resource)
							{ want(resource); resource_.lock(); got(resource);}

// -- Destructor

    ~AutoLock() { release(&resource_); resource_.unlock(); }

private:

// No copy allowed

	AutoLock(const AutoLock<T>&);
	AutoLock<T>& operator=(const AutoLock<T>&);

// -- Members
	
    T& resource_;

};

#endif
