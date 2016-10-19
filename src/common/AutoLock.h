/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
