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

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

#ifndef Mutex_H
#include "Mutex.h"
#endif

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

#include <errno.h>


#if 0
static int _tc(int a, const char* m)
{
	
	return a;
}

#undef THRCALL
#define THRCALL(a) _tc(a,#a)
#endif


Mutex::Mutex(char tag) :
	exists_(false),
	tag_(tag)
{

#if defined(__GNUC__) && __GNUC__ < 3
#ifndef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP { PTHREAD_MUTEX_RECURSIVE_NP }
#endif
	pthread_mutexattr_t attr = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#else
	pthread_mutexattr_t attr;
	THRCALL(::pthread_mutexattr_init(&attr));
	THRCALL(::pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE));
#endif

	THRCALL(pthread_mutex_init(&mutex_,&attr));

	exists_ = true;
	THRCALL(::pthread_mutexattr_destroy(&attr));
}

Mutex::~Mutex() 
{
	THRCALL(pthread_mutex_destroy(&mutex_));
}

void Mutex::lock(void) 
{

	if(!this || !exists_)
	{
		cerr << "Mutex used before being contructed" << endl;
		return;
	}

	THRCALL(pthread_mutex_lock(&mutex_)); 
}

void Mutex::unlock(void) 
{
	if(!exists_)
	{
		cerr << "Mutex used before being contructed" << endl;
		return;
	}
	THRCALL(pthread_mutex_unlock(&mutex_));
}

//=============================================================

static Mutex globalMutex;

Mutex& Mutex::global()
{
	return globalMutex;
}
