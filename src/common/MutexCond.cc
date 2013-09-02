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

#include <time.h>

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

#ifndef MagLog_H
#include "MagLog.h"
#endif

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

#ifndef MutexCond_H
#include "MutexCond.h"
#endif


#define PTHREAD_INIT NULL

MutexCond::MutexCond(char tag):
	tag_(tag)
{

#if 0 
// linux
	pthread_mutexattr_t attr = XXXXXXXXXX;
#else
	pthread_mutexattr_t attr;
	pthread_condattr_t  cattr;
	THRCALL(::pthread_mutexattr_init(&attr));
	THRCALL(::pthread_condattr_init(&cattr));
#endif

#ifdef DCE_THREADS
	THRCALL(pthread_mutex_init(&mutex_,attr));
	THRCALL(pthread_cond_init(&cond_,cattr));
#else
	THRCALL(pthread_mutex_init(&mutex_,&attr));
	THRCALL(pthread_cond_init(&cond_,&cattr));
#endif


	inited_ = true;
	THRCALL(::pthread_mutexattr_destroy(&attr));
	THRCALL(::pthread_condattr_destroy(&cattr));
}

MutexCond::~MutexCond()
{
	THRCALL(pthread_mutex_destroy(&mutex_));

	pthread_cond_destroy(&cond_); // Don't use THRCALL as some thread may be waiting for that condition
	inited_ = false;
}

void MutexCond::lock()
{
	ASSERT(inited_);
	THRCALL(pthread_mutex_lock(&mutex_));
}

void MutexCond::unlock()
{
	ASSERT(inited_);
	THRCALL(pthread_mutex_unlock(&mutex_));
}

void MutexCond::wait()
{
	ASSERT(inited_);
//	AutoState x('.');
	THRCALL(pthread_cond_wait(&cond_,&mutex_));
}

bool MutexCond::wait(int sec)
{
	ASSERT(inited_);
//	AutoState x(':');
	timespec timeout = { ::time(0) + sec ,0};
	int n = pthread_cond_timedwait(&cond_,&mutex_,&timeout);
	if(n && n != ETIMEDOUT) THRCALL(n);
	return n == ETIMEDOUT;
}

void MutexCond::signal()
{
	ASSERT(inited_);
	pthread_cond_signal(&cond_);
}

void MutexCond::broadcast()
{
	ASSERT(inited_);
	pthread_cond_broadcast(&cond_);
}
