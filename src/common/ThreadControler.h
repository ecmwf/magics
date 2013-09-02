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

// File ThreadControler.h
// Baudouin Raoult - ECMWF May 96

#ifndef ThreadControler_H
#define ThreadControler_H

#include <pthread.h>

#ifndef Task_H
#include "Task.h"
#endif

#ifndef MutexCond_H
#include "MutexCond.h"
#endif


// Don't subclass from ThreadControler put from Thread

class Thread;

namespace magics {

class ThreadControler : public Task {
public:

// -- Contructors
	
	// ThreadControler takes ownership of Thread

	ThreadControler(Thread*,bool detached = true);

// -- Destructor
	
	~ThreadControler();

// -- Overridden methods

	// From Task

	virtual void start();
	virtual void stop();
	virtual void kill();
	virtual void wait();
	virtual bool active();

protected:

// -- Members
	
	MutexCond  cond_;
	bool       detached_;

private:

// No copy allowed

	ThreadControler(const ThreadControler&);
	ThreadControler& operator=(const ThreadControler&);

// -- Members
	
	pthread_t      thread_;
	Thread     *proc_;
	bool           running_;

// -- Methods
	
	void execute();

// -- Class methods

	static void *startThread(void *);

};

} // namespace magics

#endif
