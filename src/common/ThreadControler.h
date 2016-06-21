/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
