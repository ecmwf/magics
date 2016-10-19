/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Task.h
// Baudouin Raoult - ECMWF May 96

#ifndef Task_H
#define Task_H

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

class Task {
public:

// -- Contructors

	Task();

// -- Destructor

	virtual ~Task();

// -- Methods

	virtual void start()  = 0;
	virtual void stop()   = 0;
	virtual void wait()   = 0;
	virtual bool active() = 0;
	virtual void kill()   = 0;

private:

// No copy allowed

	Task(const Task&);
	Task& operator=(const Task&);

};

#endif
