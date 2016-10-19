/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File ProgressTask.h
// Magics Team - ECMWF 2004

#ifndef ProgressTask_H
#define ProgressTask_H

#include "magics.h"

#include "Task.h"

namespace magics {

class ProgressTask: public Task {

public:
	ProgressTask();
	virtual ~ProgressTask();

protected:
	 virtual void print(ostream&) const; 

private:
// No copy allowed
	ProgressTask(const ProgressTask&);
	ProgressTask& operator=(const ProgressTask&);

// -- Friends
	friend ostream& operator<<(ostream& s,const ProgressTask& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
