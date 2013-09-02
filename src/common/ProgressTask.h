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
