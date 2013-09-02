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

// File ProgressObject.h
// Magics Team - ECMWF 2004

#ifndef ProgressObject_H
#define ProgressObject_H

#include "magics.h"

#include "BasicGraphicsObject.h"

namespace magics {

class BaseDriver;

class ProgressObject: public BasicGraphicsObject {

public:
	ProgressObject(const string& progress) : progress_(progress) {}
	virtual ~ProgressObject() {}
    
	bool reproject(const Transformation&, BasicGraphicsObjectContainer& out) const 
		{ 
			out.push_back(const_cast<ProgressObject*>(this)); 
			return true; // remove thee obect from tthe in list!
		}
	void redisplay(const BaseDriver& driver) const;
    
	string getText() const {return progress_;}

protected:
	virtual void print(ostream&) const; 
	string progress_;

private:
// No copy allowed
	ProgressObject(const ProgressObject&);
	ProgressObject& operator=(const ProgressObject&);

// -- Friends
	friend ostream& operator<<(ostream& s,const ProgressObject& p)
		{ p.print(s); return s; }

};
class ClearObject: public BasicGraphicsObject {

public:
	ClearObject() {}
	virtual ~ClearObject() {}

	void redisplay(const BaseDriver& driver) const;

protected:
	virtual void print(ostream&) const;

};
} // namespace magics
#endif
