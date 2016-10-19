/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
