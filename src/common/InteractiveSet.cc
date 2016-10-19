/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File InteractiveSet.cc
// Magics Team - ECMWF 2004

#include "InteractiveSet.h"
#include "BaseDriver.h"

using namespace magics;


InteractiveSet::InteractiveSet(Layer* layer, const string& box) : BasicGraphicsObject(layer, box)
{
	begin_ = new InteractiveBegin();
	
	end_ = new InteractiveEnd();
}


bool InteractiveSet::reproject(const Transformation& transformation, GraphicsList& out) const
{
	out.push_back(begin_);
	for ( const_iterator item = begin(); item != end(); ++item) {
		(*item)->reproject(transformation, out);
		MagLog::dev() << "InteractiveSet::reproject-->" << **item << endl;
	}
	out.push_back(end_);
	return false;
}
	
void InteractiveSet::redisplay(const BaseDriver& driver) const
{
	MagLog::dev() << "	InteractiveSet::redisplay---> to be implemented!\n";
	
}

void InteractiveSet::print(ostream& out) const
{
	out << "InteractiveSet[]" << endl;
}

InteractiveBegin::InteractiveBegin(Layer* layer, const string& box) : BasicGraphicsObject(layer, box)
{
}

	
void InteractiveBegin::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
	
}
 
void InteractiveBegin::print(ostream& out) const
{
	out << "InteractiveBegin[";
	string sep = "";
	for ( const_iterator action = begin(); action != end(); ++action) {
		out << sep << action->first << "=" << *action->second;
		sep =",";
	}
	out << "]";
}

bool InteractiveBegin::reproject(const Transformation& transformation, GraphicsList& out) const
{
	return true;
}


InteractiveEnd::InteractiveEnd(Layer* layer, const string& box)  : BasicGraphicsObject(layer, box)
{
	
}

void InteractiveEnd::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

bool InteractiveEnd::reproject(const Transformation& transformation, GraphicsList& out) const
{
	return true;
}

	

void InteractiveEnd::print(ostream& out) const
{
	out << "InteractiveEnd[]";	
}

void InteractiveLink::print(ostream& out) const
{
	out << "InteractiveLink[url=" << url_ << "]";
}

void InteractiveMagnify::print(ostream& out) const
{
	out << "InteractiveMagnify[factor=" << factor_ << "]";
}
