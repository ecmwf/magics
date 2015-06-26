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

// File BasicGraphicsObject.cc
// Magics Team - ECMWF 2004

#include "BasicGraphicsObject.h"
#include "BaseDriver.h"

using namespace magics;
BasicGraphicsObject::BasicGraphicsObject() : 
	parent_(0)
{
}

BasicGraphicsObject::~BasicGraphicsObject() 
{

}

void BasicGraphicsObject::print(ostream& out) const
{
	out << "BasicGraphicsObject[";
	out << "]";
}

void BasicGraphicsObjectContainer::print(ostream& out) const
{
	out << "BasicGraphicsObjectContainer[";
	for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object!= objects_.end(); ++object) 
		out << **object << endl;
	out << "]";
}


bool BasicGraphicsObjectContainer::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const
{
	bool more = false;
	for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object!= objects_.end(); ++object){
		if ( (*object)->buildTree(parent, frame, driver ) )
			more = true;
	}
	return more;
}

void BasicGraphicsObjectContainer::visit(const BaseDriver& driver) const
{
	for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object!= objects_.end(); ++object) 
			(*object)->redisplay(driver);
	for (vector<BasicGraphicsObject*>::const_iterator object = last_.begin(); object!= last_.end(); ++object)
				(*object)->redisplay(driver);
}

BasicGraphicsObjectContainer::~BasicGraphicsObjectContainer()
{

	for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object!= objects_.end(); ++object) {
		if (*object)
			delete *object;
		*object = 0;
	}
	objects_.clear();
}

void BasicGraphicsObjectContainer::clear()
{

	for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object!= objects_.end(); ++object) {
		if (*object) {

			delete *object;
		}
		*object = 0;
	}
	objects_.clear();
}
void BasicGraphicsObjectContainer::release()
{
	MagLog::debug() << "CLEAR CONTAINER" << objects_.size() << endl;
	for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object!= objects_.end(); ++object) {
		(*object)->release();
	}
	objects_.clear();
}
void BasicGraphicsObject::check()
{
	if (parent_)
		parent_->remove(this);
}
