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

/*! \file ViewNode.cc
    \brief Implementation of the Template class ViewNode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:
    
*/

#include "HistoVisitor.h"
#include "Transformation.h"
#include "Layout.h"
#include "BaseDriver.h"

using namespace magics;



HistoVisitor::HistoVisitor(): basic_(false) , dataLayoutTransformation_(0)
{
	name("histigram");
}

HistoVisitor::~HistoVisitor()
{


}
void HistoVisitor::print(ostream& s) const
{
	s << "HistoVisitor[";
	Layout::print(s);
	s << "]";
}

void HistoVisitor::visit(BasicGraphicsObjectContainer& tree)
{

	tree.push_back(this);
	// the Layout has been added to a Container, it will be delted automatically!
	
}

void HistoVisitor::redisplay(const BaseDriver& driver) const { 
	MagLog::dev() << " I am a HistoVisitor!" << *this << endl;	
	driver.redisplay(*this);
}

void HistoVisitor::visit(BasicSceneObject& object) { 
		object.visit(*this); 
}
