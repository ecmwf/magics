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

/*! \file ValuePlot.cc
    \brief Implementation of the Template class ValuePlot.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 3-Mar-2004
    
    Changes:
    
*/

#include "ValuePlot.h"
#include "Symbol.h"
#include "MarkerValuePlotMethod.h"
#include "BothValuePlotMethod.h"
#include "Data.h"

using namespace magics;


ValuePlot::ValuePlot()
{
   
}


ValuePlot::~ValuePlot()
{
}



/*!
 Class information are given to the output-stream.
*/	

void ValuePlot::print(ostream& out)  const
{
	out << "ValuePlot[";
	ValuePlotAttributes::print(out);
	out << "]";
}


void ValuePlot::operator()(MatrixHandler& data, BasicGraphicsObjectContainer& parent)
{
	(*(this->method_)).clear();

    (*(this->method_))(data, parent.transformation());
    
    // Now we feed the task
    for ( vector<BasicGraphicsObject*>::const_iterator object = (*this->method_).begin(); object != (*this->method_).end(); ++object)
    	parent.push_back(*object);
}

void ValuePlot::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
	(*(this->method_)).clear();
    (*(this->method_))(data.points(parent.transformation(), false), parent.transformation());
    
    // Now we feed the task
    for ( vector<BasicGraphicsObject*>::const_iterator object = (*this->method_).begin(); object != (*this->method_).end(); ++object)
    	parent.push_back(*object);
}

void ValuePlot::visit(LegendVisitor&)
{
}
    

