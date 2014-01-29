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

/*! \file Wind.cc
    \brief Implementation of the Template class Wind.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#include "Wind.h"
#include "MatrixHandler.h"
#include "Layout.h"
#include "CustomisedPoint.h"
#include "HistoVisitor.h"

using namespace magics;


Wind::Wind()
{
}


Wind::~Wind()
{
}

/*!
 Class information are given to the output-stream.
*/	

void Wind::print(ostream& out)  const
{
	out << "Wind";
}


void Wind::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{	

	if ( (*type_)(data, parent) )
		return;
	ThinningMethod* method=0;

	const Transformation& transformation = parent.transformation();

	try {
		method = MagTranslator<string, ThinningMethod>()(this->thinning_method_);
	}
	catch (...) {
	}
	if(!method) method = new BasicThinningMethod();

	method->set2D();

	this->factor_ = maground(this->thinning_factor_);
	this->nbPoints_ = this->thinning_factor_;
	this->rawOnly_ = false;
	this->height_ = parent.absoluteHeight();
	this->width_ = parent.absoluteWidth();
	method->set(*this);
	

	CustomisedPointsList points;
	std::set<string> request;
	request.insert("x_component");
	request.insert("y_component");
	request.insert("colour_component");
	(*method)(data, transformation, request, points);
	if (points.empty() )
		return;
		(*this->type_).prepare(parent, method->units());
				
		(*this->type_).adjust(points, transformation);

		
		for (vector<CustomisedPoint*>::const_iterator point =points.begin(); point != points.end(); ++point) {
			 bool north =  ((*point)->latitude() > 0);
			 double x = (*point)->longitude();
			 double y = (*point)->latitude();
			 PaperPoint xy = transformation(UserPoint((*point)->longitude(), (*point)->latitude()));
			 if ( transformation.in(xy) ) {
				 pair<double, double> component = std::make_pair(  (**point)["x_component"], (**point)["y_component"]);
				 transformation.reprojectComponents(x, y, component);
				  (*this->type_)(north, xy, component.first,  component.second, (**point)["colour_component"]);
			 }
		}
	(*this->type_).finish(parent);
}


void Wind::visit(LegendVisitor& legend)
{
	(*this->type_).visit(legend);
}

void  Wind::visit(Data& data, HistoVisitor& visitor)
{
	//if ( !matrix_ )
	//	return;

	(*this->type_).visit(data, data.points(*visitor.dataLayoutTransformation(), false), visitor);
	
}
