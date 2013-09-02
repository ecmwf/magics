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

/*! \file TaylorProjection.cc
    \brief Implementation of TaylorProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 12 16:01:47 2008

*/

#include <TaylorProjection.h>
#include <Polyline.h>

using namespace magics;

/*!
  \brief Constructor
  
  \todo what does still need implmenting? can debug message be removed?
*/
TaylorProjection::TaylorProjection() 
{

}

/*!
  \brief Destructor
*/
TaylorProjection::~TaylorProjection() 
{
}

void TaylorProjection::print(ostream& out) const
{
    out << "TaylorProjection[";
    TaylorProjectionAttributes::print(out);
    out << "]"; 
} 

void TaylorProjection::init()  
{

	Transformation::init();
	PCEnveloppe_->clear();
	PCEnveloppe_->push_back(PaperPoint(min_, min_));
	PCEnveloppe_->push_back(PaperPoint(min_, max_));

	for ( float i = 0; i <= (3.14/2); i = i +0.1) {


		PCEnveloppe_->push_back(PaperPoint(max_*cos(i), max_*sin(i)));

	}

	PCEnveloppe_->push_back(PaperPoint(max_, min_));
	PCEnveloppe_->push_back(PaperPoint(min_, min_));
}

PaperPoint TaylorProjection::operator()(const UserPoint& xy)  const
{
	double sinus = (xy.y() < 1)  ? sin(acos(xy.y())) : 0;
	double cosinus = (xy.y() > 1) ? 1 :   xy.y();
	
	return PaperPoint(xy.x()*cosinus, xy.x()*sinus,xy.value());
}



PaperPoint TaylorProjection::operator()(const PaperPoint& point)  const
{

	return Transformation::operator()(point);
}



void TaylorProjection::revert(const PaperPoint& xy, UserPoint& point)  const
{
	
	double angle = atan(xy.y()/xy.x());
	point.y(cos(angle));
	point.x(xy.x()/cos(angle));	
	
}

bool TaylorProjection::needShiftedCoastlines()  const
{
	return false;
}

void TaylorProjection::aspectRatio(double& width, double& height)  
{

	Transformation::aspectRatio(width, height);
}

void TaylorProjection::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax)  const
{

	Transformation::boundingBox(xmin, ymin, xmax, ymax);
}

double TaylorProjection::getMinX()  const
{
	
	return min_;
}

double TaylorProjection::getMinY()  const
{
	
	return 0;
}

double TaylorProjection::getMaxX()  const
{
	return max_;
}

double TaylorProjection::getMaxY()  const
{

	return 1;;
}

void TaylorProjection::setMinX(double x)  
{
	min_ = x;
}

void TaylorProjection::setMinY(double y)  
{
	min_ = y;
}

void TaylorProjection::setMaxX(double x)  
{
	max_ = x;
}

void TaylorProjection::setMaxY(double y)  
{
	max_ = y;
}

double TaylorProjection::getMinPCX()  const
{
	return min_;
}

double TaylorProjection::getMinPCY()  const
{
	return min_;
}

double TaylorProjection::getMaxPCX()  const
{
	return max_;
}

double TaylorProjection::getMaxPCY()  const
{
	return max_;
}

Polyline& TaylorProjection::getPCBoundingBox() const
{
	if ( PCEnveloppe_->empty() ) {
		PCEnveloppe_->push_back(PaperPoint(min_, min_));
		PCEnveloppe_->push_back(PaperPoint(min_, max_));
		PCEnveloppe_->push_back(PaperPoint(max_, max_));
		PCEnveloppe_->push_back(PaperPoint(max_, min_));
		PCEnveloppe_->push_back(PaperPoint(min_, min_));
	}

	return *PCEnveloppe_;

}

Polyline& TaylorProjection::getUserBoundingBox() const
{
	if ( userEnveloppe_->empty() ) {
		userEnveloppe_->push_back(PaperPoint(min_, min_));
		userEnveloppe_->push_back(PaperPoint(min_, max_));
		userEnveloppe_->push_back(PaperPoint(max_, max_));
		userEnveloppe_->push_back(PaperPoint(max_, min_));
		userEnveloppe_->push_back(PaperPoint(min_, min_));
	}

	return *userEnveloppe_;
}

void TaylorProjection::setDefinition(const string& def)
{
	assert(def.empty());
}


