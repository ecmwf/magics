/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file InputMatrix.h
    \brief Implementation of the Template class InputMatrix.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/

#include "InputMatrix.h"
#include "Transformation.h"
#include "Timer.h"
#include "MagJSon.h"
#include "ContourLibrary.h"
#include <limits>

#include "magics_windef.h"
#ifdef MAGICS_ON_WINDOWS
  #include <iterator>
#endif

using namespace magics;

InputMatrix::InputMatrix():  matrix_(0), u_(0), v_(0), speed_(0), direction_(0)
{
	Timer timer("InputMatrix", "Loading data");
	filter(field_);
	filter(u_component_);
	filter(v_component_);
	filter(wind_speed_);
	filter(wind_direction_);
	
}


InputMatrix::~InputMatrix()
{
}
/*!
 Class information are given to the output-stream.
*/
void InputMatrix::print(ostream& out)  const
{
	out << "InputMatrix[";
	out << field_;
	out << "]";
}

MatrixHandler& InputMatrix::matrix()
{
	Timer timer("InputMatrix", "Getting data");
	if ( simple_field_ ) 
		matrix_ = (*organization_).geoInterpret(&field_, *this);
	this->matrixHandlers_.push_back(new MatrixHandler(*matrix_));
	return *(this->matrixHandlers_.back()); 
}


void InputMatrix::filter(Matrix& data)
{
	data.missing(std::numeric_limits<double>::max());
	for ( unsigned int i = 0; i < data.size(); i++ )
		if ( data[i] <= suppress_below_ || data[i] >= suppress_above_ ) 
			data[i] = data.missing();
}
void InputMatrix::visit(MetaDataCollector& visitor)
{
	metadata(visitor);

}

void InputMatrix::metadata(MetaDataCollector& visitor) const
{
	ParamJSon data = ParamJSon(metadata_);

	for(map<string, string>::iterator key = visitor.begin(); key != visitor.end(); ++key )
	{
		
		auto entry = data.find(key->first);
		if ( entry != data.end() )
			key->second = entry->second;
		
		visitor.insert(make_pair(key->first, key->second));
	}

}

void InputMatrix::getReady(const Transformation& transformation)
{
	(*organization_).getReady(transformation);

	if(transformation.coordinateType() == Transformation::GeoType ) {
		if ( !field_.empty()) 
			matrix_ = (*organization_).geoInterpret(&field_, *this);
		if ( !u_component_.empty())
			u_ = (*organization_).geoInterpret(&u_component_, *this);
		if ( !v_component_.empty())
			v_ = (*organization_).geoInterpret(&v_component_, *this);
		if ( !wind_speed_.empty())
			speed_ = (*organization_).geoInterpret(&wind_speed_, *this);
		if ( !wind_direction_.empty())
			direction_ = (*organization_).geoInterpret(&wind_direction_, *this);
	}

	else {
		if ( !field_.empty())
			matrix_ = (*organization_).xyInterpret(&field_, *this);
		if ( !u_component_.empty())
			u_ = (*organization_).xyInterpret(&u_component_, *this);
		if ( !v_component_.empty())
			v_ = (*organization_).xyInterpret(&v_component_, *this);
		if ( !wind_speed_.empty())
			speed_ = (*organization_).xyInterpret(&wind_speed_, *this);
		if ( !wind_direction_.empty())
			direction_ = (*organization_).xyInterpret(&wind_direction_, *this);
	}
}



void  InputMatrix::release()
{
	 if ( matrix_ )
		 matrix_->release();

	 if ( u_ )
		 u_->release();

	 if ( v_ )
		 v_->release();

	 if ( speed_ )
	 	speed_->release();

	 if ( direction_ )
		direction_->release();
}

pair<double, double> InputMatrix::sd2uv(double s, double d)
{
	double a = 90 - (d);
	double pi = 3.14/180.;
	a *= pi;
    return std::make_pair(s * -1 * cos(a), s*-1* sin(a));
}


MatrixHandler& InputMatrix::xComponent()
{
	if ( !u_ )
		prepareComponents();

	this->matrixHandlers_.push_back(new MatrixHandler(*u_));
	return *(this->matrixHandlers_.back());
}


void InputMatrix::prepareComponents()
{
		Matrix u, v;
		// We calculate the u_component_ using the speed and direction...
		vector<double>::const_iterator speed = speed_->begin();
		vector<double>::const_iterator angle = direction_->begin();

		while ( speed != speed_->end() && angle != direction_->end() )
		{
			if ( *speed == speed_->missing() || *angle == direction_->missing() ) {
					 u.push_back(direction_->missing());
					 v.push_back(direction_->missing());
			}
			else if ( *speed == 0 && *angle == 0   ) {
					u.push_back(direction_->missing());
					v.push_back(direction_->missing());
			}
			else {
				std::pair<double, double> uv = sd2uv(*speed, *angle);
				u.push_back(uv.first);
				v.push_back(uv.second);
			}
			speed++;
			angle++;
		}
		u_ = speed_;
		v_ = direction_;
		u_->clear();
		v_->clear();
		std::copy(u.begin(), u.end(), back_inserter(*u_));
		std::copy(v.begin(), v.end(), back_inserter(*v_));
}


MatrixHandler& InputMatrix::yComponent()
{
	if ( !v_ )
		prepareComponents();
	Data::matrixHandlers_.push_back(new MatrixHandler(*v_));
	return *(this->matrixHandlers_.back()); 
}

PointsHandler& InputMatrix::points(const Transformation& transformation)
{
		this->pointsHandlers_.push_back(new BoxPointsHandler(this->matrix(), transformation, true));
		return *(this->pointsHandlers_.back());
}
void InputMatrix::scaling(double& scaling, double& offset) const
{
	scaling = 1;
	offset = 0;
	
   
    WebLibrary settings;
    MetaDataCollector needs;
    settings.askId(needs);
    metadata(needs);
    settings.getScaling(needs, scaling, offset);
    
    if (scaling == 0)
        scaling = 1;
	
}

void InputMatrix::customisedPoints(const BasicThinningMethod& thinning, const Transformation& transformation, const std::set<string>&, CustomisedPointsList& points)
{
	vector<UserPoint> thinx;
	vector<UserPoint> thiny;

	int factor = thinning.factor();
	MatrixHandler* inx = transformation.prepareData(xComponent());
	MatrixHandler* iny = transformation.prepareData(yComponent());
	transformation.thin(*inx, factor, factor, thinx);
	transformation.thin(*iny, factor, factor, thiny);

	vector<UserPoint>::const_iterator x = thinx.begin();
	vector<UserPoint>::const_iterator y = thiny.begin();

	if (thinx.empty()) return;
	while (x->value() == inx->missing() || y->value() == iny->missing())
	{
		++x;
		++y;
		if (x == thinx.end() || y == thiny.end()) 
			return;
	}
	CustomisedPoint *point = new CustomisedPoint(x->x(), x->y(), "");
	point->insert(make_pair("x_component", x->value()));
	point->insert(make_pair("y_component", y->value()));
	points.push_back(point);

//	double lastx = x->x();
//	double lasty = x->x();
	x++;
	y++;
	while ( x != thinx.end() && y != thiny.end() )
	{
		if ( x->value() != inx->missing() &&  y->value() != iny->missing() )
		{
			CustomisedPoint *point = new CustomisedPoint(x->x(), x->y(), "");
			point->insert(make_pair("x_component", x->value()));
			point->insert(make_pair("y_component", y->value()));
			points.push_back(point);

		}
//		lastx = x->x();
		x++;
		y++;
	} 
	delete inx;
	delete iny;
}


