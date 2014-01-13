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

/*! \file InputData.cc
    \brief Implementation of the Template class InputData.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 6-May-2004
    
    Changes:
    
*/

#include "InputData.h"
#include "SciMethods.h"




/*!
 Class information are given to the output-stream.
*/		

void InputData::print(ostream& out)  const
{
	out << "InputData[";
    InputDataAttributes::print(out);
	out << "]";
}


void  InputData::dateSetting(vector<string>& dates, vector<double>& values, DateTime& base)
{
	if ( dates.empty() )
		return;
	base = DateTime(dates.front());
	for (vector<string>:: iterator date = dates.begin(); date != dates.end(); ++ date ) {
		DateTime d(*date);
		values.push_back(d-base);
	}
}


void  InputData::numberSetting(vector<double>& from, vector<double>& values)
{
	std::copy(from.begin(), from.end(), back_inserter(values));
}


void  InputData::prepare()
{
    if ( !x_values_.empty() ) return;

    if ( magCompare(x_type_, "date" ) ) {
    	dateSetting(date_x_, x_values_, baseDateX_);
    	dateSetting(date_x2_, x2_values_, baseDateX_);
    }
    else {
    	numberSetting(x_,  x_values_);
    	numberSetting(x2_,  x2_values_);
    	numberSetting(longitudes_,  x_values_);
    }
    if ( magCompare(y_type_, "date" ) ) {
      	dateSetting(date_y_, y_values_, baseDateY_);
      	dateSetting(date_y2_, y2_values_, baseDateY_);
     }
     else {
      	numberSetting(y_,  y_values_);
      	numberSetting(y2_,  y2_values_);
      	numberSetting(latitudes_,  y_values_);
     }
    
    vector<double>::iterator x = x_values_.begin();
	vector<double>::iterator y = y_values_.begin();
	vector<double>::iterator v = values_.begin();
	vector<double>::reverse_iterator x2 = x2_values_.rbegin();
	vector<double>::reverse_iterator y2 = y2_values_.rbegin();

    while ( x != x_values_.end() && y != y_values_.end() ) {
    	double val = 0;
    	if ( v != values_.end() ) {
    		val = *v;
    		++v;
    	}
    	push_back(new UserPoint(*x, *y, val));
    	++x;
    	++y;

    }
    while ( x2 != x2_values_.rend() && y2 != y2_values_.rend() ) {
        	double val = 0;
        	if ( v != values_.end() ) {
        		val = *v;
        		++v;
        	}
        	push_back(new UserPoint(*x2, *y2, val));
        	++x2;
        	++y2;
        }
}
 
void InputData::customisedPoints(const Transformation& transformation, const std::set<string>& needs, CustomisedPointsList& out, bool)
{
	customisedPoints(transformation, needs, out);
}
void InputData::customisedPoints(const Transformation& transformation, const std::set<string>& needs, CustomisedPointsList& out)
{
	prepare();
	vector<double>::const_iterator x = x_values_.begin();
	vector<double>::const_iterator x2 = x2_values_.begin();
	vector<double>::const_iterator y = y_values_.begin();
	vector<double>::const_iterator y2 = y2_values_.begin();
	vector<double>::const_iterator xc = x_component_.begin();
	vector<double>::const_iterator yc = y_component_.begin();
	vector<double>::const_iterator v = values_.begin();
	while (  x != x_values_.end() && y != y_values_.end()  ) {
		double valx = *x;
		double valy = *y;
		if ( magCompare(x_type_, "date" ) ) {
			DateTime ref(transformation.getReferenceX());
			double shift = ref - baseDateX_;
			valx -= shift;
		}
		if ( magCompare(y_type_, "date" ) ) {
			DateTime ref(transformation.getReferenceY());
			double shift = ref - baseDateY_;
			valy -= shift;
		}

		CustomisedPoint* point = new CustomisedPoint();


		point->longitude(*x);
		point->latitude(*y);
		out.push_back(point);



		if ( same(*x, x_missing_) )
			point->missing(true);
		(*point)["x"] = valx;
		(*point)["x_lower"] = valx;
		x++;

		if ( same(*y, y_missing_) )
			point->missing(true);
		(*point)["y"] = valy;
		(*point)["y_lower"] = valy;
		y++;

		double speed = 0;
		double angle = 0;
		if ( xc != x_component_.end() && yc != y_component_.end() ) {
			speed = sqrt( (*xc * *xc)  +  (*yc * *yc) );
			angle = ( 270. - atan2(*yc, *xc)*(180/M_PI) );
			if ( angle == 0 ) angle =0.1;
			(*point)["x_component"] = *(xc++);
			(*point)["y_component"] = *(yc++);
			(*point)["intensity"] = speed;
			(*point)["direction"] = angle;
		}
		if ( v != values_.end() ) {
			(*point)["colour_component"] = *(v++);
		}
		else
			(*point)["colour_component"] = speed;

		if (needs.find("bar")  != needs.end()) {

			if ( y2 != y2_values_.end() ) {
				(*point)["y_upper"] = *(y2++);
			}
			if ( x2 != x2_values_.end() ) {

				(*point)["x_upper"] = *(x2++);
			}

		}
	}



	if ( needs.find("area")  == needs.end() )
		return;
	vector<double>::reverse_iterator xr2 = x2_values_.rbegin();
	vector<double>::reverse_iterator yr2 = y2_values_.rbegin();
	double shiftx = 0;
	double shifty = 0;
	if ( magCompare(x_type_, "date" ) ) {
		DateTime ref(transformation.getReferenceX());
		shiftx = ref - baseDateX_;

	}
	if ( magCompare(y_type_, "date" ) ) {
		DateTime ref(transformation.getReferenceX());
		shifty = ref - baseDateY_;

	}
	while (  xr2 != x2_values_.rend() && yr2 != y2_values_.rend()  ) {
		double x = *xr2;

		CustomisedPoint* point = new CustomisedPoint();
		if ( same(*xr2, x_missing_) )
			point->missing(true);
		(*point)["x"] = *(xr2++) - shiftx;

		if ( same(*yr2, y_missing_) )
			point->missing(true);
		(*point)["y"] = *(yr2++) -shifty;
		out.push_back(point);
	}
}

void InputData::customisedPoints(const std::set<string>&, CustomisedPointsList& out)
{
	prepare();
	vector<double>::const_iterator x = x_values_.begin();
	vector<double>::const_iterator y = y_values_.begin();
	vector<double>::const_iterator xc = x_component_.begin();
	vector<double>::const_iterator yc = y_component_.begin();
	vector<double>::const_iterator v = values_.begin();
	while (  x != x_values_.end() || y != y_values_.end()  ) {
		    	CustomisedPoint* point = new CustomisedPoint();


		    		point->longitude(*x);
		    		point->latitude(*y);
		    		out.push_back(point);

		    	if ( x != x_.end() ) (*point)["x"] = *(x++);
		    	if ( y != y_.end() ) (*point)["y"] = *(y++);
		        double speed = 0;
		    	if ( xc != x_component_.end() && yc != y_component_.end() ) {
		    		speed = sqrt( (*xc * *xc)  +  (*yc * *yc) );
		    		(*point)["x_component"] = *(xc++);
		    		(*point)["y_component"] = *(yc++);
		    	}
		    	if ( v != values_.end() ) {
		    		(*point)["colour_component"] = *(v++);
		    	}
		    	else
		    		(*point)["colour_component"] = speed;



		    }
}

MatrixHandler& InputData::matrix()
{
	if  ( !matrix_ ) {
		prepare();
		matrix_ = (*binning_)(*this);
	}

	matrixHandlers_.push_back(new MatrixHandler(*matrix_));
	return *(matrixHandlers_.back());
}


void InputData::getReady(const Transformation& transformation)
{
	try {
		for ( vector<string>::const_iterator x = date_x_.begin(); x != date_x_.end(); ++x )
			x_.push_back(transformation.x(*x));
		}
	catch (...) {}
	try {
		for ( vector<string>::const_iterator y = date_y_.begin(); y != date_y_.end(); ++y )
			y_.push_back(transformation.y(*y));
	}
	catch (...) {}
	
}

void InputData::visit(Transformation& transformation)
{
	// get the data ...
	try {
		prepare();
		if ( transformation.getAutomaticX() ) {
			double min = INT_MAX;
			double max = -INT_MAX;
			for (vector<double>::iterator x = x_values_.begin(); x != x_values_.end(); ++x) {
				if ( same(*x, x_missing_) ) continue;
				if ( min > *x ) min = *x;
				if ( max < *x ) max = *x;
			}
			for (vector<double>::iterator x = x2_values_.begin(); x != x2_values_.end(); ++x) {
				if ( same(*x, x_missing_) ) continue;
				if ( min > *x ) min = *x;
				if ( max < *x ) max = *x;
			}
			if ( magCompare(x_type_, "date" ) ) {

				transformation.setDataMinMaxX(min, max, baseDateX_);

			}
			else {
				transformation.setMinMaxX(min, max);

			}
		}
		if ( transformation.getAutomaticY() ) {
			double min = INT_MAX;
			double max = -INT_MAX;
			for (vector<double>::iterator y = y_values_.begin(); y != y_values_.end(); ++y) {
				if ( same(*y, y_missing_) ) continue;
				if ( min > *y ) min = *y;
				if ( max < *y ) max = *y;
			}
			for (vector<double>::iterator y = y2_values_.begin(); y != y2_values_.end(); ++y) {
				if ( same(*y, y_missing_) ) continue;
				if ( min > *y ) min = *y;
				if ( max < *y ) max = *y;
			}
			if ( magCompare(y_type_, "date" ) ) {
				transformation.setDataMinMaxY(min, max, baseDateY_);

			}
			else {
				transformation.setMinMaxY(min, max);

			}
		}

	}
	catch ( ... ) {}
}


PointsHandler& InputData::points(const Transformation& transformation, bool all)  {
	 prepare();

	 if ( magCompare(x_type_, "date" ) || magCompare(y_type_, "date" )) {
// create a new points list!
		PointsList*  points = new PointsList();
		for (PointsList::iterator pt = begin(); pt != end(); ++pt) {
			UserPoint* point = *pt;
			if ( magCompare(x_type_, "date" ) ) {
				DateTime ref(transformation.getReferenceX());
				double shift = ref - baseDateX_;
				point->x_ -= shift;
			}
			if ( magCompare(y_type_, "date" ) ) {
				DateTime ref(transformation.getReferenceY());
				double shift = ref - baseDateY_;
				point->y_ -= shift;
			}
			if ( !transformation.in(*point) ) {
				point->flagMissing();
				if ( all )
					points->push_back(point);
				else
					delete point;
			}
			else {
				points->push_back(point);
			}
		}
		PointsHandler* handler;
		if (all)
			handler = new PointsHandler(*points);
		else
			handler = new BoxPointsHandler(*points, transformation, false);
		pointsHandlers_.push_back(handler);
	}
	else {
		if ( all)
			pointsHandlers_.push_back(new PointsHandler(*this));
		else
			pointsHandlers_.push_back(new BoxPointsHandler(*this, transformation, false));
	}

	return *(pointsHandlers_.back());
  }


void InputData::visit(ValuesCollector& points)
{
	const Transformation& transformation = points.transformation();
	
	points.setCollected(true);
  	 
	if(points.size() <=0 || size() == 0)
	  	return;
	
	if(values_.empty())
		points.setHasValue(false);
		
	for (ValuesCollector::iterator point =  points.begin(); point != points.end(); ++point)
	{
	  	double y=(*point).y(); //lat
	  	double x=(*point).x(); //lon
		
		vector<int> idxV;		
		for(int i=0; i < size(); i++)
		{
			if(fabs(at(i)->y()-y) < points.searchRadiusY() &&
			   fabs(at(i)->x()-x) <  points.searchRadiusX())
			{
			  	idxV.push_back(i);
			}
		}
		
		if(idxV.size() ==0)
			continue;  
		
		double dist=10000000.;
		int minIdx=-1;
		
		//MagLog::debug() << "odb collect idxV : " << lat << " " << lon << " " << idxV.size() << endl;
 		
		for(int i=0; i < idxV.size(); i++)
		{  			
		  	int idx=idxV[i];
			double d;
			if(transformation.coordinateType() == Transformation::GeoType )
			{  
				d=magics::geoDistanceInKm(at(idx)->y(),at(idx)->x(),y,x);
			}
			else
			{
			  	d=(at(idx)->x()-x)*(at(idx)->x()-x) +
			        	 (at(idx)->y()-y)*(at(idx)->y()-y);
			}
			  					
			if(d < dist)
			{
			  	minIdx=idx;
				dist=d;
			}			
		}	
		if(minIdx>=0)  
			(*point).push_back(new ValuesCollectorData(at(minIdx)->x(),
							       at(minIdx)->y(),
							       at(minIdx)->value(),
							       dist));					     			
	}	  
}



