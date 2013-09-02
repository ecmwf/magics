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

/*! \file XYList.cc
    \brief Implementation of the Template class XYList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 6-May-2004
    
    Changes:
    
*/

#include "XYList.h"





/*!
 Class information are given to the output-stream.
*/		

void XYList::print(ostream& out)  const
{
	out << "XYList[";
    XYListAttributes::print(out);
	out << "]";
}

void  XYList::prepare()
{
    if ( !this->empty() ) return;
    vector<double>::const_iterator i = x_.begin();
    vector<double>::const_iterator j = y_.begin();
    
    while ( i != x_.end() && j != y_.end() ) {
    	this->push_back(new UserPoint(*i, *j, *j));
    	i++;
    	j++;
    }
/*
    vector<double>::const_reverse_iterator i2 = x2_.rbegin();
    vector<double>::const_reverse_iterator j2 = y2_.rbegin();
    
    vector<double>::const_reverse_iterator ei2 = x2_.rend();
    vector<double>::const_reverse_iterator ej2 = y2_.rend();
    while ( i2 != ei2 && j2 != ej2 ) {
    	this->push_back(new UserPoint(*i2, *j2, *j2));
    	i2++;
    	j2++;
    }
*/
}



void XYList::customisedPoints(const std::set<string>& needs, CustomisedPointsList& out)
{
	vector<double>::const_iterator x = x_.begin();
	vector<double>::const_iterator y = y_.begin();
	vector<double>::const_reverse_iterator x2 = x2_.rbegin();
	vector<double>::const_reverse_iterator y2 = y2_.rbegin();
	vector<double>::const_iterator x_upper = x_upper_.begin();
	vector<double>::const_iterator y_upper = y_upper_.begin();
	vector<double>::const_iterator x_lower = x_lower_.begin();
	vector<double>::const_iterator y_lower = y_lower_.begin();
	
	bool bar = (needs.find("bar") != needs.end());
	bool area = (needs.find("area") != needs.end());
	while (  x != x_.end() ) {
		    	CustomisedPoint* point = new CustomisedPoint();

		    	if ( x != x_.end() ) {
		    		if ( same(*x, x_missing_) )
		    			point->missing(true);
		    		(*point)["x"] = *(x++);
		    	}
		    	if ( y != y_.end() ) {
		    		if ( same(*y, y_missing_) )
		    			point->missing(true);
		    		(*point)["y"] = *(y++);
		    	}

		    	if (bar) {
		    		if ( x_lower != x_lower_.end() ) {
		    			if ( same(*x_lower, x_missing_) )
							point->missing(true);
						(*point)["x_lower"] = *(x_lower++);
					}
					if ( y_lower != y_lower_.end() ) {
						if ( same(*y_lower, y_missing_) )
						   point->missing(true);
						(*point)["y_lower"] = *(y_lower++);
					}
					if ( x_upper != x_upper_.end() ) {
						if ( same(*x_upper, x_missing_) )
							point->missing(true);
						(*point)["x_upper"] = *(x_upper++);
					}
					if ( y_upper != y_upper_.end() ) {
						if ( same(*y_upper, y_missing_) )
							   point->missing(true);
						(*point)["y_upper"] = *(y_upper++);
					}
		    	}
		    	out.push_back(point);
		    }

	// now we add the lower values in case one!
	if ( needs.find("area")  == needs.end() )
		return;


	while (  x2 != x2_.rend() && y2 != y2_.rend()  ) {
		CustomisedPoint* point = new CustomisedPoint();
			    		if ( same(*x2, x_missing_) )
			    			point->missing(true);
			    		(*point)["x"] = *(x2++);

			    		if ( same(*y2, y_missing_) )
			    			point->missing(true);
			    		(*point)["y"] = *(y2++);


			    	if (bar) {
			    		if ( x_lower != x_lower_.end() ) {
			    			if ( same(*x_lower, x_missing_) )
								point->missing(true);
							(*point)["x_lower"] = *(x_lower++);
						}
						if ( y_lower != y_lower_.end() ) {
							if ( same(*y_lower, y_missing_) )
							   point->missing(true);
							(*point)["y_lower"] = *(y_lower++);
						}
						if ( x_upper != x_upper_.end() ) {
							if ( same(*x_upper, x_missing_) )
								point->missing(true);
							(*point)["x_upper"] = *(x_upper++);
						}
						if ( y_upper != y_upper_.end() ) {
							if ( same(*y_upper, y_missing_) )
								   point->missing(true);
							(*point)["y_upper"] = *(y_upper++);
						}
			    	}
			    	out.push_back(point);
	}
}


void XYList::customisedPoints(const Transformation&, const std::set<string>& s, CustomisedPointsList& out)
{
  customisedPoints(s, out);
}

double offset(const string& val)
{
	static map<string, double> offsets;
	if ( offsets.empty() ) {
		offsets["hours"] = 3600;
		offsets["hour"] = 3600;
		offsets["days"] = 3600*24;
		offsets["day"] = 3600*24;
		offsets["minute"] = 60;
		offsets["minutes"] = 60;
		offsets["second"] = 1;
	}
	map<string, double>::iterator off = offsets.find(lowerCase(val));
	return ( off !=  offsets.end() ) ?  off->second : 1;
}

void XYList::getReady(const Transformation& transformation)
{
	try {
		if ( transformation.xAxisType() == "date" ) {
		// Fisrt create the x_date if base date and offset are used!
		if ( x_date_.empty() && !x_base_.empty() ) {
			DateTime base(x_base_);
			double off = offset(x_offset_);
			for ( vector<double>::const_iterator x = x_.begin(); x != x_.end(); ++x ) {
				DateTime date(base + Second((*x)*off));
				x_date_.push_back(date);
			}
		}
		if ( !x_date_.empty() ) 
			x_.clear();
		for ( vector<string>::const_iterator x = x_date_.begin(); x != x_date_.end(); ++x )
			x_.push_back(transformation.x(*x));
		}
	}
	catch (...) {}
	try {
		// Fisrt create the x_date if base date and offset are used!
		if ( transformation.yAxisType() == "date" ) {
		if ( y_date_.empty() && !y_base_.empty() ) {
			DateTime base(y_base_);
			double off = offset(y_offset_);
			for ( vector<double>::const_iterator y = y_.begin(); y != y_.end(); ++y ) {
				DateTime date(base + Second((*y)*off));
				y_date_.push_back(date);
			}
		}
		if ( !y_date_.empty() ) 
			y_.clear();
		for ( vector<string>::const_iterator y = y_date_.begin(); y != y_date_.end(); ++y )
			y_.push_back(transformation.y(*y));
		}
	}
	catch (...) {}
	
	try {
		if ( transformation.xAxisType() == "date" ) {
			// Fisrt create the x_date if base date and offset are used!
			if ( x2_date_.empty() && !x2_base_.empty() ) {
				DateTime base(x2_base_);
				double off = offset(x2_offset_);
				for ( vector<double>::const_iterator x = x2_.begin(); x != x2_.end(); ++x ) {
					DateTime date(base + Second((*x)*off));
					x2_date_.push_back(date);
				}
			}
			if ( !x2_date_.empty() )
				x2_.clear();
			for ( vector<string>::const_iterator x = x2_date_.begin(); x != x2_date_.end(); ++x )
				x2_.push_back(transformation.x(*x));
			}
	}
	catch (...) {}
	try {
		// Fisrt create the x_date if base date and offset are used!
				if ( transformation.yAxisType() == "date" ) {
				if ( y2_date_.empty() && !y2_base_.empty() ) {
					DateTime base(y2_base_);
					double off = offset(y2_offset_);
					for ( vector<double>::const_iterator y = y2_.begin(); y != y2_.end(); ++y ) {
						DateTime date(base + Second((*y)*off));
						y2_date_.push_back(date);
					}
				}
				if ( !y2_date_.empty() )
					y2_.clear();
				for ( vector<string>::const_iterator y = y2_date_.begin(); y != y2_date_.end(); ++y )
					y2_.push_back(transformation.y(*y));
				}
	}
	catch (...) {}

	try {
		for ( vector<string>::const_iterator x = x_upper_date_.begin(); x !=x_upper_date_.end(); ++x )
			x_upper_.push_back(transformation.x(*x));
		}
	catch (...) {}
	try {
		for ( vector<string>::const_iterator y = y_upper_date_.begin(); y != y_upper_date_.end(); ++y )
			y_upper_.push_back(transformation.y(*y));
	}
	catch (...) {}

	try {
		for ( vector<string>::const_iterator x = x_lower_date_.begin(); x != x_lower_date_.end(); ++x )
			x_lower_.push_back(transformation.x(*x));
		}
	catch (...) {}
	try {
		for ( vector<string>::const_iterator y = y_lower_date_.begin(); y != y_lower_date_.end(); ++y )
			y_lower_.push_back(transformation.y(*y));
	}
	catch (...) {}
}



void XYList::visit(Transformation& transformation)
{
	// get the data ...
	try {
		this->prepare();

				if ( transformation.getAutomaticX() ) {
					transformation.setMinX(*std::min_element(this->x_.begin(), this->x_.end()));
					transformation.setMaxX(*std::max_element(this->x_.begin(), this->x_.end()));
				}
				if ( transformation.getAutomaticY() ) {
					transformation.setMinY(*std::min_element(this->y_.begin(), this->y_.end()));
					transformation.setMaxY(*std::max_element(this->y_.begin(), this->y_.end()));
				}
			}
			catch ( ... )
			{
			}
}


void XYList::points(const Transformation&, vector<UserPoint>& data)
{
	// here the data are already ready!
	data.reserve(x_.size());
	vector<double>::const_iterator x = x_.begin();
	vector<double>::const_iterator y = y_.begin();
	while (  x != x_.end() && y != y_.end() ) {
		UserPoint point(*x, *y);
		if (*x == this->x_missing_ || *y == this->y_missing_ )
			point.flagMissing();
		data.push_back(point);
	    ++x;
	    ++y;
	}

}
