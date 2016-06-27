/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GeoPointsDecoder.cc
    \brief Implementation of the Template class GeoPointsDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 12-Dec-2005
    
    Changes:
    
*/



#include "GeoPointsDecoder.h"
#include "SciMethods.h"

using namespace magics;

GeoPointsDecoder::GeoPointsDecoder()
{
}


GeoPointsDecoder::~GeoPointsDecoder()
{
}

/*!
 Class information are given to the output-stream.
*/		
void GeoPointsDecoder::print(ostream& out)  const
{
	out << "GeoPointsDecoder[";
	out << "]";
}

void GeoPointsDecoder::add(const Transformation& transformation, UserPoint& geo)
{
    std::stack<UserPoint>   duplicates;
	transformation.wraparound(geo, duplicates);
	while (duplicates.empty() == false) {
	    push_back(new UserPoint(duplicates.top()));
	    duplicates.pop();
	    stats_["value"].push_back(geo.value());
	 }   	
}

void GeoPointsDecoder::add(const Transformation& transformation, CustomisedPoint& point)
{
	UserPoint geo(point.longitude(), point.latitude());
	std::stack<UserPoint>   duplicates;
	transformation.wraparound(geo, duplicates);

	while (duplicates.empty() == false) {
		UserPoint p = duplicates.top();
		push_back(new UserPoint(p));
		CustomisedPoint*  cp = new CustomisedPoint(p.x(), p.y(), point.identifier());

		for ( CustomisedPoint::iterator key = point.begin(); key != point.end(); ++key)
			cp->insert(make_pair(key->first, key->second));
		customisedPoints_.push_back(cp);
		duplicates.pop();
		
		//Compute speed for stats and histogram
		map<string,double>::const_iterator itX=point.find("x_component");
		map<string,double>::const_iterator itY=point.find("y_component");
		if(itX != point.end() && itY != point.end())
		{	
			double speed=sqrt((itX->second)*(itX->second)+(itY->second)*(itY->second));
			back()->value(speed);
		  	stats_["value"].push_back(speed);
		}	
	}

}

void GeoPointsDecoder::yxdtlv2(const string& line, const Transformation& transformation)
{
	std::istringstream in(line);
	double lat, lon, date, time, level, value;
	in >> lat >> lon >> level >> date >> time >>  value;
	UserPoint geo(lon, lat, value, value == missing_);
	add(transformation, geo);
}

void GeoPointsDecoder::xyv2(const string& line, const Transformation& transformation)
{
	std::istringstream in(line);
	double lat, lon, value;
	in >> lon >> lat >> value;
	UserPoint geo(lon, lat, value, value == missing_);
	add(transformation, geo);
	
}
void GeoPointsDecoder::lluv(const string& line, const Transformation& transformation)
{
	std::istringstream in(line);
	double lat, lon, height, date, time, u, v;
	in >> lat >> lon >> height >> date >> time >> u >> v;
	CustomisedPoint geo(lon, lat, "lluv");
	geo["x_component"] = u;
	geo["y_component"] = v;
	if ( u == missing_ || v == missing_ )
		geo.missing(true);
	add(transformation, geo);

}
void GeoPointsDecoder::polar(const string& line, const Transformation& transformation)
{
	std::istringstream in(line);
	double lat, lon, height, date, time, speed, direction;
	in >> lat >> lon >> height >> date >> time >> speed >> direction;

	CustomisedPoint geo(lon, lat, "polar");

	if ( speed == missing_ || direction == missing_ )
			geo.missing(true);

	else {
		double angle = (90 - (direction))*(PI/180.);
		geo["x_component"] = speed * -cos(angle);
		geo["y_component"] = speed * -sin(angle) ;
	}



	add(transformation, geo);

}
void GeoPointsDecoder::yxdtlv1(const string& line)
{
	std::istringstream in(line);
	double lat, lon, date, time, level, value;
	in >> lat >> lon >> level >> date >> time >>  value;

	push_back(new UserPoint(lon, lat, value, value == missing_));
}

void GeoPointsDecoder::xyv1(const string& line)
{
	std::istringstream in(line);
	double lat, lon, value;
	in >> lon >> lat >> value;
	push_back(new UserPoint(lon, lat, value, value == missing_));
	
}
void GeoPointsDecoder::decode(const Transformation& transformation)
{
	if ( formats_.empty() ) {
		formats_["XYV"] = &GeoPointsDecoder::xyv2;
		formats_["LLV"] = &GeoPointsDecoder::xyv2;
		formats_["XY_VECTOR"] = &GeoPointsDecoder::lluv;
		formats_["POLAR_VECTOR"] = &GeoPointsDecoder::polar;
	}
	
	GeoPointsDecoder::Decode method = &GeoPointsDecoder::yxdtlv2;
	
	if ( !empty() ) return;
	char line[1024];
	try {
		
		ifstream in(path_.c_str());
		while( in.getline( line, sizeof(line) ) ) {
		      if( strncmp( line, "#DATA", 5 ) == 0 ) 
		    	  break;
		      if( strncmp( line, "#FORMAT ", 8 ) == 0 ) {
		    	  const char* fp = line+7;
		    	  while( fp && *fp == ' ' )
		    		  	++fp;
		    	  string format(fp);
		    	  map<string, GeoPointsDecoder::Decode>::iterator m = formats_.find(format);
		    	  method = ( m != formats_.end() ) ? m->second : &GeoPointsDecoder::yxdtlv2;
		      }
		    }

		  while( in.getline( line, sizeof(line) ) ) {
		      if( (*line != '#') && (strlen( line ) > 4) ) {
		    	  (this->*method)(line, transformation);
		      }
		    }

		//computeStats();

		in.close();
	}
	catch (...)
	{
		MagLog::error() << "Geopoints file : can not open " << path_ << endl;
	}
	 
	 
}

void GeoPointsDecoder::customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& list)
{

	customisedPoints_.clear();

	decode(transformation);

	for ( vector<CustomisedPoint*>::iterator point = customisedPoints_.begin(); point != customisedPoints_.end(); ++point)
		list.push_back(*point);



}
void GeoPointsDecoder::decode()
{
	if ( simple_formats_.empty() ) {
		simple_formats_["XYV"] = &GeoPointsDecoder::xyv1;
		simple_formats_["LLV"] = &GeoPointsDecoder::xyv1;
	}
	
	GeoPointsDecoder::SimpleDecode method = &GeoPointsDecoder::yxdtlv1;
	
	if ( !empty() ) return;
	char line[1024];
	try {
		
		ifstream in(path_.c_str());
		while( in.getline( line, sizeof(line) ) ) {
		      if( strncmp( line, "#DATA", 5 ) == 0 ) 
		    	  break;
		      if( strncmp( line, "#FORMAT ", 8 ) == 0 ) {
		    	  const char* fp = line+7;
		    	  while( fp && *fp == ' ' )
		    		  	++fp;
		    	  string format(fp);
		    	  map<string, GeoPointsDecoder::SimpleDecode>::iterator m = simple_formats_.find(format);
		    	  method = ( m != simple_formats_.end() ) ? m->second : &GeoPointsDecoder::yxdtlv1;
		      }
		    }

		  while( in.getline( line, sizeof(line) ) ) {
		      if( (*line != '#') && (strlen( line ) > 4) ) {
		    	  (this->*method)(line);
		      }
		    }

		//computeStats();

		in.close();
	}
	catch (...)
	{
		MagLog::error() << "Geopoints file : can not open " << path_ << endl;
	}
	 
	 
}

void GeoPointsDecoder::initInfo()
{
	setInfo("_datatype","GEOPOINTS");
	setInfo("path",path_);
	setInfo("MV_Format","GEOPOINTS");

}

void GeoPointsDecoder::visit(MetaDataCollector& mdc)
{	
	for(map<string, string>::iterator key = mdc.begin(); key != mdc.end(); ++key )
	{	    
		if(information_.find(key->first) == information_.end() &&
		  mdc.attribute(key->first).group() == MetaDataAttribute::StatsGroup)
		{
			  computeStats();
			  break;
		}
	}
	
	MetviewIcon::visit(mdc);
}  

void GeoPointsDecoder::visit(ValuesCollector& points)
{
	points.setCollected(true);
  	 
	if(points.size() <=0 || size() == 0)
	  	return;
	

	for (ValuesCollector::iterator point =  points.begin(); point != points.end(); ++point)
	{
	  	double lat=(*point).y();
	  	double lon=(*point).x();
		
		vector<int> idxV;		
		for(int i=0; i < size(); i++)
		{
			if(fabs(at(i)->y()-lat) < points.searchRadiusY() &&
			   fabs(at(i)->x()-lon) <  points.searchRadiusX())
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
			double d=magics::geoDistanceInKm(at(idx)->y(),at(idx)->x(),lat,lon);
			
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
