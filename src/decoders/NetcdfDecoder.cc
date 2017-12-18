/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfDecoder.h
    \brief Implementation of the Template class NetcdfDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/



#include "NetcdfDecoder.h"
#include "Factory.h"
#include "Transformation.h"
#include "MagnifierVisitor.h"
#include "MagJSon.h"

using namespace magics;


NetcdfDecoder::NetcdfDecoder(): data_(0)
{
	setInfo("MV_Format","NetCDF");
}


NetcdfDecoder::~NetcdfDecoder()
{
}


/*!
 Class information are given to the output-stream.
*/	

void NetcdfDecoder::print(ostream& out)  const
{
	out << "NetcdfDecoder[";
	NetcdfDecoderAttributes::print(out);
	out << "]";
}

void NetcdfDecoder::visit(MagnifierVisitor& magnify)
{
	try {
	vector<PaperPoint> thin;
	vector<PaperPoint> all;
	const Transformation& transformation = magnify.transformation();
	
	
	transformation.thin(matrix(), thin, all);
	
	for (vector<PaperPoint>::iterator point = thin.begin(); point != thin.end(); ++point) {
		 magnify.add(*point);
	}
	for (vector<PaperPoint>::iterator point = all.begin(); point != all.end(); ++point) {
			 magnify.addMore(*point);
	}
	}
	catch (...) {}
	
		 
	
	 
}

void NetcdfDecoder::getInfo(map<string, string>& infos)
{
	ParamJSon data(metadata_);

	for (map<string, string>::iterator info = infos.begin(); info != infos.end(); ++info)
		info->second = data.get(info->first, info->second);

}
void NetcdfDecoder::visit(MetaDataCollector& mdc)
{
	bool interpretorCalled=false;
	for(map<string, string>::iterator key = mdc.begin(); key != mdc.end(); ++key )
	{	    
		if(information_.find(key->first) == information_.end() && !interpretorCalled)
		{
			 MetaDataCollector mdcInt;
			 (*interpretor_).visit(mdcInt);
			 for(map<string, string>::iterator keyInt = mdcInt.begin(); keyInt != mdcInt.end(); ++keyInt )
			 {	
				setInfo(keyInt->first,keyInt->second);
			 }
			 interpretorCalled=true;
		}
	}
	
	for(map<string, string>::iterator key = mdc.begin(); key != mdc.end(); ++key )
	{	
		if(information_.find(key->first) == information_.end() &&
		   mdc.attribute(key->first).group() == MetaDataAttribute::StatsGroup)
		{
			  (*interpretor_).statsData(stats_);
			  computeStats();
			  break;
		}
	}
	
	(*interpretor_).visit(mdc);

	MetviewIcon::visit(mdc);  	
}

void NetcdfDecoder::visit(ValuesCollector& values)
{
	try {
		(*interpretor_).visit(values,points_);
	}
	catch (...) {
		valid_ = false;
	}
}


void NetcdfDecoder::visit(TextVisitor& text)
{
	try {
		(*interpretor_).visit(text);
	}
	catch (...) {
		valid_ = false;
	}

}

PointsHandler&  NetcdfDecoder::points(const Transformation& transformation, bool all)
{
	  PointsList points;
	  valid_ = (*interpretor_).interpretAsPoints(points, transformation);

	  for ( PointsList::iterator point = points.begin(); point != points.end(); ++point ) {
		  	  stack<UserPoint> pts;
		  	  transformation.wraparound(**point, pts);
		  	  while ( pts.empty() == false ) {
		  		UserPoint pt = pts.top();
		  		pts.pop();
		  		if ( pt.missing() == false || ( pt.missing() && all) )
		  			points_.push_back(new UserPoint(pt));
		  	}

	  }

	  this->pointsHandlers_.push_back(new PointsHandler(points_));
	  return *(this->pointsHandlers_.back());

}
