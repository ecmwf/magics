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
	
	MetviewIcon::visit(mdc);  	
}

void NetcdfDecoder::visit(ValuesCollector& values)
{
	(*interpretor_).visit(values,points_);
}


void NetcdfDecoder::visit(TextVisitor& text)
{
	(*interpretor_).visit(text);

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
