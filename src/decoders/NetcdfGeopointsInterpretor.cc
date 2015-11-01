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

/*! \file NetcdfGeopointsInterpretor.cc
    \brief Implementation of the Template class NetcdfGeopointsInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#include "NetcdfGeopointsInterpretor.h"
#include "TextVisitor.h"
#include "Factory.h"
#include "Netcdf.h"
#include <limits>
#include "Layer.h"
#include "SciMethods.h"

using namespace magics;

NetcdfGeopointsInterpretor::NetcdfGeopointsInterpretor() 
{}


NetcdfGeopointsInterpretor::~NetcdfGeopointsInterpretor() 
{}



static void setDim(Netcdf& netcdf, const string& name, vector<double>& values, map<string, string>& first, map<string, string>& last)
{
    try {
       netcdf.get(name, values, first, last);
    }
    catch (...) {
        int dim = netcdf.getDimension(name);
        for (int i =0; i < dim; ++i) 
            values.push_back(i);
    }
}


bool NetcdfGeopointsInterpretor::interpretAsPoints(PointsList& list, const Transformation& projection) {
    Netcdf netcdf(path_, dimension_method_);
	
	// get the data ...
	try
	{
		vector<double> latitudes;
		vector<double> longitudes;
		vector<double> values;
		map<string, string> first, last;
		setDimensions(dimension_, first, last);
		
		netcdf.get(field_, values, first, last);
		setDim(netcdf, longitude_, longitudes, first, last);
		setDim(netcdf, latitude_, latitudes, first, last);
		
		vector<double>::iterator lat = latitudes.begin();
		vector<double>::iterator lon = longitudes.begin();
		vector<double>::const_iterator val = values.begin();
		
		//If the lat-lon units is specified as "radians" convert lat-lon 
		//to degrees. By default the units are sipposed to be "degrees"
		const char *units = 0;
		if ( magCompare(netcdf.getVariableAttribute(latitude_,"units",units), "radians") )
		{			
			while ( lat!= latitudes.end()) {
			  *lat=DEG(*lat);
			  lat++;
			}
		}
		
		if ( magCompare(netcdf.getVariableAttribute(longitude_,"units",units), "radians") )
		{			
			while ( lon!= longitudes.end()) {
			  *lon=DEG(*lon);
			  lon++;
			}  			
		}		

		lat = latitudes.begin();		
		lon = longitudes.begin();
		while ( lat != latitudes.end() && lon != longitudes.end() && val != values.end() ) {
            UserPoint* geo = new UserPoint(*lon,*lat,*val);
			list.push_back(geo);
			lon++;
			lat++;
			val++;
		}
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		return false;
	}
	return true;
}
bool NetcdfGeopointsInterpretor::interpretAsPoints(PointsList& list)
{
	Netcdf netcdf(path_, dimension_method_);
	
	// get the data ...
	try
	{
		vector<double> latitudes;
		vector<double> longitudes;
		vector<double> values;
		map<string, string> first, last;
		setDimensions(dimension_, first, last);
		
		netcdf.get(field_, values, first, last);
		setDim(netcdf, longitude_, longitudes, first, last);
		setDim(netcdf, latitude_, latitudes, first, last);
		
		vector<double>::iterator lat = latitudes.begin();
		vector<double>::iterator lon = longitudes.begin();
		vector<double>::const_iterator val = values.begin();
		
		//If the lat-lon units is specified as "radians" convert lat-lon 
		//to degrees. By default the units are sipposed to be "degrees"
		const char *units = 0;
		if ( magCompare(netcdf.getVariableAttribute(latitude_,"units",units), "radians") )
		{
			while ( lat!= latitudes.end()) {
			  *lat=DEG(*lat);
			  lat++;
			}
		}
		
		if ( magCompare(netcdf.getVariableAttribute(longitude_,"units",units), "radians") )
		{			
			while ( lon!= longitudes.end()) {
			  *lon=DEG(*lon);
			  lon++;
			} 
		}

		lat = latitudes.begin();		
		lon = longitudes.begin();
		while ( lat != latitudes.end() && lon != longitudes.end() && val != values.end() ) {
			list.push_back(new UserPoint(*lon,*lat,*val));
			lon++;
			lat++;
			val++;
		}
	}    
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		return  false;
	}
	return true;
}

void NetcdfGeopointsInterpretor::visit(MetaDataCollector& mdc)
{
  	mdc["_datatype"]="NetCDF_geopoints";
	mdc["path"]=path_;
	mdc["latitude"]=latitude_;
	mdc["longitude"]=longitude_;
	mdc["value"]=field_;	
	mdc["statsType"]="scalar";		
		
	string attrKey;
	string attrVal;		

	Netcdf nc(path_, dimension_method_);
	//Value attributes
	getAttributes(nc,field_,attrKey,attrVal);		
	if(!attrKey.empty())
	{
		mdc["valueAttrKey"]=attrKey;
		mdc["valueAttrValue"]=attrVal;
	}
}

void NetcdfGeopointsInterpretor::visit(ValuesCollector& vcp,PointsList& points)
{
	if(field_.empty())
	{
	  	vcp.setHasValue(false);
	}
  
  	vcp.setCollected(true);
	

	
	if(points.size() <=0)
	   return;

	for (ValuesCollector::iterator it =  vcp.begin(); it != vcp.end(); ++it)
	{
	  	double y=(*it).y();
	  	double x=(*it).x();
				
		vector<int> idxV;				  
		for(int i=0; i < static_cast<int>(points.size()); i++)
		{		  
		  	if(fabs(points.at(i)->x()-x) < vcp.searchRadiusX() &&
			   fabs(points.at(i)->y()-y) < vcp.searchRadiusY())
			{
			  	idxV.push_back(i);
			}
		}
		
		if(idxV.size() ==0)
			continue;  
		
		double dist=10000000.;
		int minIdx=-1;
		
		for(int i=0; i < idxV.size(); i++)
		{  			
		  	int idx=idxV[i];
			double d=magics::geoDistanceInKm(points.at(idx)->y(),points.at(idx)->x(),y,x);
		
			if(d < dist)
			{
			  	minIdx=idx;
				dist=d;
			}
		}
			
		if(minIdx>=0)
			(*it).push_back(new ValuesCollectorData(points.at(minIdx)->x(),
							       points.at(minIdx)->y(),
							       points.at(minIdx)->value(),
							       dist));					     				
	}
}

/*
 Class information are given to the output-stream.
*/		
void NetcdfGeopointsInterpretor::print(ostream& out)  const
{
	out << "NetcdfGeopointsInterpretor[";
	NetcdfInterpretor::print(out);
	out << "]";
}

NetcdfXYpointsInterpretor::NetcdfXYpointsInterpretor()
{}


NetcdfXYpointsInterpretor::~NetcdfXYpointsInterpretor()
{}




bool NetcdfXYpointsInterpretor::interpretAsPoints(PointsList& list, const Transformation& projection) {
    Netcdf netcdf(path_, dimension_method_);

	// get the data ...
	try
	{
		vector<double> xs;
		vector<double> ys;
		vector<double> values;
		map<string, string> first, last;
		setDimensions(dimension_, first, last);

		if (field_.empty() == false ) {
					netcdf.get(field_, values, first, last);
				}
		setDim(netcdf, x_, xs, first, last);
		setDim(netcdf, y_, ys, first, last);

		double xmissing = netcdf.getMissing(x_, missing_attribute_);
		double ymissing = netcdf.getMissing(y_, missing_attribute_);

		vector<double>::iterator x = xs.begin();
		vector<double>::iterator y = ys.begin();
		vector<double>::const_iterator val = values.begin();

		while ( x != xs.end() && y != ys.end()) {
			double value = 0;
			 if ( val != values.end() ) {
				 value = *val;
				 val++;
			 }
			 if ( !same(*x,xmissing)  ||   !same(*y,ymissing) ) {

            UserPoint*  xy = new UserPoint(*x,*y,value);
            if ( projection.in(*xy) )
			    list.push_back(xy);
			 }
			x++;
			y++;
		}
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		return false;
	}

	return true;
}

bool NetcdfXYpointsInterpretor::interpretAsPoints(PointsList& list)
{
	std::set<string> needs;
	return interpretAsPoints(list, needs);
}
bool NetcdfXYpointsInterpretor::interpretAsPoints(PointsList& list, const std::set<string>& needs)
{
	Netcdf netcdf(path_, dimension_method_);

	// get the data ...
	try
	{
		vector<double> ys;
		vector<double> xs, datex;
		vector<double> values;
		map<string, string> first, last;
		setDimensions(dimension_, first, last);
		if (field_.empty() == false ) {
			netcdf.get(field_, values, first, last);
		}
		setDim(netcdf, x_, xs, first, last);
		setDim(netcdf, y_, ys, first, last);

		double xmissing = netcdf.getMissing(x_, missing_attribute_);
		double ymissing = netcdf.getMissing(y_, missing_attribute_);

		 baseDateX_ = "";
		 if ( !reference_date(netcdf, x_, refDateX_, baseDateX_, xs, datex) )
		        	cf_date(netcdf, x_, refDateX_, baseDateX_, xs, datex);

		vector<double>::iterator y = ys.begin();
		vector<double>::iterator x = xs.begin();
		vector<double>::const_iterator val = values.begin();




		while ( x != xs.end() && y != ys.end()  ) {
			double value = 0;
			if ( val != values.end() ) {
			 	value = *val;
			 	val++;
			}
			if ( !same(*y,ymissing) || !same(*x,xmissing) ) {
				list.push_back(new UserPoint(*x,*y,value, (same(*y,ymissing) || same(*x,xmissing)) ));

			}

			x++;
			y++;

		}
		if ( needs.find("area")  == needs.end() )
				return true;
		vector<double> y2s;
		vector<double> x2s;

		setDim(netcdf, x2_, x2s, first, last);
		setDim(netcdf, y2_, y2s, first, last);

		vector<double>::reverse_iterator x2 = x2s.rbegin();
		vector<double>::reverse_iterator y2 = y2s.rbegin();
		while (  x2 != x2s.rend() && y2 != y2s.rend()  ) {
				list.push_back(new UserPoint(*x2,*y2));
				++x2;
				++y2;
		}

	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		return false;
	}

	return true;
}

/*
 Class information are given to the output-stream.
*/
void NetcdfXYpointsInterpretor::print(ostream& out)  const
{
	out << "NetcdfGeopointsInterpretor[";
	NetcdfInterpretor::print(out);
	out << "]";
}

void NetcdfXYpointsInterpretor::visit(Transformation& transformation)
{
	// get the data ...


	try {
		refDateX_ = ( transformation.getAutomaticX() ) ? "" : transformation.getReferenceX();
		refDateY_ = ( transformation.getAutomaticY() ) ? "" : transformation.getReferenceY();
		PointsList points;
		interpretAsPoints(points);

		if ( transformation.getAutomaticX() ) {
			if ( !this->baseDateX_.empty() ) {
				transformation.setDataMinMaxX(points.minX(), points.maxX(), this->baseDateX_);

			}

			else
			{
				transformation.setMinMaxX(points.minX(), points.maxX());

			}
		}
		if ( transformation.getAutomaticY() ) {
			transformation.setMinMaxY(points.minY(), points.maxY());
		}
	}
	catch ( ... ) {}
}

void NetcdfXYpointsInterpretor::customisedPoints(const Transformation& transformation, const std::set<string>& needs, CustomisedPointsList& out, int)
{
	refDateX_ = transformation.getReferenceX();
    refDateY_ = transformation.getReferenceY();
	PointsList points;
	interpretAsPoints(points, needs);

	for (PointsList::iterator point = points.begin(); point != points.end(); ++point)  {
		CustomisedPoint* pt = new CustomisedPoint();
		out.push_back(pt);
		pt->insert(make_pair("x", (*point)->x()));
		pt->insert(make_pair("y", (*point)->y()));
		pt->missing((*point)->missing());
	}

}

void NetcdfXYpointsInterpretor::visit(MetaDataCollector& mdc)
{
	bool isVprof=false;
  
  	Netcdf nc(path_, dimension_method_);
  	map<string, NetAttribute> attrs = nc.getAttributes();

	//Find out if its vertical profile data
	map<string, NetAttribute>::iterator attrIt=attrs.find("_VIEW");
	if( attrIt !=  attrs.end())
	{  
	  	const char* val;	  
	  	attrIt->second.get(val);
	  	string str;
	  	if(val) str=string(val);
	  
	 	if(str == "MVERTPROFVIEW")
	  		isVprof=true;
	}
    
  	if(!isVprof)
	{  
  		mdc["_datatype"]="NetCDF_xypoints";
		mdc["path"]=path_;
		mdc["x"]=x_;
		mdc["y"]=y_;
		mdc["value"]=field_;	
		mdc["statsType"]="scalar";		
		
		string attrKey;
		string attrVal;		

		//Value attributes
		getAttributes(nc,field_,attrKey,attrVal);		
		if(!attrKey.empty())
		{
		  	mdc["valueAttrKey"]=attrKey;
			mdc["valueAttrValue"]=attrVal;
		}
	}
	else
	{
		mdc["_datatype"]="NetCDF_vprof";
		mdc["description"]="Vertical profile";
		mdc["path"]=path_;
		mdc["x"]=x_;
		mdc["y"]=y_;
		
		// Get the point's co-ordinates
		try
		{
			vector<double> lat,lon;
			nc.get("latitude", lat);
			nc.get("longitude", lon);
			
			if(lat.size() == 1 && lon.size() == 1)
			{
				mdc["latitude"]=tostring(lat[0]);
				mdc["longitude"]=tostring(lon[0]);
			}
		}
		catch ( ... ) {}
		
		mdc["statsType"]="scalar";
		
		string attrKey;
		string attrVal;		

		//X attributes
		getAttributes(nc,x_,attrKey,attrVal);		
		if(!attrKey.empty())
		{
		  	mdc["xAttrKey"]=attrKey;
			mdc["xAttrValue"]=attrVal;
		}
		
		attrKey.clear();
		attrVal.clear();
		
		//Y attributes
		getAttributes(nc,y_,attrKey,attrVal);		
		if(!attrKey.empty())
		{
		  	mdc["yAttrKey"]=attrKey;
			mdc["yAttrValue"]=attrVal;
		}
		
		
		
	}
}

void NetcdfXYpointsInterpretor::visit(ValuesCollector& vcp,PointsList& points)
{
	if(field_.empty())
	{
	  	vcp.setHasValue(false);
	}
  
  	vcp.setCollected(true);
	
	
	
	if(points.size() <=0)
	   return;

	for (ValuesCollector::iterator it =  vcp.begin(); it != vcp.end(); ++it)
	{
	  	double y=(*it).y();
	  	double x=(*it).x();
				
		vector<int> idxV;				  
		for(int i=0; i < static_cast<int>(points.size()); i++)
		{		  
		  	if(fabs(points.at(i)->x()-x) < vcp.searchRadiusX() &&
			   fabs(points.at(i)->y()-y) < vcp.searchRadiusY())
			{
			  	idxV.push_back(i);
			}
		}
		
		if(idxV.size() ==0)
			continue;  
		
		double dist=10000000.;
		int minIdx=-1;
		
		for(int i=0; i < idxV.size(); i++)
		{  			
		  	int idx=idxV[i];
			double d=(points.at(idx)->x()-x)*(points.at(idx)->x()-x) +
			        	 (points.at(idx)->y()-y)*(points.at(idx)->y()-y);
			
			if(d < dist)
			{
			  	minIdx=idx;
				dist=d;
			}
		}
			
		if(minIdx>=0)
			(*it).push_back(new ValuesCollectorData(points.at(minIdx)->x(),
							       points.at(minIdx)->y(),
							       points.at(minIdx)->value(),
							       dist));
					     				
	}
}
void NetcdfXYpointsInterpretor::visit(TextVisitor& title)
{
	vector<string> titles;

	title.titles(titles);



	NetcdfTag tag(*this, title);
	for ( vector<string>::const_iterator t = titles.begin(); t != titles.end(); ++t ) {
		MagLog::debug() << "NetcdfMatrixInterpretor::visit" << *t << endl;
		tag.decode(*t);
	}



}
