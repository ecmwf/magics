/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfGeoMatrixInterpretor.cc
    \brief Implementation of the Template class NetcdfGeoMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#include "NetcdfGeoMatrixInterpretor.h"
#include "Factory.h"
#include "NetcdfData.h"
#include <limits>
#include "Layer.h"

using namespace magics;

NetcdfGeoMatrixInterpretor::NetcdfGeoMatrixInterpretor() 
{}


NetcdfGeoMatrixInterpretor::~NetcdfGeoMatrixInterpretor() 
{}



bool NetcdfGeoMatrixInterpretor::interpretAsMatrix(Matrix** data)
{
	if ( *data ) return false;
	
	Netcdf netcdf(path_, dimension_method_);


	string proj4 = netcdf.getAttribute("projection", string(""));

	if ( proj4.empty() ) {
		matrix_ = new Matrix();
		matrix_->akimaEnabled();
		
	}
	else {
		matrix_ = new Proj4Matrix(proj4);
	}
	*data = matrix_;

   
	



	double missing_value = netcdf.getMissing(field_, missing_attribute_);


	// get the data ...
	try
	{
		map<string, string> first, last;
		setDimensions(dimension_, first, last);

		netcdf.get(longitude_, matrix_->columnsAxis(), first, last);
		netcdf.get(latitude_, matrix_->rowsAxis(), first, last);
		


		matrix_->missing(missing_value);

		if  ( magCompare(primary_index_, "latitude") ) {
			// WE reserve the matrix_ since we are used to lat/lon matrix_!
			vector<double> data;
			netcdf.get(field_, data, first, last);
			int columns =  matrix_->columnsAxis().size();
			int rows =  matrix_->rowsAxis().size();
 			for (int lat = 0; lat < rows; lat++) {
		    	for (int lon = 0; lon < columns; lon++)
			    	matrix_->push_back(data[lat + lon*rows]);
			     }
		}
		else 	{
			vector<double> data;	
			netcdf.get(field_, data, first, last);
			int i = 0;
			for (vector<double>::iterator d = data.begin(); d != data.end(); ++d ) {
				if ( !std::isnan(*d) ) {
					matrix_->push_back(*d);
				}
				else 
					matrix_->push_back(missing_value);
			   i++;
			}
		}

		matrix_->multiply(scaling_);
		matrix_->plus(offset_);

	    matrix_->setMapsAxis();
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
void NetcdfGeoMatrixInterpretor::print(ostream& out)  const
{
	out << "NetcdfGeoMatrixInterpretor[";
	NetcdfInterpretor::print(out);
	NetcdfGeoMatrixInterpretorAttributes::print(out);
	out << "]";
}

bool NetcdfGeoMatrixInterpretor::interpretAsPoints(PointsList& list)
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
		double missing_value =  netcdf.getMissing(field_, missing_attribute_);
		
		netcdf.get(field_, values, first, last);
		netcdf.get(longitude_, longitudes, first, last);
		netcdf.get(latitude_, latitudes, first, last);
		unsigned int val = 0;
		
		for (unsigned int  lat  =0 ; lat < latitudes.size(); lat+=latitude_sample_) {
			for ( unsigned int lon = 0; lon < longitudes.size(); lon+=longitude_sample_) {
				val = (lat* longitudes.size() + lon);
				if ( val >= values.size() ) return true;
				if ( values[val] < suppress_below_ ) continue;
				if ( values[val] > suppress_above_ ) continue;
				if ( same(values[val], missing_value ) ) continue;
				list.push_back(new UserPoint(longitudes[lon], latitudes[lat], (values[val]*scaling_) + offset_));
			}
		}
 		MagLog::dev()<< "everything ok" << endl;
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		return false;
	}

	return true;
}

void NetcdfGeoMatrixInterpretor::statsData(map<string,vector<double> >& stats)
{
	if(matrix_)
	{
	  	for(unsigned int i=0; i < matrix_->size(); i++)
		{  
			if(matrix_->at(i) != matrix_->missing())
			{
			  	stats["value"].push_back(matrix_->at(i));
			}
		}
	}	
}

void NetcdfGeoMatrixInterpretor::visit(MetaDataCollector& mdc)
{
	mdc["_datatype"]="NetCDF_geomatrix";
	mdc["path"]=path_;
	mdc["latitude"]=latitude_;
	mdc["longitude"]=longitude_;
	mdc["value"]=field_;	
	mdc["statsType"]="scalar";
	
	Netcdf nc(path_, dimension_method_);
	
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

void NetcdfGeoMatrixInterpretor::visit(ValuesCollector& vcp,PointsList&)
{	
	vcp.setCollected(true);
	
  	ASSERT(matrix_); 
  	const Transformation& transformation = vcp.transformation();
  	MatrixHandler* box =  transformation.prepareData(*matrix_);
	for (ValuesCollector::iterator point =  vcp.begin(); point != vcp.end(); ++point) {
		point->push_back(new ValuesCollectorData(point->x(),point->y(),box->nearest(point->y(), point->x()),-1.));
	}
}

void NetcdfGeoMatrixInterpretor::customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& out, int thinning)
{
	Netcdf netcdf(path_, dimension_method_);

		// get the data ...
		try
		{
			vector<double> latitudes;
			vector<double> longitudes;
			vector<double> xcomponent;
			vector<double> ycomponent;
			vector<double> colcomponent;
			map<string, string> first, last;
			setDimensions(dimension_, first, last);
			double missing_value = missing(netcdf);

			netcdf.get(x_component_, xcomponent, first, last);
			netcdf.get(y_component_, ycomponent, first, last);
			if ( !colour_component_.empty() )
				netcdf.get(colour_component_, colcomponent, first, last);
			netcdf.get(longitude_, longitudes, first, last);
			netcdf.get(latitude_, latitudes, first, last);
			unsigned int val = 0;

			for (unsigned int  lat  =0 ; lat < latitudes.size(); lat+=thinning) {
				for ( unsigned int lon = 0; lon < longitudes.size(); lon+=thinning) {
					val = (lat* longitudes.size() + lon);
					if ( val >= xcomponent.size() ) return;
					if ( val >= ycomponent.size() ) return;
					if ( !colour_component_.empty() )
						if ( val >= colcomponent.size() ) return;
					if ( same(xcomponent[val], missing_value ) ) continue;
					if ( same(ycomponent[val], missing_value ) ) continue;
					vector<UserPoint> points;
					transformation.populate(longitudes[lon], latitudes[lat], 0, points);
					for (vector<UserPoint>::iterator  pt = points.begin(); pt != points.end(); ++pt ) {
						CustomisedPoint* point = new CustomisedPoint(pt->x_, pt->y_, "");
							(*point)["x_component"] = xcomponent[val];
						(*point)["y_component"] = ycomponent[val];
						if ( !colour_component_.empty() )
							(*point)["colour_component"] = colcomponent[val];
						out.push_back(point);
					}
				}
			}
	 		MagLog::dev()<< "everything ok" << endl;
		}
		catch (MagicsException& e)
		{
			MagLog::error() << e << "\n";
		}
}
