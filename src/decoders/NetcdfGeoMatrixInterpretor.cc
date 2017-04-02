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
#include "Netcdf.h"
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

	string proj4 = netcdf.getAttribute("projection", "");

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
		vector<double> inlon, outlon;
		vector<double> inlat, outlat;

		
		netcdf.get(longitude_, matrix_->columnsAxis(), first, last);
		netcdf.get(latitude_, matrix_->rowsAxis(), first, last);
		/*
		netcdf.get(longitude_, inlon, first, last);
		netcdf.get(latitude_, inlat, first, last);
		
		projPJ google_ = pj_init_plus("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +nadgrids=@null +units=m +no_defs");
		proj4_ = pj_init_plus(proj4.c_str());
		latlon_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");


		//-20037508.34,-20037508.34,20037508.34,20037508.342361521.9885199675,4639307.212604788,3746560.9408545615,5923449.287616995
//-20037508.34,-20037508.34,20037508.34,20037508.34
		double minx = -20037508.34;
		double maxx = 20037508.34;
		double miny = -20037508.34;
		double maxy = 20037508.34;
		double minx = 0;
		double maxx = 20037508.34;
		double miny = -20037508.34;
		double maxy = 0;

		double stepx = (maxx-minx)/(inlon.size() - 1);
		double stepy = (maxy-miny)/(inlat.size() - 1);

		for (int i = 0; i < inlon.size(); i++)
			matrix_->columnsAxis().push_back(minx + (i*stepx));
		for (int i = 0; i < inlat.size(); i++)
			matrix_->rowsAxis().push_back(miny + (i*stepy));
		*/


		



		matrix_->missing(missing_value);

		if  ( magCompare(primary_index_, "latitude") ) {			
			vector<double> data;
			netcdf.get(field_, data, first, last);
			int columns =  matrix_->columnsAxis().size();
			int rows =  matrix_->rowsAxis().size();
 			for (int lat = 0; lat < rows; lat++) {
		    	for (int lon = 0; lon < columns; lon++)
			    	matrix_->push_back(data[lat + lon*rows]);
			     }
		}
		else {
			vector<double> data;	
			netcdf.get(field_, data, first, last);
/*
			vector<double>::iterator lon = inlon.begin();
			vector<double>::iterator lat = inlat.begin();
*/
			matrix_->reserve(data.size());
			fill(matrix_->begin(), matrix_->end(), missing_value);
			for (vector<double>::iterator d = data.begin(); d != data.end(); ++d ) 
			{

	/*				double x = *lon;
				double y = *lat;
				
				//cout << "avant " << x << "  " << y << endl;
				pj_transform(proj4_, google_, 1, 1, &x, &y, NULL);
				//cout << "apres " << x << "  " << y << endl;
				
				int i = (x-minx)/stepx;
				int j = (y-miny)/stepy;
				//cout << "index " << i << "  " << j << endl;
				int index = (j* inlon.size()) + i;
				++lon; 
				if ( lon == inlon.end() ) {
					++lat;
					lon = inlon.begin();
				}
				if (index < 0 || index > data.size()) continue;
	*/			
				if ( !std::isnan(*d) ) {
					matrix_->push_back(*d);
				}
				else 
					//(*matrix_)[index] =  missing_value;
					matrix_->push_back(missing_value);
				
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


UserPoint* NetcdfGeoMatrixInterpretor::newPoint(const string& proj4, double lon, double lat, double val) 
{
	double x = lon;
	double y = lat;

	if ( !proj4.empty() ) {
		int error = pj_transform(proj4_, latlon_, 1, 1, &x, &y, NULL);    	
    	return new UserPoint(x * RAD_TO_DEG, y *RAD_TO_DEG, val);
    }
    else {
    	return new UserPoint(x, y, val);
    }
    	
}

void NetcdfGeoMatrixInterpretor::visit(Transformation& transformation) {
	
    // Here are in a dump ode .. the coordinates are pixels.
    if ( transformation.getAutomaticX() ) {
        transformation.setMinMaxX(matrix_->columnsAxis().front(),matrix_->columnsAxis().back() );
    }
    if ( transformation.getAutomaticY() ) {
        transformation.setMinMaxY(matrix_->rowsAxis().front(),matrix_->rowsAxis().back());
    }

}


bool NetcdfGeoMatrixInterpretor::interpretAsPoints(PointsList& list)
{
	Netcdf netcdf(path_, dimension_method_);
	string proj4 = netcdf.getAttribute("projection", "");

	if ( !proj4.empty() ) {
		proj4_ = pj_init_plus(proj4.c_str());
		latlon_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");
	}
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
				list.push_back(newPoint(proj4, longitudes[lon], latitudes[lat], (values[val]*scaling_) + offset_));
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
