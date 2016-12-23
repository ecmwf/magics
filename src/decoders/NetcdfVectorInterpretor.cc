/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfVectorInterpretor.h
    \brief Implementation of the Template class NetcdfVectorInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#include <limits>

#include "NetcdfVectorInterpretor.h"
#include "Factory.h"
#include "Netcdf.h"
#include "Coordinate.h"
#include "Coordinate.h"


using namespace magics;

NetcdfVectorInterpretor::NetcdfVectorInterpretor() 
{
}


NetcdfVectorInterpretor::~NetcdfVectorInterpretor() 
{
}


void NetcdfVectorInterpretor::customisedPoints(const std::set<string>&, CustomisedPointsList&)
{
	MagLog::dev() << "NetcdfVectorInterpretor::customisedPoints()--> to be implemented!" << endl;
}



/*!
 Class information are given to the output-stream.
*/		
void NetcdfVectorInterpretor::print(ostream& out)  const
{
	out << "NetcdfVectorInterpretor[";
	NetcdfInterpretor::print(out);
	NetcdfVectorInterpretorAttributes::print(out);
	out << "]";
}


NetcdfGeoVectorInterpretor::NetcdfGeoVectorInterpretor() 
{
}


NetcdfGeoVectorInterpretor::~NetcdfGeoVectorInterpretor() 
{
}

void NetcdfVectorInterpretor::customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& list, int thinning)
{
	Netcdf netcdf(path_, dimension_method_);
	try {
			vector<double> x;
			vector<double> y;
			vector<double> x_component;
			vector<double> y_component;
			map<string, string> first, last;
			setDimensions(dimension_, first, last);
			
			netcdf.get(x_component_, x_component, first, last);
			netcdf.get(y_component_, y_component, first, last);
			netcdf.get(x_, x, first, last);
			netcdf.get(y_, y, first, last);
			
			// here we assume that the 4 verctors have the same size
			
			for ( int ind = 0; ind < x.size(); ind += thinning) {
				CustomisedPoint* point = new CustomisedPoint();
				point->longitude(x[ind]);
				point->latitude(y[ind]);
				(*point)["x_component"] = x_component[ind];
				(*point)["y_component"] = y_component[ind];
				list.push_back(point);
			}
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
    }
}

void NetcdfGeoVectorInterpretor::customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& list, int thinning)
{
	Netcdf netcdf(path_, dimension_method_);
	try {
			vector<double> latitudes;
			vector<double> longitudes;
			vector<double> x_component;
			vector<double> y_component;
			map<string, string> first, last;
			setDimensions(dimension_, first, last);
			
			netcdf.get(x_component_, x_component, first, last);
			netcdf.get(y_component_, y_component, first, last);
			netcdf.get(longitude_, longitudes, first, last);
			netcdf.get(latitude_, latitudes, first, last);
			
			vector<double>::iterator lat = latitudes.begin();
			vector<double>::iterator lon = longitudes.begin();

			
			//If the lat-lon units is specified as "radians" convert lat-lon 
			//to degrees. By default the units are sipposed to be "degrees"
			string units;
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

			// here we assume that the 4 verctors have the same size

			for ( int ind = 0; ind < latitudes.size(); ind += thinning) {
				CustomisedPoint* point = new CustomisedPoint();
				point->longitude(longitudes[ind]);
				point->latitude(latitudes[ind]);
				(*point)["x_component"] = x_component[ind];
				(*point)["y_component"] = y_component[ind];
				list.push_back(point);
			}
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}



/*!
 Class information are given to the output-stream.
*/		
void NetcdfGeoVectorInterpretor::print(ostream& out)  const
{
	out << "NetcdfGeoVectorInterpretor[";
	NetcdfInterpretor::print(out);
	NetcdfGeoVectorInterpretorAttributes::print(out);
	out << "]";
}

NetcdfGeoPolarMatrixInterpretor::NetcdfGeoPolarMatrixInterpretor() 
{
}


NetcdfGeoPolarMatrixInterpretor::~NetcdfGeoPolarMatrixInterpretor() 
{
}


void NetcdfGeoPolarMatrixInterpretor::customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& list)
{
	Netcdf netcdf(path_, dimension_method_);
	try {
			vector<double> latitudes;
			vector<double> longitudes;
			vector<double> speed;
			vector<double> direction;
			map<string, string> first, last;
			setDimensions(dimension_, first, last);
			
			netcdf.get(speed_, speed, first, last);
			netcdf.get(direction_, direction, first, last);
			netcdf.get(longitude_, longitudes, first, last);
			netcdf.get(latitude_, latitudes, first, last);
			
			vector<double>::iterator lat = latitudes.begin();
			vector<double>::iterator lon = longitudes.begin();
			vector<double>::const_iterator x = speed.begin();
			vector<double>::const_iterator y = direction.begin();
			
			//If the lat-lon units is specified as "radians" convert lat-lon 
			//to degrees. By default the units are sipposed to be "degrees"
			string units;
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
			/*
			lat = latitudes.begin();		
			lon = longitudes.begin();
			while ( lat != latitudes.end() && lon != longitudes.end() &&
						x != x_component.end() && y != y_component.end() ) {
						CustomisedPoint* point = new CustomisedPoint();		
						point->longitude(*lon);
						point->latitude(*lat);
						(*point)["x_component"] = *x;
						(*point)["y_component"] = *y;
						list.push_back(point);		
						lon++;
						lat++;
						x++;
						y++;
			}
			*/
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}


/*!
 Class information are given to the output-stream.
*/		
void NetcdfGeoPolarMatrixInterpretor::print(ostream& out)  const
{
	out << "NetcdfGeoPolarMatrixInterpretor[";
	NetcdfInterpretor::print(out);
	NetcdfGeoPolarMatrixInterpretorAttributes::print(out);
	out << "]";
}
