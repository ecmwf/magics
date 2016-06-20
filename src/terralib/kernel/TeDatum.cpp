/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include <cstdlib> //FAMI
#include <cstring>
#include <map>

using namespace std;

#include "TeDatum.h"
#include "TeException.h"
#include "TeUtils.h"

const int NUMDATUM = 9; // number of datum
const int NUMSPH   = 8; // number of ellipsoids

bool tokenizeWKT(char** wkt, vector<string>& tokens);

struct SphParam 
{
	double rd;
	double flt;
};

typedef map<string, SphParam> SphMap;

// Spheroid       Radius(Semi Major Axis)        Flattening
const char* ellipsoidList [] = 
{ 
"Airy",	       "6.3775634e+06", "3.340906e-03",	  
"Clarke-1866", "6.378206e+06",	"3.390060e-03",	 
"EarthRadius", "6.371000e+06",	"0.000000e+00",
"Everest",     "6.3772763e+06", "3.324468e-03",
"GRS80",       "6.378137e+06",  "3.352811e-03",
"Hayford", 	   "6.378388e+06",  "3.367003e-03", 	 
"UGGI67", 	   "6.378160e+06",	"3.352892e-03",	 
"WGS84",	   "6.378137e+06",	"3.352811e-03"
};



// DatumName    Spheroid        Shift_X           Shift_Y           Shift_Z

const char* datumList [] = 
{ 
"Astro-Chua",   "Hayford",      "-7.700000e+01",  "2.390000e+02",  "5.000000e+00",
"CorregoAlegre","Hayford",	    "-1.387000e+02",  "1.644000e+02",  "3.440000e+01",
"Indian",	    "Everest",	    "0.000000e+00",   "0.000000e+00",  "0.000000e+00",
"NAD27",	    "Clarke-1866",	"0.000000e+00",   "0.000000e+00",  "0.000000e+00",
"NAD83",	    "GRS80",	    "0.000000e+00",   "0.000000e+00",  "0.000000e+00",
"SAD69", 	    "UGGI67",       "0.000000e+00",   "0.000000e+00",  "0.000000e+00",
"Spherical",	"EarthRadius",  "0.000000e+00",   "0.000000e+00",  "0.000000e+00",
"WGS84",	    "WGS84",	    "6.687000e+01",   "-4.370000e+00", "3.852000e+01",
"Aratu",	    "Hayford",	    "-7.81300e+01",   "2.9163000e+02", "-1.08480e+02"
};

const char** TeGetDatumList()
{
	return datumList;
}


typedef map<string, TeDatum> DatumMap;


TeDatum::TeDatum(const TeDatum& other)
{
	name_ = other.name_;
	rd_ = other.rd_ ;
	flt_= other.flt_;
	dx_ = other.dx_;
	dy_= other.dy_;
	dz_ = other.dz_;
}

TeDatum&
TeDatum::operator=(const TeDatum& rhs)
{
	if ( this != &rhs )
	{
		name_ = rhs.name_;
		rd_ = rhs.rd_;
		flt_= rhs.flt_;
		dx_ = rhs.dx_;
		dy_= rhs.dy_;
		dz_ = rhs.dz_;
	}
	return *this;
}

string
TeDatum::getProj4Description()
{
	double shiftX, shiftY,shiftZ;
	shiftX = shiftY = shiftZ = 0.0;

	string datumProj4 = "";
	if (name_ == "WGS84")
	{
		datumProj4 += " +ellps=WGS84";
		return datumProj4;
	}
	else if (name_ == "Spherical")
	{
		datumProj4 += " +R="  + Te2String(rd_,6);
	}
	else if (name_ == "SAD69")
	{
		shiftX = -66.87;
		shiftY = 4.37;
		shiftZ = -38.52;

		datumProj4 += " +ellps=GRS67";
	}
	else if (name_ == "CorregoAlegre")
	{
		shiftX = -205.57;
		shiftY = 168.77;
		shiftZ = -4.12;
		datumProj4 += " +ellps=intl";
	}
	else
	{
		datumProj4 += " +a="  + Te2String(rd_,6);
		datumProj4 += " +f="  + Te2String(flt_,6);
	}

	if (shiftX != 0.0 && shiftY != 0.0 && shiftZ != 0.0)
		datumProj4 += " +towgs84=" + Te2String(shiftX,4) + "," +  Te2String(shiftY,4)+ "," +  Te2String(shiftZ,4);
	return datumProj4;
}

string
TeDatum::getWKTDescription()
{
	string sph_epsgcode = "";
	string dtm_epsgcode = "";
	string spheroid = "";
	string crs_epsgcode = "";
	string datumWKT = "GEOGCS[\"" + name_ + "\"," + "DATUM[\"" + name_ + "\", SPHEROID[\"";
	if (name_ == "Astro-Chua")
	{
		spheroid = "Hayford";
		sph_epsgcode = "7022";
		dtm_epsgcode = "6224";
		crs_epsgcode = "4224";
	}
	else if (name_ == "CorregoAlegre")
	{
		spheroid = "Hayford";
		sph_epsgcode = "7022";
		dtm_epsgcode = "6225";
		crs_epsgcode = "4225";
	}
	else if (name_ == "Indian")
	{
		spheroid = "Everest";
		sph_epsgcode = "7015";
		dtm_epsgcode = "6239";
		crs_epsgcode = "4240";
	}
	else if (name_ == "NAD27")
	{
		spheroid = "Clarke 1866";
		sph_epsgcode = "7008";
		dtm_epsgcode = "6609";
		crs_epsgcode = "4267";
	}
	else if (name_ == "NAD83")
	{
		spheroid = "GRS80";
		sph_epsgcode = "7019";
		dtm_epsgcode = "6326";
		crs_epsgcode = "4269";
	}
	else if (name_ == "WGS84")
	{
		spheroid = "GRS80";
		sph_epsgcode = "7019";
		dtm_epsgcode = "6326";
		crs_epsgcode = "4326";
	}
	else if (name_ == "SAD69")
	{
		spheroid = "UGGI67";
		sph_epsgcode = "7036";
		dtm_epsgcode = "6291";
		crs_epsgcode = "4618";
	}
	else if (name_ == "Spherical")
	{
		spheroid = "EarthRadius";
		sph_epsgcode = "7035";
	}
	else
		spheroid = "UserDefined";
	datumWKT += spheroid + "\"," + Te2String(rd_,6) + ", " + Te2String(1.0/flt_,6);
	if (!sph_epsgcode.empty())
		datumWKT +=",AUTHORITY[\"EPSG\"," + sph_epsgcode + "]";
	datumWKT += "]";
	double shiftX, shiftY, shiftZ;
	shiftX = shiftY = shiftZ = 0.0;
	if (name_ == "SAD69")
	{
		shiftX = -66.87;
		shiftY = 4.37;
		shiftZ = -38.52;
	}
	else if (name_ == "CorregoAlegre")
	{
		shiftX = -205.57;
		shiftY = 168.77;
		shiftZ = -4.12;
	}
	if (shiftX != 0.0 && shiftY != 0.0 && shiftZ != 0.0)
		datumWKT += ",TOWGS84[" + Te2String(shiftX,4) + "," + Te2String(shiftY,4) + "," + Te2String(shiftZ,4) + ",0.0,0.0,0.0,0.0]";

	if (!dtm_epsgcode.empty())
		datumWKT += ",AUTHORITY[\"EPSG\"," + dtm_epsgcode + "]";
	datumWKT += "]";
	if (!crs_epsgcode.empty())
		datumWKT += ",AUTHORITY[\"EPSG\"," + crs_epsgcode + "]";
	datumWKT += "]";
	return datumWKT;
}

TeDatum
TeDatumFactory::make( const string& name )
{

	DatumMap datumMap_;
	// Step 1 - Read the Spheroids - Put in a Map

	SphMap sphMap;

	SphParam sphPar;

	int i, k = 0;

	for ( i = 0; i < NUMSPH; i++ )
	{
		string name = ellipsoidList [k++];
		sphPar.rd  = atof ( ellipsoidList [k++] );
		sphPar.flt = atof ( ellipsoidList [k++] );

		sphMap [ name ] = sphPar;
	}
	
	// Step 2 - Read the Datum List, create a Map


	k = 0;

	for ( i = 0; i < NUMDATUM; i++ )
	{
			string name = datumList [k++];
			
			string sphname = datumList [ k++];

			double 	dx  = atof ( datumList [k++] );
			double  dy  = atof ( datumList [k++] );
			double  dz  = atof ( datumList [k++] );

			SphMap::iterator it = sphMap.find ( sphname );
			
			if ( it == sphMap.end() )
			{
				throw TeException ( DATUM_ERROR );
			}
			else 
				sphPar = (*it).second;

			TeDatum dat ( name, sphPar.rd, sphPar.flt, dx, dy, dz );

			datumMap_ [ name ] = dat; // create a datum map
	}

	DatumMap::iterator it = datumMap_.find ( name );

	if ( it != datumMap_.end() ) // found it
		return (*it).second;

	return TeDatum ("Spherical");
}

TeDatum 
TeDatumFactory::makeFromEPSGCode(const std::string epsgCode)
{
	string dname;
	if (epsgCode == "6224")
	{
		dname = "Astro-Chua";
	}
	else if (epsgCode == "6225")
	{
		dname = "CorregoAlegre";
	}
	else if (epsgCode == "6240")
	{
		dname = "Indian";
	}
	else if (epsgCode == "6609")
	{
		dname = "NAD27";
	}
	else if (epsgCode == "6152")
	{
		dname = "NAD83";
	}
	else if (epsgCode == "6326")
	{
		dname = "WGS84";
	}
	else if (epsgCode == "6618")
	{
		dname = "SAD69";
	}
	else 
	{
		dname = "Spherical";
	}
	TeDatum dat = TeDatumFactory::make(dname);
	return dat;
}


TeDatum 
TeDatumFactory::makeFromWKT(const std::string wktDatumDescription)
{
	TeDatum dat;
	vector<string> tokens;
	char* wktchar = new char[wktDatumDescription.size()];
	strcpy(wktchar,wktDatumDescription.c_str());
	if (!tokenizeWKT((char **) &wktchar, tokens))
	{
		return dat;
	}
	unsigned int i =0;
	while (i<tokens.size() && tokens[i] != "DATUM")
		++i;

	if (i== tokens.size())
		return dat;
	++i;

	map<string,string> ewktToTeDatum;
	ewktToTeDatum["D_Chua"] = "Astro-Chua";
	ewktToTeDatum["D_Corrego_Alegre"] = "CorregoAlegre";
	ewktToTeDatum["D_Indian_1975"] = "Indian";
	ewktToTeDatum["D_North_American_1927"] = "NAD27";
	ewktToTeDatum["D_North_American_1983"] = "NAD83";
	ewktToTeDatum["D_South_American_1969"] = "SAD69";
	ewktToTeDatum["D_WGS_1984"] = "WGS84";
	ewktToTeDatum["D_Aratu"] = "Aratu";
	// lets the ESRI's WKT format 
	TeTrim(tokens[i]);
	map<string,string>::iterator it = ewktToTeDatum.find(tokens[i]);
	if (it != ewktToTeDatum.end())
	{
		dat = TeDatumFactory::make(it->second);
		return dat;
	}

	map<string,string> wktToTeDatum;
	wktToTeDatum["Chua"] = "Astro-Chua";
	wktToTeDatum["Corrego_Alegre"] = "CorregoAlegre";
	wktToTeDatum["Indian_1975"] = "Indian";
	wktToTeDatum["North_American_Datum_1927"] = "NAD27";
	wktToTeDatum["North_American_Datum_1983"] = "NAD83";
	wktToTeDatum["South_American_Datum_1969"] = "SAD69";
	wktToTeDatum["Unknown"] = "Spherical";
	wktToTeDatum["WGS_1984"] = "WGS84";
	wktToTeDatum["Aratu"] = "Aratu";

	// lets try OGC's SFS WKT format 
	it = wktToTeDatum.find(tokens[i]);
	if (it != wktToTeDatum.end())
	{
		dat = TeDatumFactory::make(it->second);
		return dat;
	}

	// lets try EPSG authority codes 
	unsigned int is = i;
	while (i<tokens.size() && tokens[i] != "AUTHORITY")
		++i;
	if (i== tokens.size())
		return dat;
	++i;
	if (tokens[i]=="EPSG")
	{
		++i;
		dat = TeDatumFactory::makeFromEPSGCode(tokens[i]);
		return dat;
	}

	// so let's try according to the parameters
	i = is;	
	dat.name(tokens[i]);
	while (i<tokens.size() && tokens[i] != "SPHEROID")
		++i;
	if (i== tokens.size())
		return dat;
	++i; ++i;
	dat.radius(atof(tokens[i].c_str()));
	++i;
	dat.flattening(1/atof(tokens[i].c_str()));
	++i;
	if (tokens[i] == "TOWGS84" && i<tokens.size())
	{
		++i;
		double dx = (atof(tokens[i].c_str()));
		++i;
		double dy = (atof(tokens[i].c_str()));
		++i;
		double dz = (atof(tokens[i].c_str()));
		++i;
		++i;
		++i;
		++i;
		if ( dx==0.0 && dy== 0 && dz == 0.0)
		{
			dat = TeDatum("WGS84",6378137,0.003352811,66.87,-4.37,38.52);
		}
		// this is a SAD69
		else if (dx == -66.87 && dy == 4.37 && dz == -38.52)
		{
			dat = TeDatum("SAD69",6378160,0.003352892,0.0,0.0,0.0);
		}
		else
		{
			dat.dx_ = dx;
			dat.dy_ = dy;
			dat.dz_ = dz;
		}
	}
	return dat;
}


bool findDatum(double semiMajor, double flatenning, TeDatum& datum)
{
	DatumMap datumMap_;

	// Step 1 - Read the Spheroids - Put in a Map

	SphMap sphMap;
	SphParam sphPar;

	int i, k = 0;

	for ( i = 0; i < NUMSPH; i++ )
	{
		string name = ellipsoidList [k++];
		sphPar.rd  = atof ( ellipsoidList [k++] );
		sphPar.flt = atof ( ellipsoidList [k++] );

		sphMap [ name ] = sphPar;
	}
	
	// Step 2 - Read the Datum List, create a Map
	k = 0;

	for ( i = 0; i < NUMDATUM; i++ )
	{
		string name = datumList [k++];
		string sphname = datumList [ k++];
		double 	dx  = atof ( datumList [k++] );
		double  dy  = atof ( datumList [k++] );
		double  dz  = atof ( datumList [k++] );

		SphMap::iterator it = sphMap.find ( sphname );
			
		if ( it == sphMap.end() )
		{
			throw TeException ( DATUM_ERROR );
		}
		else 
			sphPar = (*it).second;
		TeDatum dat ( name, sphPar.rd, sphPar.flt, dx, dy, dz );
		datumMap_ [ name ] = dat; // create a datum map
	}

	DatumMap::iterator it = datumMap_.begin();
	while (it != datumMap_.end())
	{
		TeDatum dt = (*it).second;
		if ( TeCompareDouble(dt.radius(),semiMajor,6) &&
			 TeCompareDouble(dt.flattening(),flatenning,6))
		{

			datum = dt;
			return true;
		}
		++it;
	}
	return false;
}
