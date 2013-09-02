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

#include "TeProjection.h"
#include "TeException.h"
#include "TeUtils.h"
#include <cstdlib>  //FAMI
#include <cstring>
#include <stdio.h>
#include <iostream> //FAMI

bool tokenizeWKT(char** wkt, vector<string>& tokens);

TeDatum  TeGetDatumFromWKT(const string& wkt);

TeDatum  TeGetDatumFromProj4(const string& proj4);

//=========================================================
//
//  PROJECTION INFORMATION 
//
//  Auxiliary functions which indicate the information
//  associated to a projection
//
//  Used for reading and writing information
//
//==========================================================


//FAMI: added Satellite
// Name						Units  Long  Lat  Par1  Par2  Sca  Eas  Nor 
const char* teProjInfo[]= {
"Albers",					"1",  "1",  "1", "1",  "1",  "0", "1", "1", 
"LatLong",					"1",  "0",  "0", "0",  "0",  "0", "0", "0",   
"LambertConformal",			        "1",  "1",  "1", "1",  "1",  "0", "1", "1", 
"Mercator",					"1",  "1",  "1", "1",  "0",  "0", "1", "1", 
"Miller",					"1",  "1",  "0", "0",  "0",  "0", "1", "1", 
"UTM",						"1",  "1",  "0", "0",  "0",  "1", "1", "1", 
"Polyconic",				        "1",  "1",  "1", "0",  "0",  "0", "1", "1",
"Sinusoidal",				        "1",  "1",  "0", "0",  "0",  "0", "1", "1",
"CylindricalEquidistant",	                "1",  "1",  "0", "1",  "0",  "0", "1", "1",
"PolarStereographic",		                "1",  "1",  "0", "0",  "0",  "0", "1", "1",
"Satellite",		                        "1",  "1",  "1", "0",  "0",  "0", "1", "1",
"NoProjection",				        "1",  "0",  "0", "0",  "0",  "0", "0", "0"		
};


const char** TeGetProjInfo()
{
	return teProjInfo;
}

TeProjInfo
TeProjectionInfo ( const string& projName  )
{

	TeProjInfoMap pjMap;
	TeProjInfo pjInfo;

	int k = 0;

	for ( int i = 0; i < NUM_PROJ; i++ )
	{
		string name = teProjInfo [k++];
			
	    pjInfo.hasUnits  = atoi ( teProjInfo [k++] );
	    pjInfo.hasLon0   = atoi ( teProjInfo [k++] );
	    pjInfo.hasLat0   = atoi ( teProjInfo [k++] );
	    pjInfo.hasStlat1 = atoi ( teProjInfo [k++] );
		pjInfo.hasStlat2 = atoi ( teProjInfo [k++] );
		pjInfo.hasScale  = atoi ( teProjInfo [k++] );
		pjInfo.hasOffx   = atoi ( teProjInfo [k++] );
	    pjInfo.hasOffy   = atoi ( teProjInfo [k++] );

		pjMap [ name ] = pjInfo;
	}


	TeProjInfoMap::iterator it = pjMap.find ( projName );

	if ( it == pjMap.end() )
		throw TeException ( PROJECTION_NOT_AVAILABLE );

return (*it).second;
}

//========================================================
//
// PROJECTION FACTORY
//
// =======================================================

TeProjection*
TeProjectionFactory::make ( const TeProjectionParams& par )
{
	string punits;
	if (par.units.empty())
		punits = "Meters";
	else
		punits = par.units;

	if ( par.name == "UTM" )
		return new TeUtm ( par.datum, par.lon0, par.lat0,
		par.offx, par.offy, punits, par.scale, par.hemisphere );

	if ( par.name == "LambertConformal")
		return new TeLambertConformal ( par.datum, par.lon0, par.lat0,
		par.offx, par.offy, par.stlat1, par.stlat2, punits );
	
	if ( par.name == "Albers" )
		return new TeAlbers ( par.datum, par.lon0, par.lat0,
		par.offx, par.offy, par.stlat1, par.stlat2, punits );

	if ( par.name == "Miller")
		return new TeMiller ( par.datum, par.lon0, par.offx, par.offy, punits );

	if ( par.name == "LatLong" )
	{
		if (par.units.empty())
			punits = "DecimalDegrees";
		else
			punits = par.units;
		return new TeLatLong ( par.datum, punits );
	}

	if ( par.name == "Polyconic" )
		return new TePolyconic ( par.datum, par.lon0, par.lat0, par.offx, par.offy, punits); 

	if ( par.name == "Mercator" )
		return new TeMercator ( par.datum, par.lon0, par.lat0, 
		par.offx, par.offy, par.stlat1, punits );

	if ( par.name == "Sinusoidal")
		return new TeSinusoidal ( par.datum, par.lon0, par.offx, par.offy, punits );

	if ( par.name == "CylindricalEquidistant")
		return new TeCylindricalEquidistant ( par.datum, par.lon0, par.offx, par.offy, par.stlat1, punits);

	if ( par.name == "PolarStereographic")
		return new TePolarStereographic ( par.datum, par.lon0, par.offx, par.offy,punits, par.hemisphere );

//FAMI
	if ( par.name == "Satellite")
		return new TeSatelliteProjection ( par.datum, par.offx, par.offy, par.pri, par.prj, par.pis, par.pjs, par.lat0, par.lon0, par.prs, par.pscn, par.pyaw);

	if ( par.name == "NoProjection")
		return new TeNoProjection(par.datum,punits);	
	return 0;
}

TeProjectionParams 
TeProjection::params () const
{
	TeProjectionParams par;
	
	par.name  = GPname;
	par.datum = GPdatum;
	par.lon0  = GPlon0;
	par.lat0  = GPlat0;
	par.offx  = GPoffx;
	par.offy  = GPoffy;
	par.stlat1 = GPstlat1;
	par.stlat2 = GPstlat2;
	par.units  = GPunits;
	par.scale  = GPscale;
	par.hemisphere =  GPhemisphere;

return par;
}


TeProjection::TeProjection(const TeProjection& rhs)
{
	GPname  = rhs.GPname;
	GPdatum = rhs.GPdatum;
	GPlon0  = rhs.GPlon0;
	GPlat0  = rhs.GPlat0;
	GPoffx  = rhs.GPoffx;
	GPoffy  = rhs.GPoffy;
	GPstlat1 = rhs.GPstlat1;
	GPstlat2 = rhs.GPstlat2;
	GPunits  = rhs.GPunits;
	GPscale  = rhs.GPscale;
	GPhemisphere = rhs.GPhemisphere;
}

TeProjection& 
TeProjection::operator=(const TeProjection& rhs)
{
	if ( this != &rhs )
	{
		GPname  = rhs.GPname;
		GPdatum = rhs.GPdatum;
		GPlon0  = rhs.GPlon0;
		GPlat0  = rhs.GPlat0;
		GPoffx  = rhs.GPoffx;
		GPoffy  = rhs.GPoffy;
		GPstlat1 = rhs.GPstlat1;
		GPstlat2 = rhs.GPstlat2;
		GPunits  = rhs.GPunits;
		GPscale  = rhs.GPscale;
		GPhemisphere = rhs.GPhemisphere;
	}
	return *this;
}



/*******************************************************************
	CHECKS IF A PROJECTION INSTANCE IS EQUAL TO ANOTHER
********************************************************************/

bool
TeProjection::operator== (const TeProjection& proj)
{
	if (GPname == "NoProjection" && proj.GPname == "NoProjection")
		return true;
	if (GPname  != proj.GPname)
		return false;
	if (!(GPdatum==proj.GPdatum))
		return false;
	if (!TeFPEquals(GPlon0, proj.GPlon0, 0.0000000001))
		return false;
	if (!TeFPEquals(GPlat0, proj.GPlat0, 0.0000000001))
		return false;
	if (!TeFPEquals(GPoffx,proj.GPoffx,0.001))
		return false;
	if (!TeFPEquals(GPoffy,proj.GPoffy,0.001))
		return false;
	if (!TeFPEquals(GPstlat1,proj.GPstlat1,0.0000000001))
		return false;
	if (!TeFPEquals(GPstlat2,proj.GPstlat2,0.0000000001))
		return false;
	if (GPhemisphere!=proj.GPhemisphere)
		return false;
	if (GPunits != proj.GPunits)
		return false;
	if (!TeFPEquals(GPscale,proj.GPscale, 0.0000000001))
		return false;

	return true;
}

/*******************************************************************
	PRINTS INFORMATION ABOUT A PROJECTION
********************************************************************/


void
TeProjection::print ( FILE* file_ )
{
	TeProjInfo pjInfo = TeProjectionInfo ( GPname );

	fprintf(file_,"%s\n", "// Projection Information" );
	fprintf ( file_, "%s %s \n", "PROJECTION", GPname.c_str() );
	fprintf ( file_, "%s %s \n", "DATUM", GPdatum.name().c_str() );
	fprintf ( file_, "%s %s \n", "UNITS", GPunits.c_str() );
	
	if ( pjInfo.hasLon0 )
		fprintf ( file_, "%s %17.6f \n", "ORIGIN LONG", GPlon0 );

	if ( pjInfo.hasLat0 )
		fprintf ( file_, "%s %17.6f \n", "ORIGIN LAT", GPlat0 );

	if ( pjInfo.hasOffx )
		fprintf ( file_, "%s %17.6f \n", "FALSE EASTING", GPoffx );

	if ( pjInfo.hasOffy )
		fprintf ( file_, "%s %17.6f \n", "FALSE NORTHING", GPoffy );
	
	if ( pjInfo.hasStlat1 )
		fprintf ( file_, "%s %17.6f \n", "FIRST STANDARD PARALEL", GPstlat1 );
	
	if ( pjInfo.hasStlat2 )
		fprintf ( file_, "%s %17.6f \n", "SECOND STANDARD PARALEL", GPstlat2 );

	if ( pjInfo.hasScale )
		fprintf ( file_, "%s %17.6f \n", "SCALE", GPscale );

	fprintf(file_,"%s\n", "// End of Projection Information" );
}


string
TeProjection::describe ()
{
	string desc;
	if (GPname == "NoProjection")
	{
		desc = "NoProjection";
		return desc;
	}
	desc = GPname;
	TeProjInfo pjInfo = TeProjectionInfo ( GPname );

	desc =  GPunits;
	if ( pjInfo.hasLon0 )
		desc += "," + Te2String(GPlon0*TeCRD,6);

	if ( pjInfo.hasLat0 )
		desc += "," + Te2String(GPlat0*TeCRD,6);

	if ( pjInfo.hasStlat1 )
		desc += ", " + Te2String(GPstlat1*TeCRD,6);
	
	if ( pjInfo.hasStlat2 )
		desc += "," + Te2String(GPstlat2*TeCRD,6);

	if ( pjInfo.hasOffx )
		desc += "," + Te2String(GPoffx,6);

	if ( pjInfo.hasOffy )
		desc += "," + Te2String(GPoffy,6);
	
	if ( pjInfo.hasScale )
		desc += "," + Te2String(GPscale,6);

	return desc;
}


bool decodifyDescription(const string& projDescription, TeProjectionParams& pars)
{
	vector<string> projDesc;
	if (TeSplitString(projDescription, ",", projDesc) <= 0)
		return false;
	unsigned int npar = projDesc.size();
	TeProjInfo pjInfo = TeProjectionInfo (projDesc[0]);
	pars.name = projDesc[0];
	if (npar > 1)
		pars.units = projDesc[1];
	else 
		return true;

	unsigned int nextp = 2;
	if (pjInfo.hasLon0)
	{
		 if (nextp < npar)
		 {
			pars.lon0 = atof(projDesc[nextp].c_str()) * TeCDR;
			nextp++;
		 }
		 else
			 return false;
	}

	if (pjInfo.hasLat0)
	{
		 if (nextp < npar)
		 {
			pars.lat0 = atof(projDesc[nextp].c_str()) * TeCDR;
			nextp++;
		 }
		 else
			 return false;
	}

	if (pjInfo.hasStlat1)
	{
		 if (nextp < npar)
		 {
			pars.stlat1 = atof(projDesc[nextp].c_str()) * TeCDR;
			nextp++;
		 }
		 else
			 return false;
	}

	if (pjInfo.hasStlat2)
	{
		 if (nextp < npar)
		 {
			pars.stlat2 = atof(projDesc[nextp].c_str()) * TeCDR;
			nextp++;
		 }
		 else
			 return false;
	}

	if (pjInfo.hasOffx)
	{
		 if (nextp < npar)
		 {
			pars.offx = atof(projDesc[nextp].c_str());
			nextp++;
		 }
		 else
			 return false;
	}

	if (pjInfo.hasOffy)
	{
		 if (nextp < npar)
		 {
			pars.offy = atof(projDesc[nextp].c_str());
			nextp++;
		 }
		 else
			 return false;
	}
	
	if (pjInfo.hasScale)
	{
		 if (nextp < npar)
		 {
			pars.scale = atof(projDesc[nextp].c_str());
			nextp++;
		 }
		 else
			 return false;
	}
	return true;
}

/********************************************************************
		Planimetric datum transformation 
********************************************************************/
void
TeProjection :: ChangeLL (double &lon1, double &lat1)		
{
	double 	equad1,	// Squared eccentricity - datum 1
			equad2,	// Squared eccentricity - datum 2
			n1,		// Great normal of ellipsoid - datum 1
			n2,		// Great normal od ellipsoid - datum 2
			x1,		// Geocentric cartesian coordinates - datum 1 
			y1,
			z1,
			x2,		// Geocentric cartesian coordinates - datum 2
			y2,
			z2,
			d,lat2,		// Ancillary variable
			lon2;
	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

 	if( GPdestination->GPdatum.radius() == 0.) return;

	int	flt1 = (int)(Pflt*1000000000.);
	int	flt2 = (int)(GPdestination->GPdatum.flattening()*1000000000.);
	if (Prd == GPdestination->GPdatum.radius() && flt1==flt2)
		return;

// Geocentric cartesian coordinates calculation - datum 1
	equad1 = 2.*Pflt;
	equad1 -= Pflt*Pflt;
	double a1,a2,a3;
	a1 = sin(lat1);
	a1 *= a1;
	a1 *= equad1;
	a2 = 1-a1;
	n1 = Prd/sqrt(a2);
	x1 = n1*cos(lat1)*cos(lon1);
	y1 = n1*cos(lat1)*sin(lon1);
	z1 = (n1*(1-equad1))*sin(lat1);

// Geocentric cartesian coordinates calculation - datum 2
	if (GPdatum.xShift() == TeMAXFLOAT || GPdestination->GPdatum.xShift() == TeMAXFLOAT)
	{
		x2 = x1; 
		y2 = y1;
		z2 = z1;
	}
	else
	{	
		x2 = x1 + (GPdatum.xShift() - GPdestination->GPdatum.xShift());
		y2 = y1 + (GPdatum.yShift() - GPdestination->GPdatum.yShift());
		z2 = z1 + (GPdatum.zShift() - GPdestination->GPdatum.zShift());
	}

// Geodetic coordinates calculation - datum 2
	equad2 = 2.*GPdestination->GPdatum.flattening();
	equad2 -= (GPdestination->GPdatum.flattening())*(GPdestination->GPdatum.flattening());
	lat2 = lat1;
	do
	{
		a1 = sin(lat2);
		a1 *= a1;
		a1 *= equad2;
		a2 = 1-a1;
		n2 = GPdestination->GPdatum.radius()/sqrt(a2);
		a1 = equad2*sin(lat2);
		a1 *= n2;
		a1 += z2;
		a2 = x2*x2;
		a2 += (y2*y2);
		a3 = sqrt(a2);
		lat2 = atan2(a1,a3);
		a1 = sin(lat2);
		a1 *= a1;
		a1 *= equad2;
		a2 = 1-a1;
		a3 = sqrt(a2);
		d = (GPdestination->GPdatum.radius()/a3)-n2;
	}
	while (fabs(d) > 0.0000001);
	lon2 = atan2(y2,x2);
	lat1 = lat2;
	lon1 = lon2;
}

/********************************************************************
		GEODETIC TO UTM COORDINATES
********************************************************************/


TeCoord2D
TeUtm :: LL2PC (TeCoord2D& ptll)
{

	double	k0,		// Scale factor
		equad,		// Squared eccentricity
		n,		// Great normal of ellipsoid
		elinquad,	// ancillary variables 
		aux1,aux2,aux3,aux4,aux5,aux6,aux7,aux8,aux9,
		aux10,aux11,aux12,aux13,t,c,ag,m,ptllx,ptlly;

	double Pflt = GPdatum.flattening();
	ptllx = ptll.x();
	ptlly = ptll.y();
	k0 = 1. - (1./2500.);
	equad = 2.*Pflt;	
	equad -= Pflt*Pflt;
	elinquad = equad/(1. - equad);
	aux1 = equad*equad;
	aux2 = aux1*equad;
	aux3 = sin((double)2*ptlly);
	aux4 = sin((double)4*ptlly);
	aux5 = sin((double)6*ptlly);

	double a1, a2, a3;
	a1 = equad/4.;
	a2 = 3.*aux1/64.;
	a3 = 5.*aux2/256.;
	aux6 = (1-a1-a2-a3)*ptlly;

	a1 = 3.*equad/8.;
	a2 = 3.*aux1/32.;
	a3 = 45.*aux2/1024.;
	aux7 = (a1+a2+a3)*aux3;

	a1 = 15.*aux1/256.;
	a2 = 45.*aux2/1024.;
	aux8 = (a1+a2)*aux4;

	aux9 = 35.*aux2;
	aux9 /= 3072.;
	aux9 *= aux5;

	a1 = sin(ptlly);	
	a1 *= a1;		
	a1 *= equad;		
	n = GPdatum.radius()/sqrt((double)1-a1);


	t = tan(ptlly);		
	t *= t;			

	c = cos(ptlly);		
	c *= c;			
	c *= elinquad;		
	ag = (ptllx-GPlon0)*cos(ptlly);
	m = GPdatum.radius()*(aux6 - aux7 + aux8 - aux9);


	a1 = ag*ag*ag;
	aux10 = (1.-t+c)*a1/6.;

	a1 = 5.-(18.*t);
	a2 = a1+t*t;
	a1 = a2+72.*c;
	a2 = a1-(58.*elinquad);
	a1 = ag*ag*ag;		
	a1 *= ag;		
	a1 *= ag;		
	aux11 = a2*a1/120.;	

	a1 = 5.-t+9.*c;
	a2 = 4.*c*c;
	a3 = ag*ag;
	a3 *= ag;
	a3 *= ag;
	aux12 = (a1+a2)*a3/24.;

	a1 = 61.-(58.*t) ;
	a2 = a1+(t*t);
	a1 = a2+(600.*c);
	a2 = a1-(330.*elinquad);
	a1 = ag*ag*ag;		
	a1 *= ag;		
	a1 *= ag;		
	a1 *= ag;		
	aux13 = a2*a1/720.;	

	ptllx = k0*n*(ag + aux10 + aux11);

	a1 = ag*ag/2;
	a2 = a1+aux12+aux13;
	a3 = n*tan(ptlly);
	a1 = a2*a3;
	ptlly = k0*(m+a1);

	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));
}


/********************************************************************
		UTM TO GEODETIC COORDINATES
********************************************************************/

TeCoord2D
TeUtm :: PC2LL (TeCoord2D& ptpc)
{
	double  k0,             	// Scale factor
		equad,			// Squared eccentricity
		n1,			// Great normal of ellipsoid
		elinquad,		// Ancillary variables
		e1,aux1,aux2,aux3,aux4,aux5,m,mi,aux6,aux7,aux8,lat1,
		c1,t1, quoc,r1,d,aux9,aux10,aux11,aux12,ptpcx,ptpcy;

	double Prd  = GPdatum.radius();
	double Pflt = GPdatum.flattening();
	
	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;		

	k0 = 1. - (1./2500.);
	equad = 2.*Pflt;		
	equad -= Pflt*Pflt;		
	elinquad = equad/(1. - equad);

	double a1,a2,a3;
	a1 = sqrt((double)1-equad);
	e1 = (1.-a1)/(1.+a1);

	aux1 = equad*equad;
	aux2 = aux1*equad;
	aux3 = e1*e1;
	aux4 = e1*aux3;
	aux5 = aux4*e1;

	m = ptpcy/k0;


	a1 = 1.-(equad/4.);
	a2 = 3.*(aux1/64.);
	a3 = 5.*(aux2/256.);
	mi = m/(Prd*(a1-a2-a3));


	a1 = 3.*(e1/2.);
	a2 = 27.*(aux4/32.);
	a3 = sin((double)2*mi);
	aux6 = (a1-a2)*a3;

	a1 = 21.*(aux3/16.);
	a2 = 55.*(aux5/32.);
	a3 = sin((double)4*mi);
	aux7 = (a1-a2)*a3;

	a1 = 151.*(aux4/96.);
	a2 = sin((double)6*mi);
	aux8 = a1*a2;

	lat1 = mi + aux6 + aux7 + aux8;

	c1 = cos(lat1);			
	c1 *= c1;			
	c1 *= elinquad;			

	t1 = tan(lat1);			
	t1 *= t1;			

	a1 = sin(lat1);			
	a1 *= a1;			
	a1 *= equad;			
	a2 = 1.-a1;
	n1 = Prd/sqrt(a2);

	a1 = sin(lat1);
	a1 *= a1;
	a1 *= equad;
	a2 = 1.-a1;
	quoc = a2*a2*a2;

	r1 = Prd*(1.-equad);
	r1 /= sqrt(quoc);

	d = ptpcx;
	d /= (n1*k0);


	a1 = 5.+(3.*t1);
	a1 += (10.*c1);
	a1 += (-4.*(c1*c1));
	a1 += (-9.*elinquad);
	a1 *= d;
	a1 *= d;
	a1 *= d;
	a1 *= d;
	aux9 = a1/24.;

	a1 = 61.+(90.*t1);
	a1 += (298.*c1);
	a1 += (45.*(t1*t1));
	a1 += (-252.*elinquad);
	a1 += (-3.*c1*c1);
	aux10 = d*d*d;			
	aux10 *= d;			
	aux10 *= d;			
	aux10 *= d;			
	aux10 *= a1;
	aux10 /= 720.;

	a1 = d*d*d;
	a1 /= 6.;
	a2 = 2.*t1;
	a2 += (1.+c1);
	aux11 = d-a2*a1;

	a1 = 5.-(2.*c1);
	a1 += (28.*t1);
	a1 += (-3.*c1*c1);
	a1 += (8.*elinquad);
	a1 += (24.*t1*t1);
	aux12 = d*d*d;			
	aux12 *= d;			
	aux12 *= d;			
	aux12 *= a1;			
	aux12 /= 120.;			

	a1 = d*d/2.;
	a1 += (- aux9 + aux10);
	a2 = tan(lat1)/r1;
	a2 *= n1;
	ptpcy = lat1-a2*a1;

	ptpcx= GPlon0 + (aux11 + aux12)/cos(lat1);

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return(TeCoord2D(ptpcx,ptpcy));
}

/********************************************************************
		GEODETIC TO MERCATOR COORDINATES
********************************************************************/
TeCoord2D
TeMercator :: LL2PC (TeCoord2D& ptll)
{
	double	equad,			//Squared eccentricity
		aux1,			// Ancillary variables
		aux2,aux3,aux4,aux5,aux6,ptllx,ptlly;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptllx = ptll.x();		
	ptlly = ptll.y();		

	equad = 2.*Pflt;		
	equad -= Pflt*Pflt;		
	double	a1,a2,a3;		
 
	a1 = tan(ptlly/(double)2);	
	a2 = 1.+a1;			
	a3 = 1.-a1;			
	aux1 = a2/a3;			
 
	a1 = equad*equad/4.;		
	a1 += equad;			
	a2 = equad*equad*equad/8.;	
	a3 = sin(ptlly);		
	aux2 = (a1+a2)*a3;		
 
	a1 = equad*equad/12.;		
	a2 = equad*equad*equad/16.;	
	a3 = sin((double)3*ptlly);	
	aux3 = (a1+a2)*a3;		
 
	a1 = equad*equad*equad/80.;	
	a2 = sin((double)5*ptlly);	
	aux4 = a1*a2;			
	aux5 = cos(GPstlat1);
 
	a1 = sin(GPstlat1);		
	a1 *= a1;			
	a1 *= equad;			
	a2 = sqrt((double)1-a1);	
	aux6 = 1./a2;			

	ptllx = Prd*(ptllx - GPlon0)*aux5*aux6;
	ptlly = Prd*(log(aux1) - aux2 + aux3 - aux4)*aux5*aux6;

	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}

/********************************************************************
		MERCATOR TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeMercator :: PC2LL (TeCoord2D& ptpc)
{
	double	equad,			//Squared eccentricity 
	        pi,			// PII value
		t,			// Ancillary variables
		xx,aux1,aux2,aux3,aux4,aux5,ptpcx,ptpcy;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	equad = 2.*Pflt;		
	equad -= Pflt*Pflt;		
	pi = 4.*atan((double)1);
	double	a1,a2;		
	aux1 = cos(GPstlat1);

	a1 = sin(GPstlat1);		
	a1 *= a1;			
	a1 *= equad;			
	a2 = sqrt((double)1-a1);	
	aux2 = 1./a2;			
	ptpcx = ptpcx/(aux1*aux2);
	ptpcy = ptpcy/(aux1*aux2);
	t = exp(-ptpcy/Prd);
	xx = pi/2. - 2.*atan(t);
 
	a1 = equad/2.;			
	a1 += 5.*equad*equad/24.;	
	a1 += equad*equad*equad/12.;	
	a2 = sin((double)4*atan(t));	
	aux3 = a1*a2;			
 
	a1 = 7.*equad*equad/48.;	
	a1 += 29.*equad*equad*equad/240.;	
	a2 = sin((double)8*atan(t));	
	aux4 = -a1*a2;			
 
	a1 = 7.*equad*equad*equad/120.;	
	a2 = sin((double)12*atan(t));	
	aux5 = a1*a2;		 

	ptpcy = xx + aux3 + aux4 + aux5;
	ptpcx = ptpcx/Prd + GPlon0;
	
	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return(TeCoord2D(ptpcx,ptpcy));
}




/********************************************************************
		GEODETIC TO LAMBERT CONIC COORDINATES
********************************************************************/
TeCoord2D
TeLambertConformal :: LL2PC (TeCoord2D& ptll)
{

	double	equad,			// Squared eccentricity
		e,			// Ancillary variables
		m1,m2,aux1,aux2,aux0,t1,t2,t0,n,efe,ro0,aux,
		t,ro,teta,ptllx,ptlly;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptllx = (double)ptll.x();
	ptlly = (double)ptll.y();

	equad = 2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);

	m1 = cos(GPstlat1)/sqrt((double)1-equad*pow(sin(GPstlat1),(double)2));
	m2 = cos(GPstlat2)/sqrt((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux1 = sqrt(((double)1-e*sin(GPstlat1))/((double)1+e*sin(GPstlat1)));
	aux2 = sqrt(((double)1-e*sin(GPstlat2))/((double)1+e*sin(GPstlat2)));
	aux0 = sqrt(((double)1-e*sin(GPlat0))/((double)1+e*sin(GPlat0)));
	t1 = ((1.-tan(GPstlat1/(double)2))/(1.+tan(GPstlat1/(double)2)))/pow(aux1,e);
	t2 = ((1.-tan(GPstlat2/(double)2))/(1.+tan(GPstlat2/(double)2)))/pow(aux2,e);
	t0 = ((1.-tan(GPlat0/(double)2))/(1.+tan(GPlat0/(double)2)))/pow(aux0,e);

	if (GPstlat1 == GPstlat2)
		n = sin(GPstlat1);
	else
		n = (log(m1)-log(m2))/(log(t1)-log(t2));

	efe = m1/(n*pow(t1,n));
	ro0 = Prd*efe*pow(t0,n);

	aux = sqrt(((double)1-e*sin(ptlly))/((double)1+e*sin(ptlly)));
	t = ((1.-tan(ptlly/(double)2))/(1.+tan(ptlly/(double)2)))/pow(aux,e);
	ro = Prd*efe*pow(t,n);
	teta = n*(ptllx - GPlon0);
	ptllx = ro*sin(teta);
	ptlly = ro0 - ro*cos(teta);
	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}



/********************************************************************
		LAMBERT CONIC TO GEODETIC COORDINATES
 ********************************************************************/
TeCoord2D
TeLambertConformal :: PC2LL (TeCoord2D& ptpc)
{
	double	equad,			// Squared eccentricity 
		pi,			// PI value
		e,m1,m2,aux1,aux2,aux0,t1,t2,t0,n,efe,ro0,ro,teta,
		t,xx,aux3,aux4,aux5,ptpcx,ptpcy;

	int	sinal;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();
	

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	equad = 2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);
	pi = 4.*atan((double)1);

	m1 = cos(GPstlat1)/sqrt((double)1-equad*pow(sin(GPstlat1),(double)2));
	m2 = cos(GPstlat2)/sqrt((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux1 = sqrt(((double)1-e*sin(GPstlat1))/((double)1+e*sin(GPstlat1)));
	aux2 = sqrt(((double)1-e*sin(GPstlat2))/((double)1+e*sin(GPstlat2)));
	aux0 = sqrt(((double)1-e*sin(GPlat0))/((double)1+e*sin(GPlat0)));
	t1 = ((1.-tan(GPstlat1/(double)2))/(1.+tan(GPstlat1/(double)2)))/pow(aux1,e);
	t2 = ((1.-tan(GPstlat2/(double)2))/(1.+tan(GPstlat2/(double)2)))/pow(aux2,e);
	t0 = ((1.-tan(GPlat0/(double)2))/(1.+tan(GPlat0/(double)2)))/pow(aux0,e);

	if (GPstlat1 == GPstlat2)
		n = sin(GPstlat1);
	else
		n = (log(m1)-log(m2))/(log(t1)-log(t2));

	efe = m1/(n*pow(t1,n));
	ro0 = Prd*efe*pow(t0,n);

	sinal = (int)(n/fabs(n));
	ro = sqrt(ptpcx*ptpcx + (ro0-ptpcy)*(ro0-ptpcy));
	ro *= sinal;
	teta = atan(ptpcx/(ro0-ptpcy));
	t = pow((ro/(Prd*efe)),(double)1/n);
	xx = pi/2. - 2.*atan(t);
	aux3 = equad/2. + 5.*equad*equad/24. + equad*equad*equad/12.;
	aux4 = 7.*equad*equad/48. + 29.*equad*equad*equad/240.;
	aux5 = (7.*equad*equad*equad/120.)*sin(12.*atan(t));

	ptpcy = xx + aux3*sin(4.*atan(t)) - aux4*sin(8.*atan(t)) + aux5;
	ptpcx = teta/n + GPlon0;
	
	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return (TeCoord2D(ptpcx,ptpcy));
}

/********************************************************************
		GEODETIC TO POLYCONIC COORDINATES
********************************************************************/
TeCoord2D
TePolyconic :: LL2PC (TeCoord2D& ptll)
{
	double	equad,			// Squared eccentricity 
		n,			// Great normal of ellipsoid
		aux01,			// Ancillary variables
		aux02,aux03,aux04,m0,aux1,aux2,aux3,
		aux4,m,e,ptllx,ptlly;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptllx = (double)ptll.x();
	ptlly = (double)ptll.y();

	equad = 2.*Pflt - pow(Pflt,(double)2);

	aux01 = (1.-equad/4.-3.*equad*equad/64.-5.*equad*equad*equad/256.)*GPlat0;
	aux02 = (3.*equad/8+3.*equad*equad/32.+45.*equad*equad*equad/1024.)*sin((double)2*GPlat0);
	aux03 = (15.*equad*equad/256.+45.*equad*equad*equad/1024.)*sin((double)4*GPlat0);
	aux04 = (35.*equad*equad*equad/3072.)*sin((double)6*GPlat0);
	m0 = Prd*(aux01 - aux02 + aux03 - aux04);

	if (ptlly == 0.)
	{
		ptllx = Prd*(ptllx - GPlon0);
		ptlly = -m0;
	}
	else
	{
		aux1 = (1.-equad/4.-3.*equad*equad/64.-5.*equad*equad*equad/256.)*ptlly;
		aux2 = (3.*equad/8+3.*equad*equad/32.+45.*equad*equad*equad/1024.)*sin((double)2*ptlly);
		aux3 = (15.*equad*equad/256.+45.*equad*equad*equad/1024.)*sin((double)4*ptlly);
		aux4 = (35.*equad*equad*equad/3072.)*sin((double)6*ptlly);
		m = Prd*(aux1 - aux2 + aux3 - aux4);
		n = Prd/sqrt((double)1-equad*pow(sin(ptlly),(double)2));
		e = (ptllx - GPlon0)*sin(ptlly);
		ptllx = n*sin(e)/tan(ptlly);
		ptlly = m - m0 + (n*(1. - cos(e))/tan(ptlly));
	}
	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}

/*******************************************************************
		POLYCONIC TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TePolyconic :: PC2LL (TeCoord2D& ptpc)
{	
	double	A,	
		B,
		C,
		equad,			// Squared eccentricity
		aux01,			// Ancillary variables
		aux02,aux03,aux04,m0,mn,mnl,ma,cp,lat1,lat2,aux05,
		aux06,aux07,aux08,aux09,aux10,aux11,aux12,aux21,
		aux22,aux23,aux24,ptpcx,ptpcy;


	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	equad = 2.*Pflt - pow(Pflt,(double)2);

	// Initialize latitude with latitude of origin
	aux01 = (1.-equad/4.-3.*equad*equad/64.-5.*equad*equad*equad/256.)*GPlat0;
	aux02 = (3.*equad/8+3.*equad*equad/32.+45.*equad*equad*equad/1024.)*sin((double)2*GPlat0);
	aux03 = (15.*equad*equad/256.+45.*equad*equad*equad/1024.)*sin((double)4*GPlat0);
	aux04 = (35.*equad*equad*equad/3072.)*sin((double)6*GPlat0);
	m0 = Prd*(aux01 - aux02 + aux03 - aux04);

	if (ptpcy == (-m0))
	{
		ptpcy= 0.;
		ptpcx = ptpcx/Prd + GPlon0;
	}
	else
	{
		A = (m0 + ptpcy)/Prd;
		B = ((ptpcx*ptpcx)/(Prd*Prd)) +(A*A);

		lat2 = A;

		do
		{
			C = (sqrt(1.- equad*sin(lat2)*sin(lat2)))*tan(lat2);

			// mn calculation	
			aux21 = (1.-equad/4.-3.*equad*equad/64.-5.*equad*equad*equad/256.)*lat2;
			aux22 = (3.*equad/8.+3.*equad*equad/32.+45.*equad*equad*equad/1024.)*sin((double)2*lat2);
			aux23 = (15.*equad*equad/256.+45.*equad*equad*equad/1024.)*sin((double)4*lat2);
			aux24 = (35.*equad*equad*equad/3072.)*sin((double)6*lat2);
			mn = Prd*(aux21 - aux22 + aux23 - aux24);
		
			// mnl calculation
			aux05 = 1.- equad/4.-3.*equad*equad/64.-5.*equad*equad*equad/256.;
			aux06 = 2.*(3.*equad/8.+3.*equad*equad/32.+45.*equad*equad*equad/1024.)*cos((double)2*lat2);
			aux07 = 4.*(15.*equad*equad/256.+45.*equad*equad*equad/1024.)*cos((double)4*lat2);
			aux08 = 6.*(35.*equad*equad*equad/3072.)*cos((double)6*lat2);
			mnl = aux05 - aux06 + aux07- aux08;

			// ma calculation
			ma = mn/Prd;

			aux09 = (A*(C*ma+1)-ma)-(0.5*(ma*ma+B)*C);
			aux10 = equad*sin((double)2*lat2)*(ma*ma+B-2.*A*ma);
			aux11 = 4.*C+(A-ma);
			aux12 = C*mnl-(2./sin((double)2*lat2));

			// New latitude calculation 
			lat1 = lat2 - (aux09/((aux10/(aux11*aux12)) - mnl));
			cp = fabs(lat1-lat2) ;
			lat2 = lat1;


		}while(cp > 0.0000000001);

		ptpcy = lat1;
		ptpcx = ((asin((ptpcx*C)/Prd))/(sin(lat1))) + GPlon0;
	}

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return (TeCoord2D(ptpcx,ptpcy));	

}

/********************************************************************
	GEODETIC TO LAT LONG
********************************************************************/
TeCoord2D
TeLatLong :: LL2PC (TeCoord2D& ptll)
{
	return TeCoord2D( ptll.x()*TeCRD, ptll.y()*TeCRD ); 
}

//FAMI
void
TeLatLong :: LL2PC (double xi, double yi, double& xo, double& yo) const
{
	xo = xi*TeCRD;
	yo = yi*TeCRD;
	return;
}

/********************************************************************
	LAT LONG TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeLatLong :: PC2LL (TeCoord2D& ptpc)
{
	double ptpcx, ptpcy;

	ptpcx = ptpc.x()*TeCDR;
	ptpcy = ptpc.y()*TeCDR;
	
	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return TeCoord2D(ptpcx,ptpcy); 
}

//FAMI
void
TeLatLong :: PC2LL (double xi, double yi, double& ptpcx, double& ptpcy)
{
	ptpcx = xi*TeCDR;
	ptpcy = yi*TeCDR;
	
	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return; 
}

/********************************************************************
	GEODETIC TO ALBERS EQUAL-AREA COORDINATES
********************************************************************/
TeCoord2D
TeAlbers :: LL2PC (TeCoord2D& ptll)
{

	double	equad,			// Squared eccentricity
		e,			// Ancillary variables
		m1,m2,aux1,aux10,aux11,aux12,aux2,aux20,aux21,
		aux22,q0,q1,q2,q,n,c,ro0,teta,ro,ptllx,ptlly;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptllx = (double)ptll.x();
	ptlly = (double)ptll.y();

	equad = 2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);

	m1 = cos(GPstlat1)/sqrt((double)1-equad*pow(sin(GPstlat1),(double)2));
	m2 = cos(GPstlat2)/sqrt((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux1 = sin(ptlly)/((double)1-equad*pow(sin(ptlly),(double)2));
	aux10 = sin(GPlat0)/((double)1-equad*pow(sin(GPlat0),(double)2));
	aux11 = sin(GPstlat1)/((double)1-equad*pow(sin(GPstlat1),(double)2));
	aux12 = sin(GPstlat2)/((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux2 = log((1. - e*sin(ptlly))/(1. + e*sin(ptlly)));
	aux20 = log((1. - e*sin(GPlat0))/(1. + e*sin(GPlat0)));
	aux21 = log((1. - e*sin(GPstlat1))/(1. + e*sin(GPstlat1)));
	aux22 = log((1. - e*sin(GPstlat2))/(1. + e*sin(GPstlat2)));
	q0 = (1. - equad)*(aux10 - (1./(2.*e))*aux20);
	q1 = (1. - equad)*(aux11 - (1./(2.*e))*aux21);
	q2 = (1. - equad)*(aux12 - (1./(2.*e))*aux22);
	q = (1. - equad)*(aux1 - (1./(2.*e))*aux2);
	n = (m1*m1 - m2*m2)/(q2 - q1);
	c = m1*m1 + n*q1;
	ro0 = Prd*sqrt(c - n*q0)/n;
	teta = n*(ptllx - GPlon0);
	ro = Prd*sqrt(c - n*q)/n;

	ptllx = ro*sin(teta);
	ptlly = ro0 - ro*cos(teta);

	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}


/********************************************************************
		ALBERS EQUAL-AREA TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeAlbers :: PC2LL(TeCoord2D& ptpc)
{

	double	equad,			// Squared eccentricity
		e,			// Eccentricity
		m1,			// Ancillary variable
		m2,aux10,aux11,aux12,aux20,aux21,aux22,q0,q1,q2,
		n,c,ro0,ro,q,aux,beta,aux1,aux2,aux3,teta,ptpcx,ptpcy;

	int	sinal;			// Ancillary variable	

	
	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	sinal = (int)(GPstlat2/fabs(GPstlat2));
	equad = 2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);

	m1 = cos(GPstlat1)/sqrt((double)1-equad*pow(sin(GPstlat1),(double)2));
	m2 = cos(GPstlat2)/sqrt((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux10 = sin(GPlat0)/((double)1-equad*pow(sin(GPlat0),(double)2));
	aux11 = sin(GPstlat1)/((double)1-equad*pow(sin(GPstlat1),(double)2));
	aux12 = sin(GPstlat2)/((double)1-equad*pow(sin(GPstlat2),(double)2));
	aux20 = log((1. - e*sin(GPlat0))/(1. + e*sin(GPlat0)));
	aux21 = log((1. - e*sin(GPstlat1))/(1. + e*sin(GPstlat1)));
	aux22 = log((1. - e*sin(GPstlat2))/(1. + e*sin(GPstlat2)));
	q0 = (1. - equad)*(aux10 - (1./(2.*e))*aux20);
	q1 = (1. - equad)*(aux11 - (1./(2.*e))*aux21);
	q2 = (1. - equad)*(aux12 - (1./(2.*e))*aux22);
	n = (m1*m1 - m2*m2)/(q2 - q1);
	c = m1*m1 + n*q1;
	ro0 = Prd*sqrt(c - n*q0)/n;
	ro = sqrt(ptpcx*ptpcx + (ro0 - ptpcy)*(ro0 - ptpcy));
	q = (c - (ro*ro*n*n/(Prd*Prd)))/n;
	aux = ((1. - equad)/(2.*e))*log((1. - e)/(1. + e));
	beta = asin(q/(1. - aux));
	aux1 = (equad/3. + 31.*equad*equad/180.+517.*equad*equad*equad/5040.)*sin(2.*beta);
	aux2 = (23.*equad*equad/360.+251.*equad*equad*equad/3780.)*sin(4.*beta);
	aux3 = (761.*equad*equad*equad/45360.)*sin(6.*beta);
	teta = fabs(atan(ptpcx/(ro0 - ptpcy)));

	if (sinal == 1)
	{
		if (ptpcx < 0.)
			teta = -teta;
	}

	if (sinal == -1)
	{
		if (ptpcx > 0.)
			teta *= sinal;
	}

	ptpcy = beta + aux1 + aux2 + aux3;
	ptpcx = GPlon0 + (teta/n);

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return (TeCoord2D(ptpcx,ptpcy));
}

/*******************************************************************
		GEODETIC TO MILLER COORDINATES
********************************************************************/
TeCoord2D
TeMiller :: LL2PC (TeCoord2D& ptll)
{

	double	pi,		// PI value
		ptllx,ptlly;	// Ancillary variables 

	int	sinal;		// Ancillary variables

	double Prd  = GPdatum.radius();
	
	ptllx = (double)ptll.x();
	ptlly = (double)ptll.y();

	pi = 4.*atan((double)1);
	sinal = (int)(fabs(ptlly)/ptlly);
	
	ptllx = Prd*(ptllx - GPlon0);

	if (sinal == 1)
		ptlly = (Prd/0.8)*(log(tan(pi/4. + 0.4*ptlly)));
	else
	{
		ptlly *= sinal;
		ptlly = (Prd/0.8)*(log(tan(pi/4. + 0.4*ptlly)));
		ptlly *= sinal;
	}

	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}


/********************************************************************
		MILLER TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeMiller :: PC2LL (TeCoord2D& ptpc)
{

	double	e,		// Neperian logarithm base 
		pi,		// PI value
		ptpcx,ptpcy;	// Ancillary variables

	int	sinal;		// Ancillary variable

	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	e = 2.718281828;
	pi = 4.*atan((double)1);
	sinal = (int)(fabs(ptpcy)/ptpcy);

	ptpcx = GPlon0 + (ptpcx/Prd);

	if (sinal == 1)
		ptpcy = 2.5*atan(pow(e,(0.8*ptpcy/Prd)))-5.*pi/8.;
	else
	{
		ptpcy *= sinal;
		ptpcy = 2.5*atan(pow(e,(0.8*ptpcy/Prd))) - 5.*pi/8.;
		ptpcy *= sinal;
	}

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return(TeCoord2D(ptpcx,ptpcy));
}


/*******************************************************************
		GEODETIC TO SINUSOIDAL COORDINATES
********************************************************************/
TeCoord2D
TeSinusoidal :: LL2PC (TeCoord2D& ptll)
{

	double	ptllx,ptlly;	// Ancillary variables 

	double Prd  = GPdatum.radius();
	
	ptllx = (double)ptll.x();
	ptlly = (double)ptll.y();
	
	ptllx = Prd*(ptllx - GPlon0);
	ptllx *= cos(ptlly);

	ptlly = Prd*ptlly;
	
	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));	
}


/********************************************************************
		SINUSOIDAL TO GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeSinusoidal :: PC2LL (TeCoord2D& ptpc)
{

	double	ptpcx,ptpcy;	// Ancillary variables

	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	ptpcy = ptpcy/Prd;

	ptpcx = GPlon0 + (ptpcx/(Prd*cos(ptpcy)));
	
	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return(TeCoord2D(ptpcx,ptpcy));
}

/*******************************************************************
		GEODETIC TO CYLINDRICAL EQUIDISTANT COORDINATES
********************************************************************/
TeCoord2D
TeCylindricalEquidistant :: LL2PC (TeCoord2D& ptll)
{
	double	ptllx,ptlly;

	double Prd  = GPdatum.radius();

	ptllx = ptll.x();
	ptlly = ptll.y();

	ptllx = Prd*(ptllx - GPlon0)*cos(GPstlat1);
	ptlly = Prd*ptlly;

	return(TeCoord2D(ptllx+GPoffx,ptlly+GPoffy));
}


/*******************************************************************
	CYLINDRICAL EQUIDISTANT TO	GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TeCylindricalEquidistant :: PC2LL (TeCoord2D& ptpc)
{
	double	ptpcx,ptpcy;

	double Prd  = GPdatum.radius();

	ptpcx = ptpc.x()-GPoffx;
	ptpcy = ptpc.y()-GPoffy;

	ptpcy = ptpcy/Prd;
	ptpcx = GPlon0 + ptpcx/(Prd*cos(GPstlat1));

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return (TeCoord2D(ptpcx,ptpcy));
}

/*******************************************************************
		GEODETIC TO POLAR STEREOGRAPHIC COORDINATES
********************************************************************/
TeCoord2D
TePolarStereographic :: LL2PC (TeCoord2D& ptll)
{
	double	k0,				// Scale factor
		e,					// Eccentricity
		pi,					// Auxillary variables
		lon0,				// auxilliary origin longitude
		t,ro,aux1,aux2,aux3,ptllx,ptlly;
	
	int GPhemis;
	if (GPhemisphere == TeNORTH_HEM)
		GPhemis = 1;
	else
		GPhemis = -1;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();
	
	ptllx = ptll.x();
	ptlly = ptll.y();

	k0 = 0.933;	// Standard parallel 60 degrees 
	e = sqrt((double)2*Pflt - pow(Pflt,(double)2));
	pi = 4.*atan((double)1);

	ptlly *= GPhemis;
	ptllx *= GPhemis;
	lon0 = (GPhemis == 1) ? GPlon0 : -GPlon0;

	aux1 = (1. - e*sin(ptlly))/(1. + e*sin(ptlly));
	t = tan((pi/4.) - (ptlly/2.))/pow(aux1,(e/(double)2));
	aux2 = pow(((double)1 + e),((double)1 + e));
	aux3 = pow(((double)1 - e),((double)1 - e));
	ro = 2.*Prd*k0*t/sqrt(aux2*aux3);

	aux1   = ro*sin(ptllx - lon0);
	ptlly = -ro*cos(ptllx - lon0);
	aux1 *= GPhemis;
	ptlly *= GPhemis;
	
	if (GPhemis == -1)
	{
		aux1 *= GPhemis;
		ptlly *= GPhemis;
	}
//	return(Point((float)aux1,(float)ptlly));
	return (TeCoord2D(aux1+GPoffx,ptlly+GPoffy));

}

//FAMI
void
TePolarStereographic :: LL2PC (double xi, double yi, double& xo, double& yo) const
{
	double	k0,				// Scale factor
		e,					// Eccentricity
		pi,					// Auxillary variables
		lon0,				// auxilliary origin longitude
		t,ro,aux1,aux2,aux3,ptllx,ptlly;
	
	int GPhemis;
	if (GPhemisphere == TeNORTH_HEM)
		GPhemis = 1;
	else
		GPhemis = -1;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();
	
	ptllx = xi;
	ptlly = yi;

	k0 = 0.933;	// Standard parallel 60 degrees 
	e = sqrt((double)2*Pflt - pow(Pflt,(double)2));
	pi = 4.*atan((double)1);

	ptlly *= GPhemis;
	ptllx *= GPhemis;
	lon0 = (GPhemis == 1) ? GPlon0 : -GPlon0;

	aux1 = (1. - e*sin(ptlly))/(1. + e*sin(ptlly));
	t = tan((pi/4.) - (ptlly/2.))/pow(aux1,(e/(double)2));
	aux2 = pow(((double)1 + e),((double)1 + e));
	aux3 = pow(((double)1 - e),((double)1 - e));
	ro = 2.*Prd*k0*t/sqrt(aux2*aux3);

	aux1   = ro*sin(ptllx - lon0);
	ptlly = -ro*cos(ptllx - lon0);
	aux1 *= GPhemis;
	ptlly *= GPhemis;
	
	if (GPhemis == -1)
	{
		aux1 *= GPhemis;
		ptlly *= GPhemis;
	}

	xo = aux1+GPoffx;
	yo = ptlly+GPoffy;
	return;
}

//FAMI
void TePolarStereographic :: LL2PC (ostream& java) const 
{
	double	k0,			// Scale factor
		equad,			// Squared eccentricity
		e,		        // Eccentricity
		pi,			// Ancillary variables
		lon0,			// auxilliary origin longitude
		aux1,aux2,aux3,aux4,aux5, aux6, aux7;
	
		
	//java << "\tpx = xpc - " << GPoffx << ";\n";
	//java << "\tpy = ypc - " << GPoffy << ";\n";

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();
	k0   = 0.933;
	
	pi = 4.*atan((double)1); 
	equad =  2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);
	
	
	if (GPhemisphere != TeNORTH_HEM) {
		//java << "\tpx *= -1;\n";
		//java << "\tpy *= -1;\n";
	}
	lon0 = (GPhemisphere == TeNORTH_HEM) ? GPlon0 : -GPlon0;
	string name = (GPhemisphere == TeNORTH_HEM) ? "polar_north" : "polar_south";
	//java << "\tro = Math.sqrt(px*px + py*py);\n";	
	aux1 = pow(((double)1 + e),((double)1 + e));
    aux6 = pow(((double)1 + e),((double)1 + e));
    aux7 = pow(((double)1 - e),((double)1 - e));
	aux2 = 2.*Prd*k0;
	
	//java << "\tt = (ro* " << sqrt(aux1*aux2) << ")/(" << 2.*Prd*k0 << ") " << ";\n";
	//java << "\txx = " << pi/2. << " - 2.*Math.atan(t);\n";
	
	aux3 =  equad/2. + 5.*equad*equad/24. + equad*equad*equad/12.;
	aux4 = 7.*equad*equad/48. + 29.*equad*equad*equad/240.;
	aux5 = 7.*equad*equad*equad/120.;
	
	
		java <<     "\"name\" : \"" <<  name << "\"," ;
		java <<     "\"GPoffx\" : " <<  GPoffx << ",";
		java <<     "\"GPoffy\" : " <<  GPoffy << ",";
		java <<     "\"lon0\" : " << GPlon0 << "," ;
        java <<     "\"e\" : " << e << "," ;
		java <<     "\"coeff1\" : " << aux1 << "," ;
		java <<     "\"coeff2\" : " << aux2 << "," ;
		java <<     "\"coeff3\" : " << aux3 << "," ;
		java <<     "\"coeff4\" : " << aux4 << "," ;
		java <<     "\"coeff5\" : " << aux5 <<  "," ;
		java <<     "\"coeff6\" : " << aux6 <<  "," ;
		java <<     "\"coeff7\" : " << aux7 <<  "," ;
        
		
		

	//java << "\tyll = xx + " << aux3 << "*Math.sin(2*xx) + " << aux4 << "*Math.sin(4*xx) + "<< aux5 << "*Math.sin(6*xx);\n";

	//java << "\tif (py != 0.) \n";
	//java << "\t\txll = " << lon0 << "+ Math.atan(px/(-py));\n";

	if (GPhemisphere == TeNORTH_HEM)
	{
		//java << "\tif (and(greater(px, 0.), greater(py,0.)))\n";
		//java << "\t\txll = xll + " << pi << ";\n";
		//java << "\telse if (and(smaller(px,0.), greater(py,0.)))\n";
		//java << "\t\txll = xll - " << pi << ";\n";
		//java << "\telse if (and(greater(px, 0.), equal(py,0.)))\n";
		//java << "\t\txll = " << lon0  << "+ " << pi << "/ 2.;\n";
		//java << "\telse if (and(smaller(px, 0.),  equal(py,0.)))\n";
		//java << "\t\txll = " << lon0 << " - " << pi << "/ 2.;\n";
		//java << "\telse if (and(equal(px,0.), equal(py, 0.)))\n";
		//java << "\t\txll = " << lon0 << ";\n";
	}
	else
	{
		//java << "\tyll = -yll;\n";
		//java << "\txll = -xll;\n";

		//java << "\tif (and(greater(px,0.), smaller(py,0)))\n";
		//java << "\t\txll = xll + " << pi << ";\n";
		//java << "\telse if (and (smaller(px, 0.),  smaller(py,0)))\n";
		//java << "\t\txll = xll - " << pi << ";\n";
		//java << "\telse if (and(greater(px,0.), equal(py,0.)))\n";
		//java << "\t\txll = " << lon0 << " + " << pi << "/ 2.;\n";
		//java << "\telse if (and(smaller(px,0.), equal(py, 0.)))\n";
		//java << "\t\txll = " << lon0 << " - " << pi << " / 2.;\n";
		//java << "\telse if (and(equal(px,0.), equal(py, 0.)))\n";
		//java << "\t\txll = " << lon0 << ";\n";
	}

	//java << "\tif (smaller(xll, (-" << pi << ") )) xll += 2.* "<< pi << ";\n";
	//java << "\telse if (greater(xll, " << pi << ")  )    xll -= 2.*" << pi << ";\n";
	
	//java << "\txll = xll * " << 180/3.14159 << ";\n";
	//java << "\tyll = yll * " << 180/3.14159 << ";\n\n";

	//java << "\treturn { lon :  xll, lat : yll } ;\n";
	//java << "}\n";
	
}
/*******************************************************************
	POLAR STEREOGRAPHIC TO	GEODETIC COORDINATES
********************************************************************/
TeCoord2D
TePolarStereographic :: PC2LL (TeCoord2D& ptpc)
{
	double	k0,			// Scale factor

		equad,			// Squared eccentricity
		e,		        // Eccentricity
		pi,			// Ancillary variables
		lon0,			// auxilliary origin longitude
		ro,t,xx,aux1,aux2,aux3,aux4,aux5,ptpcx=0.,ptpcy,px,py;

	px = (double)ptpc.x()-GPoffx;
	py = (double)ptpc.y()-GPoffy;

	int GPhemis;
	if (GPhemisphere == TeNORTH_HEM)
		GPhemis = 1;
	else
		GPhemis = -1;

	double Pflt = GPdatum.flattening();
	double Prd  = GPdatum.radius();

//	k0 = 0.994;	// Standard parallel 80.1 degrees 
	k0 = 0.933;	// Standard parallel 60 degrees
	pi = 4.*atan((double)1);
	equad = 2.*Pflt - pow(Pflt,(double)2);
	e = sqrt(equad);

	if (GPhemis == -1)
	{
		px *= GPhemis;
		py *= GPhemis;
	}

	lon0 = (GPhemis == 1) ? GPlon0 : -GPlon0;

	ro = sqrt(px*px + py*py);
	aux1 = pow(((double)1 + e),((double)1 + e));
	aux2 = pow(((double)1 - e),((double)1 - e));
	t = (ro*sqrt(aux1*aux2))/(2.*Prd*k0);
	xx = pi/2. - 2.*atan(t);
	aux3 = equad/2. + 5.*equad*equad/24. + equad*equad*equad/12.;
	aux4 = 7.*equad*equad/48. + 29.*equad*equad*equad/240.;
	aux5 = 7.*equad*equad*equad/120.;

	ptpcy = xx + aux3*sin((double)2*xx) + aux4*sin((double)4*xx) + aux5*sin((double)6*xx);

	if (py != 0.)
		ptpcx = lon0 + atan(px/(-py));


	if (GPhemis == 1)
	{
		if (px > 0. && py > 0.)
			ptpcx = ptpcx + pi;
		else if (px < 0. && py > 0.)
			ptpcx = ptpcx - pi;
		else if (px > 0. && py == 0.)
			ptpcx = lon0 + pi / 2.;
		else if (px < 0. && py == 0.)
			ptpcx = lon0 - pi / 2.;
		else if (px == 0. && py == 0.)
			ptpcx = lon0;
	}
	else
	{
		ptpcy *= GPhemis;
		ptpcx *= GPhemis;

		if (px > 0. && py < 0.)
			ptpcx = ptpcx + pi;
		else if (px < 0. && py < 0.)
			ptpcx = ptpcx - pi;
		else if (px > 0. && py == 0.)
			ptpcx = lon0 + pi / 2.;
		else if (px < 0. && py == 0.)
			ptpcx = lon0 - pi / 2.;
		else if (px == 0. && py == 0.)
			ptpcx = lon0;
	}

	if (ptpcx < (-pi)) ptpcx += 2.*pi;
	else if (ptpcx > pi)    ptpcx -= 2.*pi;

	if( GPdestination && !(datum() == GPdestination->datum()))
		ChangeLL(ptpcx,ptpcy);

	return (TeCoord2D(ptpcx,ptpcy));
}

//FAMI
/************************************************************************
		GEODETIC COORDINATES TO SATELLITE PROJECTION
*************************************************************************/
TeCoord2D 
TeSatelliteProjection::LL2PC(TeCoord2D& ptll)
{
	double equad, n, x, y, z, sn, dp, dl,xlo,yla,lin,col,resx,resy;
	double x1, x2, a, b, c,  Rd2, Rm, Rm2, Rs2, v;

//	xlo = ptpc.x()-GPoffx;
//	yla = ptpc.y()-GPoffy;
	xlo = ptll.x();
	yla = ptll.y();

	// Cartesian geocentric coordinates
	xlo = xlo - GPlon0;
	yla = yla - GPlat0;
	equad = 2.*GPdatum.flattening() - pow(GPdatum.flattening(),(double)2);
	n = GPdatum.radius()/sqrt((double)1-equad*pow(sin(yla),(double)2));
	x = n*cos(yla)*cos(xlo);
	y = n*cos(yla)*sin(xlo);
	z = (n*(1-equad))*sin(yla);
 
	// Field of view angles
	dp = atan(y / (SPrs - x));
	dl = atan(z * cos(dp) /(SPrs - x));

	// Visibility test
	if ( x < 0.0 )
	{
		col = TeMAXFLOAT;
		lin = TeMAXFLOAT;
		return (TeCoord2D(col,lin));
	}
	else
	{
		Rd2 = GPdatum.radius() * GPdatum.radius() ;
		Rm  = GPdatum.radius() * (1 - GPdatum.flattening());
		Rm2 = Rm * Rm ;
		Rs2 = SPrs * SPrs;

		v = (tan(dp)*tan(dp)*cos(dp)*cos(dp)*Rm2)+(tan(dl)*tan(dl)*Rd2);
		a = cos(dp)*cos(dp)*Rm2 + v;      
		b = -2.*SPrs*v;
		c = Rs2*v-Rd2*Rm2*cos(dp)*cos(dp);

		// Equation Resolution a*x**2+b*x+c=0 
		v = (b*b) - (4.*a*c);
		if (v < 0.0) v=0.0;
   		x1 = (-b + sqrt(v)) / (2.*a);
		x2 = (-b - sqrt(v)) / (2.*a);
		if (x1 < x2)  x1 = x2;

		if ( fabs(x-x1) > 1.0 )
		{
			col = TeMAXFLOAT;
			lin = TeMAXFLOAT;
			return (TeCoord2D(col,lin));
		}
	}
	// Line and column of image
	if (SPscn == 0.) sn = 1.0;  else sn = -1.0;
	col = (sn * dp / SPrj + SPjs );
	lin = (-sn * dl / SPri + SPis);

	// Axis rotation correction due to yaw angle
	col = col*cos(SPyaw) - lin*sin(SPyaw);
	lin = lin*cos(SPyaw) + col*sin(SPyaw);
	resx = tan(SPrj) * (SPrs - GPdatum.radius());
	resy = tan(SPri) * (SPrs - GPdatum.radius());

	return (TeCoord2D(col*resx,-(lin*resy)));
}

/************************************************************************
		SATELLITE PROJECTION TO GEODETIC COORDINATES
************************************************************************/
TeCoord2D
TeSatelliteProjection::PC2LL(TeCoord2D& ptpc)
{
	double	dl, dp, x, y, z, x1, x2, equad, n, d, sn,
		a, b, c,  Rd2, Rm,Rm2, Rs2,v,ptpcx,ptpcy,
		yla,xlo,resx,resy;

	resx = tan(SPrj) * (SPrs - GPdatum.radius());
	resy = tan(SPri) * (SPrs - GPdatum.radius());
	ptpcx = ptpc.x()/resx;
	ptpcy = -ptpc.y()/resy;

	// Axis rotation correction due yaw angle
	ptpcx = ptpcx*cos(SPyaw) + ptpcy*sin(SPyaw);
	ptpcy = ptpcy*cos(SPyaw) - ptpcx*sin(SPyaw);

	// Field of view angles
	if (SPscn == 0.) sn =  1.0;
	else 		 sn = -1.0;
	dl = - sn * ((ptpcy - SPis) * SPri);
	dp =   sn * ((ptpcx - SPjs) * SPrj);

	// Cartesian coordinates
	Rd2 = GPdatum.radius() * GPdatum.radius() ;
	Rm  = GPdatum.radius() * (1 - GPdatum.flattening());
	Rm2 = Rm * Rm ;
	Rs2 = SPrs * SPrs;

	v = (tan(dp)*tan(dp)*cos(dp)*cos(dp)*Rm2)+(tan(dl)*tan(dl)*Rd2);
	a = cos(dp)*cos(dp)*Rm2 + v;      
	b = -2.*SPrs*v;
	c = Rs2*v-Rd2*Rm2*cos(dp)*cos(dp);

	// Equation Resolution a*x**2+b*x+c=0 
	v = (b*b) - (4.*a*c);
	if (v < 0)
	{
		xlo = TeMAXFLOAT;
		yla = TeMAXFLOAT;
		return (TeCoord2D(xlo,yla));
	}

   	x1 = (-b + sqrt(v)) / (2.*a);
	x2 = (-b - sqrt(v)) / (2.*a);

	if ( x1 >= x2)  x = x1;
	else		  x = x2;

	z = (SPrs - x) * tan(dl) / cos(dp);
	y = (SPrs - x) * tan(dp);

	// Geodetic coordinates
	equad 	= 2. * GPdatum.flattening() - GPdatum.flattening() * GPdatum.flattening();

	double aux = SPrs * dl / GPdatum.radius();
	if (aux > 1.0) aux = 1.0;
	if (aux < -1.0) aux = -1.0;

	yla = asin(aux);
	
	do
	{
		n= GPdatum.radius()/sqrt((double)1-equad*pow(sin(yla),(double)2));
		yla = atan((z + n*equad*sin(yla))/sqrt(x*x + y*y));
		d = GPdatum.radius()/sqrt((double)1-equad*pow(sin(yla),(double)2)) - n;
	}
	while (fabs(d) > 0.001);

	xlo = atan(y/x) + GPlon0;
	yla = yla + GPlat0;

	return (TeCoord2D(xlo,yla));
}

TeProjectionParams 
TeSatelliteProjection::params () const
{
	TeProjectionParams par;
	par = TeProjection::params();

	par.pri  = SPri;
	par.prj  = SPrj;
	par.pis  = SPis;
	par.pjs  = SPjs;
	par.prs  = SPrs;
	par.pscn = SPscn;
	par.pyaw = SPyaw;

return par;
}

TeDatum 
TeGetDatumFromProj4(const string& proj4)
{
	unsigned int pos;
    string attribute;
    string value;
	double dx, dy, dz;
	dx = dy = dz = 0.0;

	TeDatum dat;
	dat.name("UserDefined");
	for(unsigned int i = 0; i < proj4.size(); i++)
    {
        for( pos = i; i != proj4.size() && proj4[i] != '=';i++);

        attribute = proj4.substr(pos+1, i-pos-1);

        for(pos = i; i != proj4.size() && proj4[i] != ' ';i++);

        if(proj4[i] == ' ') value = proj4.substr(pos+1, i-pos-1);
        else                value = proj4.substr(pos+1, i-pos);
		
		double b;
		// Datum parameters
        if(attribute == "R")	// radius for a spherical ellipsoid
		{
			dat = TeDatum("Spherical",atof(value.c_str()));
			return dat;
		}
		else if(attribute == "a")   // Earth equatorial radius
			dat.radius(atof(value.c_str()));
		else if (attribute == "b")
			b = atof(value.c_str());
	    else if(attribute == "f")	// Earth flattening
			dat.flattening(atof(value.c_str()));
	    else if(attribute == "rf")	// Earth  reverse flattening (f = 1/rf)
			dat.flattening(1/atof(value.c_str()));
		else if (attribute == "es") // eccentricity squared (f=1-(1-es)^0.5) 
			dat.flattening(1-sqrt(1-atof(value.c_str())));
		else if (attribute == "e")	// eccentricity (f=1-(1-es^2)^0.5)
			dat.flattening(1-sqrt(1-pow(atof(value.c_str()),2)));
		else if (attribute == "ellps")
			dat.name(value);
		else if (attribute == "towgs84")
		{
			vector<string> deltas;
			TeSplitString(value, ",", deltas);
			
			if (deltas.size()>=3)
			{
				dx = atof(deltas[0].c_str());
				dy = atof(deltas[1].c_str());
				dz = atof(deltas[2].c_str());
			}
		}
	}
	if ( dx==0.0 && dy== 0 && dz == 0.0)
	{
		dat = TeDatum("WGS84",6378137,0.003352811,66.87,-4.37,38.52);
	}
	// this is a SAD69
	else if (dx == -66.87 && dy == 4.37 &&  dz == -38.52)
	{
		dat = TeDatum("SAD69",6378160,0.003352892,0.0,0.0,0.0);
	}
	else
	{
		dat.dx_ = dx;
		dat.dy_ = dy;
		dat.dz_ = dz;
	}
	return dat;
}

TeProjection* TeGetTeProjectionFromSProj(const string& proj4)
{
	// a map from sproj4 projections to TerraLib projections
	map<string,string> sprojToTeProj;
	sprojToTeProj["aea"] = "Albers";
	sprojToTeProj["latlong"] = "LatLong";
	sprojToTeProj["lcc"] = "LambertConformal";
	sprojToTeProj["merc"] = "Mercator";
	sprojToTeProj["mill"] = "Miller";
	sprojToTeProj["utm"] = "UTM";
	sprojToTeProj["sinu"] = "Sinusoidal";
	sprojToTeProj["poly"] = "Polyconic";
	sprojToTeProj["eqc"] = "CylindricalEquidistant";
	sprojToTeProj["ups"] = "PolarStereographic";
	sprojToTeProj["sat"] = "Satellite"; //FAMI

    unsigned int pos;
    string attribute;
    string value;

  	TeProjectionParams par;
	par.hemisphere = TeNORTH_HEM;
	par.offx = 500000;
	par.offy = 0;
	par.units = "";
    for(unsigned int i = 0; i < proj4.size(); i++)
    {
        for( pos = i; i != proj4.size() && proj4[i] != '=';i++);

        attribute = proj4.substr(pos+1, i-pos-1);

        for(pos = i; i != proj4.size() && proj4[i] != ' ';i++);

        if(proj4[i] == ' ') value = proj4.substr(pos+1, i-pos-1);
        else                value = proj4.substr(pos+1, i-pos);

        if(attribute == "proj")
		{
			map<string,string>::iterator it = sprojToTeProj.find(value);
			if (it != sprojToTeProj.end())
				par.name = sprojToTeProj[value];
			else
				par.name = "NoProjection"; 
		}
	// Currently this routine understands the following parameters from
	// a sproj description: lon_0, lat_0, lat_1, lat_2, k, zone, x_0, y_0

        else if(attribute == "lon_0") 
			par.lon0 = atof(value.c_str())*TeCDR;
        else if(attribute == "lat_0") 
			par.lat0 = atof(value.c_str())*TeCDR;
        else if(attribute == "lat_1") 
			par.stlat1 = atof(value.c_str())*TeCDR;
        else if(attribute == "lat_2") 
			par.stlat2 = atof(value.c_str())*TeCDR;
        else if(attribute == "k") 
			par.scale = atof(value.c_str());
        else if(attribute == "zone") 
			par.lon0 = (-183 + 6*(atoi(value.c_str()))*TeCDR);
		else if (attribute == "south") 
		{
			par.hemisphere = TeSOUTH_HEM;
			par.offy = 10000000;
		}
		else if(attribute == "x_0") 
			par.offx = atof(value.c_str());
 		else if(attribute == "y_0") 
			par.offy = atof(value.c_str());
		else if (attribute == "units")
			par.units = value;
// TODO: sproj4 also allowd the selection of standard, predefined
// ellipsoid figures using the option +ellps=acronym. But we are not decoding 
// them yet because of our problem with the shiftings from SAD69
    }
	par.datum = TeGetDatumFromProj4(proj4);
	return TeProjectionFactory::make(par);
}

string TeGetSProjFromTeProjection(TeProjection* teproj)
{
	map<string,string> teProjToSProj;
	teProjToSProj["Albers"] = "aea";
	teProjToSProj["LatLong"] = "latlong";
	teProjToSProj["LambertConformal"] = "lcc";
	teProjToSProj["Mercator"] = "merc";
	teProjToSProj["Miller"] = "mill";
	teProjToSProj["UTM"] = "utm";
	teProjToSProj["Sinusoidal"] = "sinu";
	teProjToSProj["Polyconic"] = "poly";
	teProjToSProj["CylindricalEquidistant"] = "eqc";
	teProjToSProj["PolarStereographic"] = "ups";
	teProjToSProj["Satellite"] = "sat"; //FAMI

    string sresp = "+proj=";
	map<string,string>::iterator it = teProjToSProj.find(teproj->name());
	if (it == teProjToSProj.end())
	{
		sresp += "noprojection";
		return sresp;
	}
	else
		sresp += it->second;

	TeProjInfo pjInfo = TeProjectionInfo(teproj->name());
	if ( pjInfo.hasLon0 )
		sresp += " +lon_0=" + Te2String(teproj->lon0()*TeCRD,6);

	if ( pjInfo.hasLat0 )
		sresp += " +lat_0=" + Te2String(teproj->lat0()*TeCRD,6);

	if ( pjInfo.hasStlat1 )
		sresp += " +lat_1=" + Te2String(teproj->stLat1()*TeCRD,6);
	
	if ( pjInfo.hasStlat2 )
		sresp += " +lat_2=" +Te2String(teproj->stLat2()*TeCRD,6);

	if ( pjInfo.hasOffx )
		sresp += " +x_0=" + Te2String(teproj->offX(),6);

	if ( pjInfo.hasOffy )
		sresp += " +y_0=" + Te2String(teproj->offY(),6);
	
	if ( pjInfo.hasScale )
		sresp += " +k="  + Te2String(teproj->scale(),6);

	string datumD = teproj->datum().getProj4Description();
	if (!datumD.empty())
		sresp += datumD;
	return sresp;
}

string 
TeGetWKTFromTeProjection(TeProjection* teproj)
{
	string wkt = "";
	if (!teproj)
		return wkt;

	string wktDatum = teproj->datum().getWKTDescription();
	if (teproj->name() == "LatLong")
		return wktDatum;

	map<string,string> teProjToWKT;
	teProjToWKT["Albers"] = "Albers_Conic_Equal_Area";
	teProjToWKT["LambertConformal"] = "Lambert_Conformal_Conic_1SP";
	teProjToWKT["Mercator"] = "Mercator_1SP";
	teProjToWKT["Miller"] = "Miller_Cylindrical";
	teProjToWKT["UTM"] = "Transverse_Mercator";
	teProjToWKT["Sinusoidal"] = "Sinusoidal";
	teProjToWKT["Polyconic"] = "Polyconic";
	teProjToWKT["CylindricalEquidistant"] = "Equirectangular";
	teProjToWKT["PolarStereographic"] = "Polar_Stereographic";
	teProjToWKT["Satellite"] = "Satellite"; //FAMI
	teProjToWKT["NoProjection"] = "Unknown";

	wkt = "PROJCS[\"" + teproj->name() + "\",";
	wkt += wktDatum;
	wkt += ",PROJECTION[\"" + teProjToWKT[teproj->name()] + "\"]";

	TeProjInfo pjInfo = TeProjectionInfo(teproj->name());
	if (pjInfo.hasLon0)
		wkt +=",PARAMETER[\"central_meridian\","+ Te2String(teproj->lon0()*TeCRD,6)+"]";

	if (pjInfo.hasLat0)
		wkt +=",PARAMETER[\"latitude_of_origin\","+ Te2String(teproj->lat0()*TeCRD,6)+"]";

	if (pjInfo.hasStlat1)
		wkt +=",PARAMETER[\"standard_parallel_1\","+ Te2String(teproj->stLat1()*TeCRD,6)+"]";
	
	if (pjInfo.hasStlat2)
		wkt +=",PARAMETER[\"standard_parallel_2\","+ Te2String(teproj->stLat2()*TeCRD,6)+"]";

	if (pjInfo.hasOffx)
		wkt +=",PARAMETER[\"false_easting\","+ Te2String(teproj->offX(),6)+"]";

	if (pjInfo.hasOffy)
		wkt +=",PARAMETER[\"false_northing\","+ Te2String(teproj->offY(),6)+"]";

	if (pjInfo.hasScale)
		wkt +=",PARAMETER[\"scale_factor\","+ Te2String(teproj->scale(),6)+"]";
	
	wkt += ",UNIT[\"meter\",1]]";
	return wkt;
}

bool tokenizeWKT(char** wkt, vector<string>& tokens)
{
	const char *pszInput =  *wkt;
	int isInQuote = false;
	
	string token = "";
	int  sizeToken = 0;
	while (*pszInput != '\0')
	{
		if (*pszInput == '"')
		{
			isInQuote = !isInQuote;
		}
		else if (!isInQuote && 
				(*pszInput == '[' || *pszInput == ']' || *pszInput == ',' || 
					*pszInput == '(' || *pszInput == ')' ))
		{
			break;
		}
		else if (!isInQuote && 
				(*pszInput == ' ' || *pszInput == '\t' || 
				*pszInput == 10 || *pszInput == 13))
		{
		/* just skip over whitespace */
		} 
		else
		{
			token += *pszInput;
			++sizeToken;
		}
		pszInput++;
	}
	if (*pszInput == '\0')
		return true;
		
	tokens.push_back(token);
	
	bool result;  // read sublist
	if( *pszInput == '[' || *pszInput == '(' )
	{
		do
		{
			pszInput++; // Skip bracket or comma.
			result = tokenizeWKT((char **) &pszInput, tokens);
			if (!result )
				return result;          
		}while (*pszInput == ',');
	
		if (*pszInput != ')' && *pszInput != ']')
			return result;
		pszInput++;
	}
	*wkt = (char *) pszInput;
	return true;
}



TeProjection* 
TeGetTeProjectionFromWKT(const string& wkt)
{
	map<string,string> wktToTeProj;
	wktToTeProj["Albers_Conic_Equal_Area"] = "Albers";
	wktToTeProj["Lambert_Conformal_Conic_1SP"] = "LambertConformal";
	wktToTeProj["Mercator_1SP"] = "Mercator";
	wktToTeProj["Miller_Cylindrical"] = "Miller";
	wktToTeProj["Transverse_Mercator"] = "UTM";
	wktToTeProj["Sinusoidal"] = "Sinusoidal";
	wktToTeProj["Polyconic"] = "Polyconic";
	wktToTeProj["Equirectangular"] = "CylindricalEquidistant";
	wktToTeProj["Polar_Stereographic"] = "PolarStereographic";
	wktToTeProj["Satellite"] = "Satellite"; //FAMI

	TeDatum dat = TeDatumFactory::makeFromWKT(wkt);
	vector<string> tokens;
	char* wktchar = new char[wkt.size()];
	strcpy(wktchar,wkt.c_str());
	bool res = tokenizeWKT((char **) &wktchar, tokens);
	if (!res)
		return 0;

	// geographic coordinates 

	TeProjectionParams par;
	unsigned int i = 1;
	bool isProjected = false;
	if (tokens[0] == "GEOGCS")
	{
		isProjected = false;
		par.name = "LatLong";
	}
	else if (tokens[0] == "PROJCS")
	{
		isProjected = true;
	}
	else
		return 0;
	par.datum = dat;
	if (!isProjected)
		return TeProjectionFactory::make(par);

	while (i<tokens.size() && tokens[i] != "PROJECTION")
		++i;
	if (i== tokens.size())
		return 0;
	map<string,string>::iterator it = wktToTeProj.find(tokens[++i]);
	if (it != wktToTeProj.end())
		par.name = it->second;
	else
		return 0;
	while (tokens[++i] == "PARAMETER")
	{
		++i;
		if (TeStringCompare(tokens[i],"latitude_of_origin",false))
			par.lat0 = atof(tokens[++i].c_str())*TeCDR;
		else if (TeStringCompare(tokens[i],"central_meridian",false))
			par.lon0 = atof(tokens[++i].c_str())*TeCDR;
 		else if (TeStringCompare(tokens[i],"standard_parallel_1",false))
			par.stlat1 = atof(tokens[++i].c_str())*TeCDR;
 		else if (TeStringCompare(tokens[i],"standard_parallel_2",false))
			par.stlat2 = atof(tokens[++i].c_str())*TeCDR;
 		else if (TeStringCompare(tokens[i],"false_easting",false))
			par.offx = atof(tokens[++i].c_str());
 		else if (TeStringCompare(tokens[i],"false_northing",false))
			par.offy = atof(tokens[++i].c_str());
 		else if (TeStringCompare(tokens[i],"scale_factor",false))
			par.scale = atof(tokens[++i].c_str());
	}
	return TeProjectionFactory::make(par);
}

