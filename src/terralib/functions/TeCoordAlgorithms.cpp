#include "TeCoordAlgorithms.h"
#include <math.h>

bool TeLongDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec)
{
	short  	posat,lim = 180;

	double	dd,pi;			
	posat = 0;
	pi = 4.*atan((double)1.);

	// Longitude 
	if(dg < 0 || dg > lim)	
		return false;

	if(mn < 0 || mn > 60)
		return false;

	if(sc < 0. || sc > 60.)
		return false;

	if(lado == "W" || lado == "w"|| lado == "O"|| lado == "o")
	{
		if(dg > 0) dg = dg * -1;
		else if(mn > 0) mn = mn * -1;
		     else sc = sc * -1.;
	}

	dd = (double)(abs(dg)) + ((double)abs(mn)/60.) + fabs(sc)/3600.;
	if (dg < 0 || mn < 0 || (int)sc < 0)
		dd = -dd;
	grauDec = dd;
	return true;
}


bool
TeLatDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec)
{
	short  	posat,lim = 90;

	double	dd,pi;			
	posat = 0;
	pi = 4.*atan((double)1.);

	// Longitude 
	if(dg < 0 || dg > lim)	
		return false;

	if(mn < 0 || mn > 60)
		return false;

	if(sc < 0. || sc > 60.)
		return false;

	if(lado == "s" || lado == "S")
	{
		if(dg > 0) dg = dg * -1;
		else if(mn > 0) mn = mn * -1;
		     else sc = sc * -1.;
	}

	dd = (double)(abs(dg)) + ((double)abs(mn)/60.) + fabs(sc)/3600.;
	if (dg < 0 || mn < 0 || (int)sc < 0)
		dd = -dd;

//	dd = dd*pi/180.;
	grauDec = dd;
	return true;
}
