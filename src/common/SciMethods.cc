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

/*!
    \file SciMethods.cc
    \brief Definition of SciMethods.
    \author Graphics Section, ECMWF

    Started: September 2011
*/

#include <math.h>

#include "SciMethods.h"

namespace magics 
{
  
const double KAPPA=0.285611; //Rd/cp

/*! Computes the distance on Earth in km */
double geoDistanceInKm(double fi1,double la1,double fi2,double la2)
{   
  	double rfi1=RAD(fi1);
	double rla1=RAD(la1);
	double rfi2=RAD(fi2);
	double rla2=RAD(la2);

   	if(rla1 != rla2)
     	{
       		double d=sin(rfi1)*sin(rfi2)+cos(rfi1)*cos(rfi2)*cos(fabs(rla1-rla2));
       		return acos(d)*EarthRadiusInKm;
     	}
	else
	{  
   	      	return fabs(rfi1-rfi2)*EarthRadiusInKm;

	}
}

/*! 
  \brief computes potential temperature

   This method computes the potential temperature for a given
   temperature and pressure.
   
   \param t temperature in K
   \param p pressure in Pa 
   \return potential temperature in K
 */
double theta(double t, double p)
{
	return t*pow(100000./p,KAPPA); 
} 

/*! 
  \brief computes temperature from potential temperature

   This method computes the temperature for a given 
   potential temperature and pressure.
   
   \param th potential temperature in K
   \param p pressure in Pa   
   \return temperature in K
   
*/
double temperatureFromTheta(double th, double p)
{	
   	return th*pow(p/100000.,KAPPA); 
}

/*! 
  \brief computes pressure from potential temperature

   This method computes the pressure for a given 
   potential temperature and temperature.
   
   \param th potential temperature in K
   \param t temperature in K 
   \return pressure in Pa   
*/
double pressureFromTheta(double th, double t)
{	
   	return 100000.*pow(t/th,1./KAPPA); 
}

/*! 
  \brief computes the dew point temperature

   This method computes the dew point
   temperature for a given temperature
   and relative humidity.
   
   \param t  temperature in K
   \param rh relative humidity in %
   \return   dew point temperature in K
*/

double tDew(double t, double rh)
{
	double td;
  	double tc=t-273.16;
  	double a=17.27;
 	double b=237.7;

  	if (rh>=100.) td=tc; 
  	else 
 	{
  		double gamma=a*tc/(b+tc)+log(rh/100.);
    		td=b*gamma/(a-gamma);
   	}
   
  	return td+273.16;
} 

/*! 
  \brief computes the wet bulb temperature
 
  This method computes the wet bulb temperature.

  \param td dew point temperature in K 
  \param t  temperature in K  
  \param p  pressure in Pa
  \return   wet bulb temperature in K   
*/   

double tWet(double td,double t,double p)
{
  	double r=mixingRatio(td,p);  
  	double th=theta(t,p);
	
	double pSec=p;
	for(int i=0; i < 10; i++)
	{
	  	double x=0.02*(temperatureFromMixingRatio(r,pSec)-temperatureFromTheta(th,pSec));
		if(abs(x) < 0.01)
		{
		  	break;
		}
		pSec=pow(2.,x);
	}
	
	double tSec=temperatureFromTheta(th,pSec);
	
	double thSat = thetaEq(tSec,pSec);
	return temperatureFromThetaEq(thSat, p);
}

/*! 
  \brief computes the equivalent potential temperature from initial conditions
  
   \param td  dew point temperature in K
   \param t   temperature in K
   \param p   pressure in Pa
   \return   equivalent potential temperature in K 
*/

double thetaEq(double td,double t,double p)
{
	double tLCL=temperatureLCL(td,t);	
	double th=theta(t,p);	
	double thEq=th * exp(2.6518986 * mixingRatio(td,p)/tLCL);	
 	return thEq;
}

/*! 
  \brief computes the equivalent potential temperature on a saturation adiabat

  This method computes the equivalent potential temperature for a saturation 
  adiabat.
  
  \param t  temperature in K  
  \param p  pressure in Pa
  \return   equvalent potential temperature in K   
*/  

double thetaEq(double t,double p)
{
	return theta(t,p)/exp(-2.6518986*mixingRatio(t,p)/t);
}	

/*! 
  \brief computes the temperature on saturation adiabat

  This method computes the temperature on a saturation adiabat for
  a given equvalent potential temperature and pressure.

  \param thEq equivalent potential temperature   
  \param p    pressure in Pa
  \return     temperature in K 
*/   

double temperatureFromThetaEq(double thEq,double p)
{
  	double tq=253.16;
	double d=120.;
  	for(int i=0; i<12; i++)
	{  
		d/=2;
		double x = thEq*exp(-2.6518986*mixingRatio(tq,p)/tq)-theta(tq,p);	
		if(abs(x) <= 0.0000001)
		{	
			break;
		}	
		tq+=((x<0)?-1:1)*abs(d);			
	}
	
	return tq;
}
 
/*! 
  \brief computes the saturation vapour pressure over water from temperature
 
  This method computes the saturation vapour pressure
  over water from temperature.

  \param t temperature in K 
  \return pressure in mb   
*/   

double saturationVapourPressure(double t)
{
	double c1 = 23.832241-5.02808*log10(t);
   	double c2 = 3.49149-1302.8844/t;
	double c3 = 11.344-0.0303998*t;
  	return (exp10(c1-1.3816E-7*exp10(c3)+8.1328E-3*exp10(c2)-2949.076/t));
} 
  
/*! 
  \brief computes the saturation  mixing ratio

  This method computes the mixing ratio.

  \param t  temperature in K 
  \param p  pressure in Pa
  \return   saturation mixing ratio in g/kg 
*/

double mixingRatio(double t, double p)
{
	double e=saturationVapourPressure(t); 
  	return 622.*e/(0.01*p-e); 
}


double temperatureFromMixingRatio(double r, double p)
{		
  	double pmb=p*0.01;
  	double c = log10(r*pmb/(622.+r));
	
	double tmr=exp10(0.0498646455*c + 2.4082965)-280.23475+
	          38.9114*pow(2.,exp10(0.0915*c)-1.2035);

	return tmr+273.16;
}

/*! 
  \brief computes the temperature of LCL

   This method computes the temperature of
   the lifted condensation level (LCL) 
   
   \param td  dew point temperature in K
   \param tf  temperature in K
   \return   LCL temperature in Pa
*/

double temperatureLCL(double td,double t)
{
  double tdc=td-273.16;
  double tLCL=tdc-(0.001296*tdc+0.1963)*(t-td);
  
  return tLCL+273.5;
} 

/*! 
  \brief computes the pressure of LCL

   This method computes the pressure of
   the lifted condensation level (LCL) 
   
   \param td  dew point temperature in K
   \param t  temperature in K
   \param p   pressure in Pa
   \return   LCL pressure in Pa
*/

double pressureLCL(double td, double t, double p)
{
  	double pLCL;
  	double tLCL=temperatureLCL(td,t);

	if (tLCL == t) pLCL=p;
  	else 
    	{ 
    		pLCL=p*pow(tLCL/t,3.5); 
    	}
  
	return pLCL;
} 


} //end namespace
