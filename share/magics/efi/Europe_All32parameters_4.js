/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

magics_height = 358.; 
magics_left = 60; 
magics_outside = function (x, y) { 
	if (smaller(x, 60) )
		return true;
	if (greater(x, 740) )
		return true;
	if (smaller(y, 89.3861) )
		return true;
	if (greater(y, 446.931) )
		return true;
	return false;
}
; 
magics_reprojection = function (xd, yd)
{
	yd = 446.931  -  yd; 
	xpc = (xd -  60 ) * ( 9.14838e+06 /  680) + -4.91719e+06;
	ypc = yd  * ( 4.81581e+06 /   357.545) +  -6.52533e+06;
	px = xpc - 0;
	py = ypc - 0;
	ro = Math.sqrt(px*px + py*py);
	t = (ro* 1)/(1.18883e+07) ;
	xx = 1.5708 - 2.*Math.atan(t);
	yll = xx + 0*Math.sin(2*xx) + 0*Math.sin(4*xx) + 0*Math.sin(6*xx);
	if (py != 0.) 
		xll = 0+ Math.atan(px/(-py));
	if (and(greater(px, 0.), greater(py,0.)))
		xll = xll + 3.14159;
	else if (and(smaller(px,0.), greater(py,0.)))
		xll = xll - 3.14159;
	else if (and(greater(px, 0.), equal(py,0.)))
		xll = 0+ 3.14159/ 2.;
	else if (and(smaller(px, 0.),  equal(py,0.)))
		xll = 0 - 3.14159/ 2.;
	else if (and(equal(px,0.), equal(py, 0.)))
		xll = 0;
	if (smaller(xll, (-3.14159) )) xll += 2.* 3.14159;
	else if (greater(xll, 3.14159)  )    xll -= 2.*3.14159;
	xll = xll * 57.2958;
	yll = yll * 57.2958;

	return { lon :  xll, lat : yll } ;
}
; 
magics_top = 122.; 
magics_version = "2.6.0"; 
magics_width = 680; 
