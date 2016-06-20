/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

magics_height = 340.; 
magics_left = 60; 
magics_outside = function (x, y) { 
	if (smaller(x, 60) )
		return true;
	if (greater(x, 740) )
		return true;
	if (smaller(y, 113.202) )
		return true;
	if (greater(y, 452.807) )
		return true;
	return false;
}
; 
magics_reprojection = function (x, y)
{
	return {
		lon: (x -  60 ) * ( 360 /  680 ) + (-180),
		lat: ( 113.202 - y) * ( 180 / 339.605) + (90)
	}
}
; 
magics_top = 140.; 
magics_version = "2.6.0"; 
magics_width = 680; 
