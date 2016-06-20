/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

magics_height = 371.; 
magics_left = 60; 
magics_outside = function (x, y) { 
	if (smaller(x, 60) )
		return true;
	if (greater(x, 740) )
		return true;
	if (smaller(y, 92.6196) )
		return true;
	if (greater(y, 463.098) )
		return true;
	return false;
}
; 
magics_reprojection = function (x, y)
{
	return {
		lon: (x -  60 ) * ( 110 /  680 ) + (80),
		lat: ( 92.6196 - y) * ( 60 / 370.479) + (5)
	}
}
; 
magics_top = 110.; 
magics_version = "2.6.0"; 
magics_width = 680; 
