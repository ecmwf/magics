/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

magics_height = 384.; 
magics_left = 60.; 
magics_outside = function (x, y) { 
	if (smaller(x, 49.9109) )
		return true;
	if (greater(x, 615.567) )
		return true;
	if (smaller(y, 96.05) )
		return true;
	if (greater(y, 480.25) )
		return true;
	return false;
}
; 
magics_reprojection = function (x, y)
{
	return {
		lon: (x -  49.9109 ) * ( 125 /  565.657 ) + (-140),
		lat: ( 96.05 - y) * ( 85 / 384.2) + (80)
	}
}
; 
magics_top = 96.; 
magics_version = "2.6.0"; 
magics_width = 565.; 
