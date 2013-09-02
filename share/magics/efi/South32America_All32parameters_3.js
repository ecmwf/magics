magics_height = 385.; 
magics_left = 60.; 
magics_outside = function (x, y) { 
	if (smaller(x, 51.9073) )
		return true;
	if (greater(x, 640.19) )
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
		lon: (x -  51.9073 ) * ( 130 /  588.283 ) + (-125),
		lat: ( 96.05 - y) * ( 85 / 384.2) + (20)
	}
}
; 
magics_top = 96.; 
magics_version = "2.6.0"; 
magics_width = 590.; 
