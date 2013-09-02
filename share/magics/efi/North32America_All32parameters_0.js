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
