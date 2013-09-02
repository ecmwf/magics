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
