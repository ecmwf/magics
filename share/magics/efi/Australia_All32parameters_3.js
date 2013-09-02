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
