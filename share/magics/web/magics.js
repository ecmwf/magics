/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*jslint white: false, onevar:false */

function magics_factory(params) {


  console.info("magics_factory" + params);
  function round(x, prec) {
    if(!prec) { prec = 100; }
    return Math.floor(x*prec+0.5)/prec;
  }

  var magics = params;
  var projections = {};

  projections.cylindrical = function() {

    var cylindrical = params.projection;

    cylindrical.pcxmin= parseFloat(params.projection.pcxmin);
    cylindrical.pcymin= parseFloat(params.projection.pcymin);
    cylindrical.pcwidth= parseFloat(params.projection.pcwidth);
    cylindrical.pcheight= parseFloat(params.projection.pcheight);
    cylindrical.width= parseFloat(params.projection.width);
    cylindrical.height= parseFloat(params.projection.height);
    cylindrical.pxwidth= parseFloat(params.projection.img_width);
    cylindrical.pxheight= parseFloat(params.projection.img_height);
    cylindrical.top= parseFloat(params.projection.top);
    cylindrical.left= parseFloat(params.projection.left);
    cylindrical.zoomout= true;
    cylindrical.name = "cylindrical";
	
    
    cylindrical.pc2ll = function(x, y) {

      return {
        lon: (x - cylindrical.left) * (cylindrical.pcwidth / cylindrical.pxwidth) + (cylindrical.pcxmin),
        lat: (cylindrical.top - y) * (cylindrical.pcheight / cylindrical.pxheight) + (cylindrical.pcheight + cylindrical.pcymin)
      };
    };
    
    cylindrical.pc2xy = cylindrical.pc2ll
    cylindrical.ll2pc = function(lat, lon) {
      return {
        x: (lon - cylindrical.pcxmin) * (cylindrical.pxwidth / cylindrical.pcwidth) + (cylindrical.left),
        y: -(lat - cylindrical.pcymin) * (cylindrical.pxheight / cylindrical.pcheight) + (cylindrical.top + cylindrical.height)
      };
    };

    cylindrical.new_projection = function(ll, ur) {
      if ( ur.lon > 720 ) {
        cylindrical.zoomout= false;
		ur.lon = 720
      }
      if ( ur.lat > 90 ) {
        cylindrical.zoomout= false;
		ur.lat = 90 
      }
      if ( ll.lon < -360 ) {
        cylindrical.zoomout= false;
	 	ll.lon = -360
      }
      if ( ll.lat < -90 ) {
        cylindrical.zoomout= false;
		ll.lat = -90
      }
      var p=  {
        "projection": "cylindrical",
        "upper_right_longitude": String(round(ur.lon, 1000)),
        "upper_right_latitude": String(round(ur.lat, 1000)),
        "lower_left_longitude": String(round(ll.lon, 1000)),
        "lower_left_latitude": String(round(ll.lat, 1000))
      };
      
      p.zoomout = true;
      p.zoomin = true;
      if ( ur.lon - ll.lon > 1000) 
        p.zoomout = false;
      if ( ur.lon - ll.lon < 10)  
        p.zoomin = false;
      if ( ur.lat - ll.lat < 10)
        p.zoomin = false;
      
      return p;
    };

    cylindrical.new_projection_from_vertical_longitude = function(lon) {
      var ll = cylindrical.pc2ll(0,cylindrical.pxheight);
      var ur = cylindrical.pc2ll(cylindrical.pxwidth,0);
      var p =  cylindrical.new_projection(ll, ur);
      return p;
    };

    cylindrical.full_map_projection_name = function() {
        return "global";
    };

    return cylindrical;

  };
    projections.cartesian = function() {

    var cartesian = params.projection;

    cartesian.pcxmin= parseFloat(params.projection.pcxmin);
    cartesian.pcymin= parseFloat(params.projection.pcymin);
    cartesian.pcwidth= parseFloat(params.projection.pcwidth);
    cartesian.pcheight= parseFloat(params.projection.pcheight);
    cartesian.width= parseFloat(params.projection.width);
    cartesian.height= parseFloat(params.projection.height);
    cartesian.top= parseFloat(params.projection.top);
    cartesian.left= parseFloat(params.projection.left);
    
	cartesian.pxwidth= parseFloat(params.projection.img_width);
    cartesian.pxheight= parseFloat(params.projection.img_height);
    
    console.log("definition projection->" + JSON.stringify(cartesian));
   
    
    
	cartesian.x_date_setting = function() {
	
		cartesian.subpage_x_date_min = params.projection.subpage_x_date_min.substr(0, 10)
	}
	cartesian.x_regular_setting = function() {
		
	}
	cartesian.y_regular_setting = function() {
		
	}
	cartesian.y_date_setting = function() {
		cartesian.subpage_y_date_min = params.projection.subpage_y_date_min.substr(0, 10)
	}
	
    
    cartesian.x_date_definition = function(def, min, max) {
        console.log("x_date_min->" + cartesian.x_date_min);
        j = cartesian.subpage_x_date_min.split("-"); 
        var base = new Date(j[0], j[1]-1,j[2]); 
        
        var dmin = new Date()
		console.log("base->" + base.format("yyyy-mm-dd"));
        dmin.setTime(base.getTime() + min*1000);
        var dmax = new Date()
        dmax.setTime(base.getTime() + max*1000);
         
        dmax.format("yyyy-mm-dd");
        
        console.log("min->" + min);
		console.log("min->" + dmin.format("yyyy-mm-dd"));
        def.x_date = {
            x_date_min : dmin.format("yyyy-mm-dd"),
            x_date_max : dmax.format('yyyy-mm-dd'),
            x_automatic : 'off'
        }
        return def
    }
    cartesian.x_date_format = function(x) {
        j = cartesian.subpage_x_date_min.split("-"); 
        var base = new Date(j[0], j[1], j[2]) ;
        
        var date = new Date();
        date.setTime(base.getTime() + x*1000);
        return date.format("yyyy-mm-dd HH:MM:00")

    }
	cartesian.x_regular_format = function(x) {  
        
        return x
    }
    
    cartesian.x_regular_definition = function(def, min, max) {
         
        def.x_regular = {
            x_min :  min,
            x_max :  max,
            x_automatic : 'off'
        }
       
        return def;
    }
    cartesian.y_regular_format = function(y) {  
        
        return y
    }
    
    cartesian.y_regular_definition = function(def, min, max) {
         
        def.y_regular = {
            y_min :  min,
            y_max :  max,
            y_automatic : 'off'
        }
        
        return def;
    }
	cartesian.x_definition = cartesian[params.projection.subpage_x_axis_type+"_definition"]
    cartesian.y_definition = cartesian[params.projection.subpage_y_axis_type+"_definition"]
    cartesian.x_format = cartesian[params.projection.subpage_x_axis_type+"_format"]
    cartesian.y_format = cartesian[params.projection.subpage_y_axis_type+"_format"]
	cartesian.y_setting = cartesian[params.projection.subpage_y_axis_type+"_setting"]
	cartesian.x_setting = cartesian[params.projection.subpage_x_axis_type+"_setting"]
	
	cartesian.x_setting();
	cartesian.y_setting();
    cartesian.name = "cartesian";
    cartesian.pc2ll = function(x, y) {
      
    // Origine of the coordinates system should be  at the top left corner! 
      // Check the boundaries of the map!
		
	    if ( x < cartesian.left ) x = cartesian.left
		if ( x > cartesian.left + cartesian.pxwidth) 
			x = cartesian.left+ cartesian.pxwidth;
		if ( y < cartesian.top ) y = cartesian.top
		if ( y > cartesian.top + cartesian.pxheight) 
			y = cartesian.top+ cartesian.pxheight;
		
      
      return {
        lon: (x - cartesian.left) * (cartesian.pcwidth / cartesian.pxwidth) + (cartesian.pcxmin),
        lat:  (cartesian.top - y) * (cartesian.pcheight / cartesian.pxheight) + (cartesian.pcheight + cartesian.pcymin)
      }
    };
    
    cartesian.pc2xy = function(x, y) {
	// Check the boundaries of the map!
		
	    if ( x < cartesian.left ) x = cartesian.left
		if ( x > cartesian.left + cartesian.pxwidth) 
			x = cartesian.left+ cartesian.pxwidth;
		if ( y < cartesian.top ) y = cartesian.top
		if ( y > cartesian.top + cartesian.pxheight) 
			y = cartesian.top+ cartesian.pxheight;
		
				
        var ll = cartesian.pc2ll(x, y);
        return {
            x: cartesian.x_format(ll.lon),
            y: cartesian.y_format(ll.lat)
        }
    };
     
        
    cartesian.ll2pc = function(lat, lon) {
      return {
        x: (lon - cartesian.pcxmin) * (cartesian.pxwidth / cartesian.pcwidth) + (cartesian.left),
        y: -(lat - cartesian.pcymin) * (cartesian.pxheight / cartesian.pcheight) + (cartesian.top + cartesian.pxheight)
      };
    };

    cartesian.new_projection = function(ll, ur) {
      if ( ll.lon == ur.lon) {
	  	// we return the initial projection
	  	ll.lon = cartesian.pcxmin;
		ur.lon = ll.lon  + cartesian.pcwidth
		ll.lat = cartesian.pcymin;
		ur.lat = ll.lat  + cartesian.pcheight
	  }
	  if ( ll.lat == ur.lat) {
	  // we return the initial projection
	  	ll.lon = cartesian.pcxmin;
		ur.lon = ll.lon  + cartesian.pcwidth
		ll.lat = cartesian.pcymin;
		ur.lat = ll.lat  + cartesian.pcheight
	  }
	  
	  var p=  {
	  "projection": "cartesian"
	  };
      p.cartesian = {
        name: "cartesian"
      }
      
      cartesian.x_definition(p.cartesian, ll.lon, ur.lon)
      cartesian.y_definition(p.cartesian, ll.lat, ur.lat);
      console.log("new projection->" + JSON.stringify(p.cartesian));
      return p;
    };

    cartesian.new_projection_from_vertical_longitude = function(lon) {
      var ll = cartesian.pc2ll(0,cartesian.pxheight);
      var ur = cartesian.pc2ll(cartesian.pxwidth,0);
      var p =  cartesian.new_projection(ll, ur);
      return p;
    };

    cartesian.full_map_projection_name = function() {
        return "global";
    };

    return cartesian;

  };

  projections.polar_north = function() {

    var polar = params.projection;

    polar.pcxmin= parseFloat(params.projection.pcxmin);
    polar.pcymin= parseFloat(params.projection.pcymin);
    polar.pcwidth= parseFloat(params.projection.pcwidth);
    polar.pcheight= parseFloat(params.projection.pcheight);
    polar.width= parseFloat(params.projection.width);
    polar.height= parseFloat(params.projection.height);
    polar.pxwidth= parseFloat(params.projection.img_width);
    polar.pxheight= parseFloat(params.projection.img_height);
    polar.top= parseFloat(params.projection.top);
    polar.left= parseFloat(params.projection.left);

    polar.name = "polar_stereographic";
    polar.hemisphere = "north";

    polar.pc2ll = function(xd, yd) {

      var xpc = (xd - polar.left) * (polar.pcwidth / polar.pxwidth) + polar.pcxmin;
      var ypc = (polar.pxheight - yd) * (polar.pcheight / polar.pxheight) + polar.pcymin;
      var px = xpc - polar.GPoffx;
      var py = ypc - polar.GPoffy;
      var ro = Math.sqrt(px * px + py * py);
      var t = (ro * Math.sqrt(polar.coeff1)) / (polar.coeff2);
      var xx = Math.PI / 2.0 - 2.0 * Math.atan(t);
      var ptpcy = xx + polar.coeff3 * Math.sin(2 * xx) + polar.coeff4 * Math.sin(4 * xx) + polar.coeff5 * Math.sin(6 * xx);
      var ptpcx;

      if (py !== 0.0) {
        ptpcx = polar.lon0 + Math.atan(px / (-py));
      }

      if (px > 0.0 && py > 0.0) {
        ptpcx = ptpcx + Math.PI;
      } else if (px < 0.0 && py > 0.0) {
        ptpcx = ptpcx - Math.PI;
      } else if (px > 0.0 && py === 0.0) {
        ptpcx = polar.lon0 + Math.PI / 2.0;
      } else if (px < 0.0 && py === 0.0) {
        ptpcx = polar.lon0 - Math.PI / 2.0;
      } else if (px === 0.0 && py === 0.0) {
        ptpcx = polar.lon0;
      }

      if (ptpcx < (-Math.PI)) {
        ptpcx += 2.0 * Math.PI;
      } else if (ptpcx > Math.PI) {
        ptpcx -= 2.0 * Math.PI;
      }

      return {
        lon: ptpcx * 180.0 / Math.PI,
        lat: ptpcy * 180.0 / Math.PI
      };

    };
    polar.pc2xy = polar.pc2ll
    polar.ll2pc = function(lat, lon) {

      lat = lat * Math.PI / 180.0;
      lon = lon * Math.PI / 180.0;

      var aux1 = (1.0 - polar.e * Math.sin(lat)) / (1.0 + polar.e * Math.sin(lat));
      var t = Math.tan((Math.PI / 4.0) - (lat / 2.0)) / Math.pow(aux1, (polar.e / 2.0));
      var ro = polar.coeff2 * t / Math.sqrt(polar.coeff6 * polar.coeff7);
      var x = ro * Math.sin(lon - polar.lon0) + aux1 + polar.GPoffx;
      var y = -ro * Math.cos(lon - polar.lon0) + polar.GPoffy;

      return {
        x: (x - polar.pcxmin) * (polar.pxwidth / polar.pcwidth) + (polar.left),
        y: -(y - polar.pcymin) * (polar.pxheight / polar.pcheight) + (polar.top + polar.pxheight)
      };

    };

    polar.new_projection = function(ll, ur) {
      return {
        "projection": "polar_stereographic",
        "map_hemisphere": "north",
        "map_vertical_longitude": String(round(polar.lon0 * 180.0 / Math.PI, 1000)),
        "upper_right_longitude": String(round(ur.lon, 1000)),
        "upper_right_latitude": String(round(ur.lat, 1000)),
        "lower_left_longitude": String(round(ll.lon, 1000)),
        "lower_left_latitude": String(round(ll.lat, 1000))
      };
    };

    polar.new_projection_from_vertical_longitude = function(lon) {

      var delta = (polar.lon0 * 180.0 / Math.PI) - lon;

      var ll = polar.pc2ll(0,polar.pxheight);
      var ur = polar.pc2ll(polar.pxwidth,0);

      return {
        "projection": "polar_stereographic",
        "map_hemisphere": "north",
        "map_vertical_longitude": String(round(lon, 1000)),
        "upper_right_longitude":  String(round(ur.lon - delta, 1000)),
        "upper_right_latitude":   String(round(ur.lat, 1000)),
        "lower_left_longitude":   String(round(ll.lon - delta, 1000)),
        "lower_left_latitude":    String(round(ll.lat, 1000))
      };

    };

    polar.full_map_projection_name = function() {
        return "north_hemisphere";
    };

    return polar;
  };

  projections.polar_south = function() {

    var polar = params.projection;

    polar.pcxmin= parseFloat(params.projection.pcxmin);
    polar.pcymin= parseFloat(params.projection.pcymin);
    polar.pcwidth= parseFloat(params.projection.pcwidth);
    polar.pcheight= parseFloat(params.projection.pcheight);
    polar.width= parseFloat(params.projection.width);
    polar.height= parseFloat(params.projection.height);
    polar.pxwidth= parseFloat(params.projection.img_width);
    polar.pxheight= parseFloat(params.projection.img_height);
    polar.top= parseFloat(params.projection.top);
    polar.left= parseFloat(params.projection.left);

    polar.name = "polar_stereographic";
    polar.hemisphere = "south";
    
    polar.pc2ll = function(xd, yd) {
      var xpc = (xd - polar.left) * (polar.pcwidth / polar.pxwidth) + polar.pcxmin;
      var ypc = (polar.pxheight - yd) * (polar.pcheight / polar.pxheight) + polar.pcymin;
      var px = -(xpc - polar.GPoffx);
      var py = -(ypc - polar.GPoffy);
      var ro = Math.sqrt(px * px + py * py);
      var t = (ro * Math.sqrt(polar.coeff1)) / (polar.coeff2);
      var xx = Math.PI / 2.0 - 2.0 * Math.atan(t);
      var ptpcy = xx + polar.coeff3 * Math.sin(2 * xx) + polar.coeff4 * Math.sin(4 * xx) + polar.coeff5 * Math.sin(6 * xx);
      var ptpcx;

      if (py !== 0.0) {
        ptpcx = -polar.lon0 + Math.atan(px / (-py));
      }

      ptpcy *= -1;
      ptpcx *= -1;

      if (px > 0.0 && py < 0.0) {
        ptpcx = ptpcx + Math.PI;
      } else if (px < 0.0 && py < 0.0) {
        ptpcx = ptpcx - Math.PI;
      } else if (px > 0.0 && py === 0.0) {
        ptpcx = -polar.lon0 + Math.PI / 2.0;
      } else if (px < 0.0 && py === 0.0) {
        ptpcx = -polar.lon0 - Math.PI / 2.0;
      } else if (px === 0.0 && py === 0.0) {
        ptpcx = -polar.lon0;
      }

      if (ptpcx < (-Math.PI)) {
        ptpcx += 2.0 * Math.PI;
      } else if (ptpcx > Math.PI) {
        ptpcx -= 2.0 * Math.PI;
      }

      return {
        lon: ptpcx * 180.0 / Math.PI,
        lat: ptpcy * 180.0 / Math.PI
      };
    };
    polar.pc2xy = polar.pc2ll
    polar.ll2pc = function(lat, lon) {

      lat = -lat * Math.PI / 180.0;
      lon = -lon * Math.PI / 180.0;

      var aux1 = (1.0 - polar.e * Math.sin(lat)) / (1.0 + polar.e * Math.sin(lat));
      var t = Math.tan((Math.PI / 4.0) - (lat / 2.0)) / Math.pow(aux1, (polar.e / 2.0));
      var ro = polar.coeff2 * t / Math.sqrt(polar.coeff6 * polar.coeff7);
      var x = ro * Math.sin(lon + polar.lon0) + polar.GPoffx;
      var y = -ro * Math.cos(lon + polar.lon0) + polar.GPoffy;

      return {
        x: (x - polar.pcxmin) * (polar.pxwidth / polar.pcwidth) + (polar.left),
        y: -(y - polar.pcymin) * (polar.pxheight / polar.pcheight) + (polar.top + polar.pxheight)
      };

    };

    polar.new_projection = function(ll, ur) {
      return {
        "projection": "polar_stereographic",
        "map_hemisphere": "south",
        "map_vertical_longitude": String(round(polar.lon0 * 180.0 / Math.PI, 1000)),
        "upper_right_longitude": String(round(ur.lon, 1000)),
        "upper_right_latitude": String(round(ur.lat, 1000)),
        "lower_left_longitude": String(round(ll.lon, 1000)),
        "lower_left_latitude": String(round(ll.lat, 1000))
      };
    };

    polar.new_projection_from_vertical_longitude = function(lon) {
      lon += 180;
      while(lon < 0)   { lon -= 360; }
      while(lon > 180) { lon -= 360; }

      var delta = (polar.lon0 * 180.0 / Math.PI) - lon;

      var ll = polar.pc2ll(0,polar.pxheight);
      var ur = polar.pc2ll(polar.pxwidth,0);

      return {
        "projection": "polar_stereographic",
        "map_hemisphere": "south",
        "map_vertical_longitude": String(round(lon, 1000)),
        "upper_right_longitude":  String(round(ur.lon - delta, 1000)),
        "upper_right_latitude":   String(round(ur.lat, 1000)),
        "lower_left_longitude":   String(round(ll.lon - delta, 1000)),
        "lower_left_latitude":    String(round(ll.lat, 1000))
      };
    };

    polar.full_map_projection_name = function() {
        return "south_hemisphere";
    };

    return polar;

  };

  (function(params) { 
    
    if(!params)  {
		console.log("no projection defined");
		return;
	};
    if(!projections[params.projection.name]){ //console.log('no such projection type:' + params.projection.name); 
    return; }
    
    // This is a private member
    var projection = projections[params.projection.name]();

    magics.width = projection.width;
    magics.height = projection.height;
    magics.pc2ll = projection.pc2ll;
	magics.pc2xy = projection.pc2xy;
    magics.ll2pc = projection.ll2pc;
    magics.ll2xy = projection.ll2xy;

    magics.resize = function(width, height) {

      if(0) {
      if(width === projection.width && height === projection.height) {
        // In this case, we should not recompute, as we would get slightly
        // different values that will upset the caching
        return magics;
      }
      }

      var ll = projection.pc2ll(0,height);
      var ur = projection.pc2ll(width,0);
      return projection.new_projection(ll, ur);
    };

    magics.expand = function(xmargin, ymargin) {
      var ll = projection.pc2ll(-xmargin,  params.projection.height -ymargin);
      var ur = projection.pc2ll(params.projection.width +xmargin,  +ymargin);
      return projection.new_projection(ll, ur);
    };

    magics.move = function(dx, dy) {
      var ll = projection.pc2ll(dx, params.projection.height + dy);
      var ur = projection.pc2ll(params.projection.width + dx, +dy);
      return projection.new_projection(ll, ur);
    };

    magics.tranform = function(matrix) {

      for(var i=1;i<=3;i++) {
        var x = "";
        for(var j=1;j<=3;j++) {
          x += ", " + matrix.e(i,j);
        }
        /*
        console.log(x);
        */
      }

      var ll = $V([0,params.projection.height, 1]);
      var ur = $V([params.projection.width, 0, 1]);

      /*
      console.log("MM " + JSON.stringify(matrix));
      console.log("ll " + JSON.stringify(ll));
      console.log("ur " + JSON.stringify(ur));
      */

      ll = matrix.multiply(ll);
      ur = matrix.multiply(ur);

      /*
      console.log("MM " + JSON.stringify(matrix));
      console.log("ll " + JSON.stringify(ll));
      console.log("ur " + JSON.stringify(ur));
      */

      ll = projection.pc2ll(ll.e(1), ll.e(2));
      ur = projection.pc2ll(ur.e(1), ur.e(2));

      //console.log("ll " + ll.e(1) + " " + ll.e(2) + " " + ll.e(3));
      //console.log("ur " + ur.e(1) + " " + ur.e(2) + " " + ur.e(3));

      return projection.new_projection(ll, ur);
    };

    function zoom3(factor, cx, cy) {
      var nw = (projection.width / factor) / 2;
      var nh = (projection.height / factor) / 2;
      var ll = projection.pc2ll(cx - nw, cy + nh);
      var ur = projection.pc2ll(cx + nw, cy - nh);
      return projection.new_projection(ll, ur);
    }

    function zoom4(ll_x, ll_y, ur_x, ur_y) {
      //console.log("zoom4" + ll_x + " " + ll_y+ " " + ur_x+ " " + ur_y )
      var ll = projection.pc2ll(ll_x, ll_y);
      var ur = projection.pc2ll(ur_x, ur_y);
      return projection.new_projection(ll, ur);
    }

    function zoom2(ll, ur) {
      return zoom4(ll.x, ll.y, ur.x, ur.y);
    }

    function zoom1(factor) {
      var cx = projection.width / 2;
      var cy = projection.height / 2;
      return zoom3(factor, cx, cy);
    }

    magics.full_map_projection_name = function() {
      return projection.full_map_projection_name();
    }

    magics.vertical_longitude = function(lon) {
      return projection.new_projection_from_vertical_longitude(lon);
    }

    magics.zoom = function(a, b, c, d) {
      if (b === undefined && c === undefined && d === undefined) {
        return zoom1(a);
      }
      if (c === undefined && d === undefined) {
        return zoom2(a, b);
      }
      if (d === undefined) {
        return zoom3(a, b, c);
      }
      return zoom4(a, b, c, d);
    };

  } (params));
  return magics;
}
