
if (!ECMWF) {
    var ECMWF = {};
}


ECMWF.Magics = function () {
    ECMWF.MagicsService.url='/services/MagicsService';    
    this.handler = new ECMWF.MagicsService.Plot();
    
    this.first= true;
    smaller = function(a, b) {
        return a<b;
    }
    greater = function(a, b) {
        return a> b;
    }
    equal = function(a, b) {
        return a == b;
    }
    and = function(a, b) {
        return a && b;
    }
    this.round = function(number)
    {
        number *= Math.pow(10,2);
	    number = Math.floor(number);
	    number /= Math.pow(10,2);
        return number;
    }
    this.execute =  function(request, where, params, dim,  cb) {
         this.parent = where;
         
         if ( this.first) {
           
            $(this.parent).mousemove(function (event) { me.mousemove(event); } );
            $(this.parent).mousedown(function (event) { me.mousedown(event); } ); 
            $(this.parent).mouseup(function (event)   { me.mouseup(event); } ); 
             
            this.first=false; 
        } 
         var me = this;  
         var params = params;
         this.box = $( "<div></div>" );
        // Set the properties on the fade level.
         this.box.css(
         {
            backgroundColor: "yellow",
            opacity: 0.8,
            top: "10px",
            left: "10px",
            color:'black',
            fontSize: 'x-small', 
            position: "absolute",
            border:'solid yellow small',
            visibility:'hidden'
            }
         )
        .width( '80px' )
        .height( '15px' )
        .html('toolkit')
        ;   
         this.mousemove = this.empty;
         this.mouseup = this.empty;
         this.mousedown = this.empty; 
         
         ECMWF.HTTP.get(request, function (out) {     
                me.handler.request(out);
                   
                for(key in params) {
                    //console.log("key", key, "value", params[key]);
                    me.set(key, params[key]);
                }
                me.handler.render(function (out) {
                    out = out.replace(/<\?xml.+?\?>/, '');
                    var url = $(out).find("file[type='gif']").attr('url');
                    //$(me.parent).html('<img src="' + url + '" width="' + dim + '%" height="' + dim + '%"/>');
                    if ( dim > 0 ) 
                        $(me.parent).html('<img src="' + url + '" width="' + dim + '%" height="' + dim + '%"/>'); 
                    else 
                        $(me.parent).html('<img src="' + url + '"/>');
                        
                    me.reproject = eval($(out).find("reprojection").html()); 
                    
                    me.outside = eval($(out).find("outside").html());  
                                    
                     $(me.parent).append(me.box); 
                    cb()
                                
                }); 
            }); 
           
            
    }
   
  this.mode = function() {}
    
    this.set = function(param, value) {
        this.handler._request=this.handler.parameter(this.handler._request, param, value);
    }
    
    this.empty = function(event) {   }
    
    this.position = function(event) {
        var origin = $(this.parent).offset();  
        return this.reproject(event.pageX - origin.left, event.pageY - origin.top);
    }
    this.ignore = function(event) {
        var origin = $(this.parent).offset();  
        return this.outside(event.pageX - origin.left, event.pageY - origin.top);
    }
    
    
    this.tooltipmode = function() {
        
        this.box.css(
         {
            backgroundColor: "yellow",
            opacity: 0.8,
          
            color:'black',
            fontSize: 'x-small', 
            position: "absolute",
            border:'solid yellow small',
            visibility:'visible'
            }
         )
         .width( '80px' )
         .height( '15px' );     
       
         this.mousemove=this.tooltip;
         this.mouseup = this.empty;
         this.mousedown= this.empty;            
    }
    
  
    this.tooltip = function(event) {
        if ( this.ignore(event) )return; 
        var point = this.position(event);   
        var lat = this.round(point.lat);
        var lon = this.round(point.lon); 
        this.box.css(
        {
            backgroundColor: "yellow",
            opacity: 0.8,
            visibility:'visible',
            color:'black',
            fontSize: 'x-small', 
            position: "absolute",
            border:'solid yellow small',
            top: (event.pageY + 10)+"px",
            left: (event.pageX +10) +"px",
            border:'solid yellow small',
        })
        .html(lat + '/' + lon)
        ;
        //this.ontooltip(lat, lon);
        
    }
    
    this.ontooltip = function() {}
    
    
   
    this.pointselectionmode = function() {
        this.box.css(
         {
            backgroundColor: "yellow",
            opacity: 0.5,
            color:'yellow',
            position: "absolute",
            border:'solid red medium'
            }
        )
        .width('2px')
        .height('2px')
        .html("");
        
        this.mouseup = this.pointselection;   
        this.mousedown = this.empty;
        this.mousemove = this.empty; 
        this.box.css ({ cursor: 'auto' });         
    }
    
    this.onpointselection = function() {}
    
    this.pointselection = function(event) {
        if ( this.ignore(event) ) return;
        this.box.css(
        {
            backgroundColor: "yellow",
            opacity: 0.5,
            color:'yellow',
            position: "absolute",
            border:'solid red medium',
            top: (event.pageY)+"px",
            left: (event.pageX)+"px",
            visibility:'visible',
            border:'dashed red medium',
        });
        var point = this.position(event);   
        var lat = this.round(point.lat);
        var lon = this.round(point.lon); 
        this.onpointselection(lat, lon);  
        this.mousedown = this.empty;
        this.mousemove = this.empty;
     }
    
    
    this.areaselectionmode = function() {
       
        //this.box.hide();
        //console.log("areaselectionmode");
        this.mouseup = this.empty;   
        this.mousedown = this.startarea;
        this.mousemove = this.empty;      
    }
    
    this.onpointselection = function() {}
    
  
    
   
    
    this.startarea = function(event) {
        if ( this.ignore(event) ) return;
          this.box.css(
         {
            backgroundColor: "yellow",
            opacity: 0.5,
            color:'yellow',
            position: "absolute",
            border:'dashed red medium',
            top: (event.pageY)+"px",
            left: (event.pageX)+"px",
            visibility:'visible'
            }
        )
        .width('10px')
        .height('10px')
        .html("");
         
         
        this.x=event.pageX;
        this.y=event.pageY; 
        
        this.origin = $(this.parent).offset();  
        this.left = this.x;
        this.right =  this.left + 10;
        this.top = this.y;
        this.bottom = this.top + 10;  
        //console.log(this.left + "--");
       //console.log(this.right+ "--");
    //console.log(this.top+ "--");
    //console.log(this.bottom+ "--");
        
         
        this.mousedown = this.startarea;
        this.mousemove = this.resizearea; 
        this.mouseup = this.empty;  
        
    }
    
    this.resizearea = function(event) {
         if ( this.ignore(event) ) return;
         this.box
        .width( (event.pageX-this.x) +'px')
        .height((event.pageY-this.y) + 'px');
        this.mousedown = this.endarea;
        this.right= this.left + event.pageX-this.x;       
        this.bottom = this.top + event.pageY - this.y;
        
        
      
    }
    
    this.endarea = function(event) {
    if ( this.ignore(event) ) return;
         var origin = $(this.parent).offset();  
         this.box
        .width( (event.pageX-this.x) +'px')
        .height((event.pageY-this.y) + 'px')
        ;
       
        this.right= this.left + event.pageX-this.x;       
        this.bottom = this.top + event.pageY - this.y; 
        
        var point1 = this.reproject(this.x - origin.left, this.y - origin.top); 
        var lat1=point1.lat;
        var lon1=point1.lon;
        var point2=this.reproject(event.pageX - origin.left, event.pageY - origin.top);
        var lat2=point2.lat;
        var lon2=point2.lon;
        this.onselectarea(this.round(lat1), this.round(lon1), this.round(lat2), this.round(lon2)); 
       
        this.mousedown = this.movearea;
        this.mousemove = this.cursor;
        this.mouseup = this.empty;
        
    }
    
  this.mouse = function(event)
{ 
 
    this.mx = event.pageX ;
    this.my = event.pageY;
}  

    this.cursor =  function(event)
    {
     this.mouse(event);
     var gap = 10;
    
  
   
    if ( (this.mx - this.left) < gap && (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'nw-resize' });         
        return;
    }
    if ( (this.right - this.mx) < gap && (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'ne-resize' });        
        return;
    }
    
    if (  (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'n-resize' });
        
        return;
    }  
    if ( (this.mx - this.left) < gap && (this.bottom - this.my) < gap ) {
        this.box.css ( { cursor: 'sw-resize' });
        
        return;
    }
    if ( (this.mx - this.left) < gap ) {
       this.box.css ({ cursor: 'w-resize' });
       
        
        return;
    }
    if ( (this.right - this.mx) < gap && (this.bottom - this.my) < gap ) {
         this.box.css ({ cursor: 'se-resize' });
         
        
        return;
    }
    if ( ( this.bottom - this.my) < gap ) {
        this.box.css( { cursor: 's-resize' });
        
       
        return;
    }
    if ( ( this.right - this.mx) < gap ) {
        this.box.css ({ cursor: 'e-resize' });
        
       
        return;
    }
    
    
    
    this.box.css ({ cursor: 'move' });
}
    
    
    this.movearea = function(event)
{
   
    
    this.mouse(event);
  
    
 
    var gap = 10;
    this.mouseup = this.stop;
  
   
    if ( (this.mx - this.left) < gap && (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'nw-resize' });
        //console.log('nw-resize');
        this.mousemove = this.nwresize;       
        return;
    }
    if ( (this.right - this.mx) < gap && (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'ne-resize' });
        //console.log('ne-resize');
        this.mousemove = this.neresize;  
        return;
    }
    
    if (  (this.my - this.top) < gap ) {
        this.box.css ({ cursor: 'n-resize' });
        //console.log('n-resize');
        this.mousemove = this.nresize; 
        return;
    }  
    if ( (this.mx - this.left) < gap && (this.bottom - this.my) < gap ) {
        this.box.css ( { cursor: 'sw-resize' });
        //console.log('sw-resize');
        this.mousemove = this.swresize; 
        return;
    }
    if ( (this.mx - this.left) < gap ) {
       this.box.css ({ cursor: 'w-resize' });
       //console.log('w-resize');
        this.mousemove = this.wresize; 
        
        return;
    }
    if ( (this.right - this.mx) < gap && (this.bottom - this.my) < gap ) {
         this.box.css ({ cursor: 'se-resize' });
         //console.log('se-resize');
        this.mousemove = this.seresize; 
        
        return;
    }
    if ( ( this.bottom - this.my) < gap ) {
        this.box.css( { cursor: 's-resize' });
        //console.log('s-resize');
        this.mousemove = this.sresize; 
       
        return;
    }
    if ( ( this.right - this.mx) < gap ) {
        this.box.css ({ cursor: 'e-resize' });
        this.mousemove = this.eresize;
        //console.log('e-resize'); 
       
        return;
    }
    //console.log('mpve');
    this.shiftx =  event.pageX - this.left;
    this.shifty =  event.pageY - this.top;
     
    this.mousemove = this.move; 
    this.box.css ({ cursor: 'move' });
}



this.resize = function()
{ 
    // make sure we do not go out of the map! 
    //if ( this.left < this.magics.left ) this.left = this.magics.left;
    //if ( this.right > this.magics.right ) this.right = this.magics.right;
    //if ( this.top < this.magics.top ) this.top = this.magics.top;
    //if ( this.bottom > this.magics.bottom ) this.bottom = this.magics.bottom;
     
     var origin = $(this.parent).offset();  
    this.box.css( {
        left: (this.left) +'px',
        top: (this.top) + 'px'
      })
      .width((this.right-this.left)+'px')
      .height((this.bottom-this.top)+'px');
     
    
    
}

this.nwresize = function(event)
{
    this.mouse(event);
    this.top = this.my - 5;
    this.left = this.mx - 5;    
    this.resize();
}

this.nresize = function(event)
{this.mouse(event);
    this.top = this.my + 5;   
    this.resize();
}
this.wresize = function(event)
{this.mouse(event);
    this.left = this.mx - 5;   
    this.resize();
}

this.neresize = function(event)
{this.mouse(event);
    this.top = this.my - 5;
    this.right = this.mx + 5;    
    this.resize();
}

this.swresize = function(event)
{
    this.mouse(event);
    this.bottom = this.my + 5;
    this.left = this.mx - 5;    
    this.resize();
}
this.seresize = function(event)
{
    this.mouse(event);this.bottom = this.my + 5;
    this.right = this.mx + 5;    
    this.resize();
}
this.sresize = function(event)
{
    this.mouse(event);this.bottom = this.my + 5;
    this.resize();
}
this.eresize = function(event)
{
    this.mouse(event);this.right = this.mx + 5;    
    this.resize();
}



this.stop = function(event)
{

    this.mousemove = this.cursor;
    //console.log("stop");
    
    
         var origin = $(this.parent).offset();  
       
        var point1 = this.reproject(this.left - origin.left, this.top - origin.top); 
        var lat1=point1.lat;
        var lon1=point1.lon;
        var point2=this.reproject(this.right - origin.left, this.bottom - origin.top);
        var lat2=point2.lat;
        var lon2=point2.lon;
        this.onselectarea(this.round(lat1), this.round(lon1), this.round(lat2), this.round(lon2)); 
       
        this.mousedown = this.movearea;
        this.mousemove = this.cursor;
        this.mouseup = this.empty;
        
    
}

this.move = function(event)
{
    
    this.mouse(event);
    
    
    
    var height = this.bottom -this.top;
    var width = this.right -this.left;
    
    var newshiftx =  event.pageX - this.left ;
    var newshifty =  event.pageY - this.top;
    
    this.left = this.left + (newshiftx - this.shiftx);
    this.top = this.top + (newshifty - this.shifty);
    this.shiftx= event.pageX-this.left;
    this.shifty= event.pageY-this.top;
    
    
    this.right = this.left + width;
    this.bottom = this.top + height;
    
    this.resize();
    
    
    
}
    
}  
