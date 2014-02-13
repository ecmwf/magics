var hexa = { 
	"white": "#FFFFFF",
	"black": "#000000",
	"blue": "#0000FF",
	"red": "#FF0000"
	};


var parameters

var find = function(name) {
	var found
	AJS.$.each(parameters, function(i, param) {
		
		if (param.name == name ) {
			found = param
		}
	});
	return found;
}

var unactive = function(name) {
    return  (AJS.$("#" + name).attr("class") == "unactive");
}
var toggle = function(def) 
{
    
    var id = def.name;
    $option = AJS.$('<td>').attr({'id': id});
    
    AJS.$.each(def.values, function(i, value) { 
        
        var id = def.parent + "_" + value;
        
       
        var $toggle =  AJS.$('<input>').attr({'id': id,
               'type': "button", 
                'class': (value == def.default) ? "radio_on" : "radio",
                'name': def.name,
                'checked' : (value == def.default),
                'value': value}).click({ value: value, def : def}, click); 

        
        $option.append($toggle);
        
        last = id;
        
		
                       
    });
    
    
   
    return $option;
}

var create = function(parent, param) {
    var $param = AJS.$('<tr>').attr('id', param.name).attr('class','active');
    param.parent = param.name;
    
    $param.append(AJS.$('<td>').attr('id', param.name).text(param.name));
    $param.append(AJS.$('<td>').text(param.documentation));
    
    $param.append(factory[param.type](param));
    $param.append(AJS.$('<td>').text(param.default));
    $param.append(AJS.$('<td>').attr({
             	id:  param.set, 
				class: 'active',
             	name:"current"}).text(""));
      
    parent.append($param);
    
    
     
}
var show = function(val, def)
{   
    if ( def[val] == undefined)  return;
    
    last = def.parent;
    var parent = def.parent;
  
    AJS.$("#" + parent +"  > :[id='current']").text(val);
  
    AJS.$.each(def[val], function(i, param) {
        AJS.$("#" + param  +"  > td").attr('class', "active");   
        AJS.$("#" + param ).attr('class', "active");
        // Show also the children... 
      
        newdef = find(param)
        show(newdef.default, newdef)
   });
  
}
var hide = function(val, def)
{  
    if ( def[val] == undefined)  return;  
    var parent = def.parent;
    AJS.$("#" + parent +"  > :[id='current']").text(val);
   
   
    AJS.$.each(def[val], function(i, param) {		
        AJS.$("#" + param +"  > td" ).attr('class', "unactive");    
        AJS.$("#" + param).attr('class', "unactive");     
        // hide also the children...
        newdef = find(param)
		if ( newdef == undefined ) 
			console.info(param)
		else 
        	hide(newdef.default, newdef)
        
       
          
   });
   
}
var update = function(def, value)
{
   // If the line is inactive return...
	active = AJS.$("#"+ def.name).attr("class")
	
	setactive = ""
   AJS.$.each(def["values"], function(i, val) {
	   
     if ( val == value ) {
        if (active == "active") {
			setactive = val;
			AJS.$("#"+ def.name + "_" + val).attr('class', "radio_on");
		}
		else {
			hide(val, def);
			AJS.$("#"+ def.name + "_" + val).attr('class', "radio_on");
		}
    }
    else  {
        hide(val, def);
        AJS.$("#"+ def.name + "_" + val).attr('class', "radio");
    }
   });
   show(setactive, def);
   
}
var click = function(event)
{
   var value = event.handleObj.data.value
   var def = event.handleObj.data.def
   
   
  var id = def.parent;
  if (unactive(id) ) return;
  AJS.$("#" + id +"  > :[name='current']").text(value);
  show(value, def);  
  update(def, value);  
}
var colour = function(def) 
{
     var id = def.parent;
	var colour = def.default;
	colour = colour.toLowerCase();
    $option = AJS.$('<td>').attr({'id': id}).text("press to change colour");   
    var $input = AJS.$('<input>').attr({'id': id,
               'type': "text", 
			   'size' : '6',
               'class': "color-picker",
               'style' : "background-color:"+hexa[colour]+";",
			   'value': hexa[colour]});
	$input.miniColors({
		change: function(hex, rgb) {
            if (unactive(id) ) return;
			red = Math.round((rgb.r/256.)*100)/100;
			green = Math.round((rgb.g/256.)*100)/100;
			blue = Math.round((rgb.b/256.)*100)/100;
			colour= "rgb(" + red + "," + green + "," + blue + ")";
           
      		AJS.$("#" + id +"  > :[name='current']").attr({'style': "color:"+hex+";"}).text(colour);
      		$input.attr({'style': "background-color:"+hex+";"}).text(colour);
		}
	});
    var $help = AJS.$('<a>').attr({
		'href': 'http://software.ecmwf.int/wiki/display/MAGP/Setting+colours',
		'target': '_blank'}).text("More on colours...")
    $option.append($input);
    $option.append($help);
    return $option;
}
var style = function(def) 
{
    
    var id = def.parent;
    $option =  AJS.$('<td>').attr({'id': id}).text("solid/dash");   
    return $option;
}
var intlist = function(def) 
{
    var id = def.name;
    var parent = def.parent;
    $option =  AJS.$('<td>').attr({'id': id}).text("Enter a list of integer: 5/9/12");
    var $input = AJS.$('<input>').attr({'id': id,
               'type': "text", 
			   'value': ""}).
               keypress(function(event){        
                    event.stopPropagation()
               }).
               bind("propertychange keyup input paste", function(event){
                    if (unactive(parent) ) return;
      		        AJS.$("#" + parent +"  > :[name='current']").text($input.val());
               });
	$option.append($input);
    return $option;
}

var floatlist = function(def) 
{
    var id = def.name;
    var parent = def.parent;
    $option =  AJS.$('<td>').attr({'id': id}).text("Enter a list of float: 4.5/5./1.");
    var $input =  AJS.$('<textarea>').attr({'id': id,
               'type': "text", 
			   'value': ""}).
               keypress(function(event){        
                    event.stopPropagation()
               }).
               bind("propertychange keyup input paste", function(event){
                    if (unactive(parent) ) return;
      		        AJS.$("#" + parent +"  > :[name='current']").text($input.val());
               });
	$option.append($input);
    return $option;
}


var stringlist = function(def) 
{
    var id = def.name;
    var parent = def.parent;
    $option =  AJS.$('<td>').attr({'id': id}).text("Enter a list of strings: val1/val2/val3");
    var $input =  AJS.$('<textarea>').attr({'id': id,
               'type': "text", 
			   'value': ""}).
               keypress(function(event){        
                    event.stopPropagation()
               }).
               bind("propertychange keyup input paste", function(event){
                    if (AJS.$("#" + parent).attr("class") == "unactive") return;
                    AJS.$("#" + parent +"  > :[name='current']").text($input.val());
                });
	$option.append($input);
    return $option;
}

var any = function(def) 
{
    var id = def.name;
    var parent = def.parent;
    $option = AJS.$('<td>').attr({'id': id});
    var $input = AJS.$('<textarea>').attr({'id': id,
               'type': "text", 
			   'value': def.default}).
               keypress(function(event){        
                    event.stopPropagation()
               }).
               bind("propertychange keyup input paste", function(event){ 
                    if (AJS.$("#" + parent).attr("class") == "unactive") return;
                    AJS.$("#" + parent +"  > :[name='current']").text($input.val());
                });


	$option.append($input);
    return $option;
}
var number = function(def) 
{
    var id = def.name;
    var parent = def.parent;
    $option = AJS.$('<td>').attr({'id': id});
	if ( def.min != undefined && def.max != undefined ){
		var $input = AJS.$('<div>').slider({
		               min: def.min,
		               max: def.max,
					   value: def.default, 
					   slide: function( event, ui ) { 
                            if (unactive(parent) ) return;
      						AJS.$("#" + parent +"  > :[name='current']").text(ui.value);
						}});

		$option.append($input);
	}
	else {
    	var $input = AJS.$('<input>').attr({'id': id,
               	'type': "text", 
			   	'value': def.default}).
                keypress(function(event){        
                    event.stopPropagation()
                }).
                bind("propertychange keyup input paste", function(event){
                    if (unactive(parent) ) return;
	  	            if ( !isNaN(parseFloat($input.val())) && isFinite($input.val())) 
      		            AJS.$("#" + parent +"  > :[name='current']").text($input.val());
     	});
		$option.append($input);
	}

    return $option;
}
 var factory = {
        "toggle" : toggle,
        "Colour" : colour,
        "LineStyle" : style,
        "bool" : toggle,
        "number" : number,
        "string" : any,
        "stringlist" : stringlist,
        "intlist" : intlist,
        "numberlist" : floatlist,
        "floatlist" : floatlist
    };

var load  = function(file, where, python_only ) { 
	where = where || "magics"
	python_only = python_only || false
	
	 AJS.$.getJSON(file, function(data) { 
		 
        AJS.$.each(data.magics, function(i, action) {
		
        table = "table_" + action.action
          $action =  AJS.$('<table>').attr({'class': "parameters", "id": table});
		  genname = action.action
          $generate =  AJS.$('<div>').attr('id', genname);
          $code =  AJS.$('<div>').attr('id', "code");
           AJS.$("#" + where).append($action);
           AJS.$("#" + where).append($generate);
           AJS.$("#" + where).append($code);
          var $param =  AJS.$('<tr>').attr('id', "title");
          $param.append( AJS.$('<th>').attr({'width':150}).text(action.action));
          $param.append( AJS.$('<th>').attr({'width':350}).text("description"));
          $param.append( AJS.$('<th>').attr({'width':200}).text(""));
          $param.append( AJS.$('<th>').attr({'width':100}).text("default"));
          $param.append( AJS.$('<th>').attr({'width':100}).text("Your Selection"));

          $action.append($param);
          $param =  AJS.$('<tr>').attr('id', "title");
          $param.append( AJS.$('<td>').attr({'colspan':5}).text(action.documentation));
          $action.append($param);  
          AJS.$.each(action.parameters, function(i, param) {
			  
           create($action, param);
          }); 
		  parameters = action.parameters;
          AJS.$.each(action.parameters, function(i, param) {			  
			 update(param, param.default);
          }); 
         
         AJS.$("#"+genname).append(AJS.$('<input>').attr({
				type: "button", 
                value : "generate python"})
                .click({action: action.metview, where:where, table:table}, python));
		if (python_only == false) {
        AJS.$("#"+genname).append(AJS.$('<input>').attr({type: "button", 
                value : "generate Metview4 icon"})
                .click({action: action.metview, where:where, table:table}, metview));
        AJS.$("#"+genname).append(AJS.$('<input>').attr({type: "button", 
                value : "generate fortran"})
                .click({action: action.fortran, where:where, table:table}, fortran));
		 }
          return false; 
     	}); 
      return false;
    });
 };

var python = function(event) {
	
   var action = event.handleObj.data.action
   var where = event.handleObj.data.where
   var table = event.handleObj.data.table
    var code = "<b> python code </b><br/>" + action + "(";
    var sep = "";
	if  (pythons[action]  != undefined ) {
		code = "<b> python code </b><br/>" + pythons[action];
	    sep = ",<br/>&nbsp;&nbsp;"; 
	}
    AJS.$("#" + table + '>*>tr.active').each(function(i, entry) {
		

    	if ( entry.id == "") 
			return;
    	children = "#" + entry.id +"  > :[name='current']";
		AJS.$(entry).children().each(function(i, child) {
			if ( AJS.$(child).attr("name") == 'current' ) {
			    if (AJS.$(child).text() != "" ) {
				    code += sep + entry.id + " = " + pset(AJS.$(child)); 
				    sep = ",<br/>&nbsp;&nbsp;"; 
			    }
            }
		});
    });
	code += "<br/>)"
    AJS.$("#" + where + "> #code").html(code);
}


var metview = function(event) {
   var action = event.handleObj.data.action
    var code = "<b> Metview4 icon </b><br/>  " + action + "(";
   var where = event.handleObj.data.where
   var table = event.handleObj.data.table
    var sep = "";
    AJS.$("#" + table + '>*>tr.active').each(function(i, entry) {
    	if ( entry.id == "") 
			return;
    	children = "#" + entry.id +"  > :[name='current']";
		AJS.$(entry).children().each(function(i, child) {
			if ( AJS.$(child).attr("name") == 'current' ) {
			    if (AJS.$(child).text() != "" ) {
				    code += sep + entry.id + " : " + mset(AJS.$(child)); 
				    sep = ",<br/>&nbsp;&nbsp;"; 
			    }
            }
		});
    });
	code += "<br/>)"
    AJS.$("#" + where + "> #code").html(code);
}

var fortran = function(event) {
   var action = event.handleObj.data.action
   var where = event.handleObj.data.where
    
     var table = event.handleObj.data.table
    var code = "<b> Fortran skeleton </b>";
	sep = "<br/>&nbsp;&nbsp;"; 
    AJS.$("#" + table + '>*>tr.active').each(function(i, entry) {
    	if ( entry.id == "") 
			return;
    	
		AJS.$(entry).children().each(function(i, child) {
			if ( AJS.$(child).attr("name") == 'current' ) {
			    if (AJS.$(child).text() != "" ) {
				    code += sep + "call " + AJS.$(child).attr("id") + "(\""+ entry.id + "\", " + fset(AJS.$(child)) + ")"; 
				    sep = "<br/>&nbsp;&nbsp;"; 
			    }
            }
		});
    });
	
	if  (fortrans[action] == undefined  ) {
		code += sep + "call " + action + "()"
	}
	else  {
		code += sep + "call " + fortrans[action]
	}
    AJS.$("#" + where + " > #code").html(code);
}



var psetr = function(val)
{
    var out = "" + parseFloat(val).toFixed(2);
    return out;
}

var psetc = function(val)
{
    var out = "\"" + val + "\"";
    return out;
}


var quote = function(val)
{
	return out = "\"" + val + "\"";
}
var ppset1i = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "("
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + parseInt(values[i]);
		sep = ", ";
	}
	out = out + ")";
    return out;
}

var mpset1i = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "["
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + parseInt(values[i]);
		sep = ", ";
	}
	out = out + "]";
    return out;
}
var fpset1i = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "(/"
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + parseInt(values[i]);
		sep = ", ";
	}
	out = out + "/), " + nb;
    return out;
}


var ppset1r = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "("
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + parseFloat(values[i]).toFixed(2);
		sep = ", ";
	}
	out = out + ")";
    return out;
}

var mpset1r = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "["
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep +  parseFloat(values[i]).toFixed(2);
		sep = ", ";
	}
	out = out + "]";
    return out;
}
var fpset1r = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "(/"
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep +  parseFloat(values[i]).toFixed(2);
		sep = ", ";
	}
	out = out + "/), " + nb;
	return out
}
var ppset1c = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "("
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + quote(values[i]);
		sep = ", ";
	}
	out = out + ")";
    return out;
}

var mpset1c = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "["
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + quote(values[i]);
		sep = ", ";
	}
	out = out + "]";
    return out;
}
var fpset1c = function(val)
{
    values  = val.split("/");

	nb = values.length

	sep = "(/"
	out = ""

	for(i=0; i<nb; i++) {
		out = out + sep + quote(values[i]);
		sep = ", ";
	}
	out = out + "/), " + nb;
    return out;
}


var pseti = function(val)
{
    var out = "" + parseInt(val);
    return out;
}

 var fpset = {
 	"psetc" : psetc,
 	"pseti" : pseti,
 	"psetr" : psetr,
 	"pset1c" : fpset1c,
 	"pset1r" : fpset1r,
 	"pset1i" : fpset1i
 };
 var ppset = {
 	"psetc" : psetc,
 	"pseti" : pseti,
 	"psetr" : psetr,
 	"pset1c" : ppset1c,
 	"pset1r" : ppset1r,
 	"pset1i" : ppset1i
 };
 var mpset = {
 	"psetc" : psetc,
 	"pseti" : pseti,
 	"psetr" : psetr,
 	"pset1c" : mpset1c,
 	"pset1r" : mpset1r,
 	"pset1i" : mpset1i
	}

var pythons = {
        "mpostscript" : "output(output_formats=['ps']",
        "mpng" : "output(output_formats=['png']",
        "msvg" : "output(output_formats=['svg']",
        "mkml" : "output(output_formats=['kml']",
		}
 
var fortrans = {
        "ppostscript" : "pset1c('output_formats' , (/'ps'/), 1)",
        "ppng"        : "pset1c('output_formats' , (/'png'/), 1)",
        "psvg"        : "pset1c('output_formats' , (/'svg'/), 1)",
        "pkml       " : "pset1c('output_formats' , (/'kml'/), 1)"
}

var $action;

var fset = function(param) {
	return fpset[param.attr("id")](param.text());
}
var pset = function(param) {
	return ppset[param.attr("id")](param.text());
}
var mset = function(param) {
	return mpset[param.attr("id")](param.text());
}
