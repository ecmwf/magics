/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */



var build = function(def, base, python_only)
{   

  html= base + "/" + def.name +".html";
  python=base + "/" + def.name +".py";
  mv4=base + "/" + def.name + ".mv4";
  fortran=base + "/" + def.name + ".f90";
  img=base + "/" + def.name+".png";
  zip=base + "/" + def.name+".zip";  

  AJS.$("#magics").append(AJS.$("<span>").attr({class:"gallery_title"}).append(AJS.$("<b>").text(def.title)));
  AJS.$("#magics").append(AJS.$('<div>').attr({class:"gallery_right"}).
    append(AJS.$('<a>').attr({href : img}).append(AJS.$('<img>').attr({id : def.title,
                                       src: img,
                                       width : 277,
                                       height: 188 }))));
  $code = AJS.$("<code>");                            
   $code.load(html);
  $python =  AJS.$("<a>").attr({href:python}).html("python");
  $fortran = AJS.$("<a>").attr({href:fortran}).html("fortran"); 
  $mv4 =     AJS.$("<a>").attr({href:mv4}).html("Metview4 icon"); 
  $zip =     AJS.$("<a>").attr({href:zip}).html("all files"); 
  $pre = AJS.$('<pre>');
   $pre.append($code);
   $pre.append(AJS.$("<span>").html("<hr><b>Download:</b>&nbsp;"));
   $pre.append($python);
   $pre.append(AJS.$("<span>").html("&nbsp;"));
   if ( python_only==false){
   	$pre.append($fortran);
   	$pre.append(AJS.$("<span>").html("&nbsp;"));
   	$pre.append($mv4);
   	$pre.append(AJS.$("<span>").html("&nbsp;"));
   }
   
   $pre.append($zip);
  AJS.$("#magics").append($pre);
}


var load  = function(base, file, python_only) { 
	python_only = python_only || false
AJS.$.getJSON(base + "/" + file, function(data) {
    AJS.$.each(data.examples, function(i, example) {
        
        build(example, base, python_only)
        return true;
    });     
    return true;
  });
};


