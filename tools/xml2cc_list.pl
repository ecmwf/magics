#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


use XML::Parser;
use Time::localtime;

my $file = shift;
my $dir = shift;
my $Author="MagicsTeam";
my $ecmwf="http://wms.ecmwf.int";
my $info = {};
my $element = {};



my %basetype = (
    "int" => 1,
    "float" => 1,
    "string" => 1,
    "bool"=>1,
    "floatarray" =>1,
    "stringarray" => 1,
    "intarray" => 1, 
    "LineStyle" =>1,
    "ListPolicy" =>1,
    "Hemisphere" =>1,
    "ArrowPosition" => 1,
    "AxisAutomaticSetting" => 1,
    "Justification" => 1,
    "DisplayType" => 1,
    "OpenGLDriverObserverPtr" => 1,
    "Widget" =>1, 
    "cairo_t*" =>1,
    "QWidget*" =>1,
    "QGraphicsScene*" => 1,
    "Matrix" => 1,
    "GribHandlePtr" =>1,  
);

my %translator = (
    "int" => "ParameterManager::getInt",
    "float" => "ParameterManager::getDouble",
    "string" => "ParameterManager::getString",
    "stringarray" => "ParameterManager::getStringArray",
    "bool" => "ParameterManager::getBool",
    "doublearray" => "ParameterManager::getDoubleArray",
    "floatarray" => "ParameterManager::getDoubleArray",
    "intarray" => "ParameterManager::getIntArray"
    
);
my %magtype = (
	"float" =>'double',
);
my %consttype = (
	"string" =>'const string&',
);

my %classtype = (
	"Colour" =>1,
	"DateTime" =>1,
	"Path" => 1
);

my %arraytype = (
	"floatarray" => "atof(data)",
	"stringarray" => "data",
	"intarray" => "atoi(data)",
);

my %quote = (
    "string" => 1,
);

sub parse
{
    my $def = shift;
    my $node = shift;   
   
    while ( defined ( $element = shift @{ $node } ) )
    {
       
        my $child = shift @{ $node };
        if ( ref $child )
        {
           my $attr = \%{ shift @{ $child } };
           my $name = $attr->{name};
           
           if ($name ne "") 
           {
               
               $def->{$element}->{$name} = {};
               my $list = $element . "_list";
               push( @{$def->{$list}}, $name);
               foreach my $a (keys %{$attr}) 
               {
                   $def->{$element}->{$name}->{attributes}->{$a} = $attr->{$a}; 
                  
               }
               parse($def->{$element}->{$name}, $child); 
               
           }
           else
           {
                   $def->{$element} = {};
                   foreach my $a (keys %{$attr}) 
                   {
                        $def->{$element}->{attributes}->{$a} = $attr->{$a};
                       
                   }
                   parse($def->{$element}, $child); 
           }
        }
        else 
        {
         
          $def->{data} = $child;
        }
    }   
}





my $xml= new XML::Parser(Style=>"Tree");
  
parse ($info, $xml->parsefile($file));

foreach my $object (keys %{$info->{magics}->{class}}) 
{
    $current = $info->{magics}->{class}->{$object};
    $directory = $info->{magics}->{class}->{$object}->{attributes}->{directory};
    $tag = $info->{magics}->{class}->{$object}->{attributes}->{xmltag};
    print "$dir/$directory/$object\Attributes.h\n";
    print "$dir/$directory/$object\Attributes.cc\n";


    
}


