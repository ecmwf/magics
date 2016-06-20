#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



use XML::Parser;
use Data::Dumper;

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
	"bool" => 1,
    "string" => 1,  
    "floatarray" =>1,
    "stringarray" => 1,
     "magstringarray" => 1,
    "intarray" => 1, 
    "LineStyle" =>1,
    "Hemisphere" =>1,
    "ArrowPosition" => 1,
    "Justification" => 1,
    "OpenGLDriverObserverPtr" => 1,
    "Widget" =>1, 
    "Matrix" => 1,
    "GribHandlePtr" =>1,
    "Colour" =>1,
    "Path" =>1

);

my %arraytype = (
	"magfloatarray" => "atof(data)",
	"magstringarray" => "data",
	"magintarray" => "atoi(data)",
);

my %quote = (
    "string" => 1,    
);
my %type = (
    "float" => "double",    
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


my %metview = (
	"string" => "@",
	"float" => "*",
    "int" => "*",
    "bool"=> "on\n\t\toff"
);


my $xml= new XML::Parser(Style=>"Tree");
  
parse ($info, $xml->parsefile($file));





foreach my $object (keys %{$info->{magics}->{class}}) 
{
   
    $current = $info->{magics}->{class}->{$object};
    
    $class = $info->{magics}->{class}->{$object}->{attributes}->{name};
    print "$class; MagicsPlusPlus Object; $file\n{\n";
    
    foreach  my $param (@{$current->{parameter_list}}) 
    {     
        my $todo = $current->{parameter}->{$param}->{attributes}->{implemented};
        next if $todo eq 'no';
        
        my $param = $current->{parameter}->{$param}->{attributes}->{name};
        my $name  = uc $param;
        my $from = $current->{parameter}->{$param}->{attributes}->{from};
        my $to = $current->{parameter}->{$param}->{attributes}->{to};
     
        my $default = $current->{parameter}->{$param}->{attributes}->{default};
        $default = "\"\"" if $default eq "";
        $default = "\".\"" if $default eq ".";
        print "\t$name {\n";
            if ( $metview{$to} ) {
                print "\t\t$metview{$to}\n";
            } 
        print "\t} = $default\n";
        
    }
    print "}\n";  
      
   
}


