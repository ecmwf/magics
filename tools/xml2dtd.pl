#!/usr/bin/env perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



use XML::Parser;
use Data::Dumper;
use Date::Calc;

my $file = shift;
my $Author="MagicsTeam";
my $ecmwf="http://wms.ecmwf.int";
my $info = {};
my $element = {};


my %basetype = (
	"int" => 1,
	"magint" => 1,
	"double" => 1,
	"float" => 1,
	"magfloat" => 1,
	"long" => 1,
	"bool" => 1,
    "string" => 1,
    "magstring" => 1,
    "FloatArray" =>1,
    "magfloatarray" =>1,
    "StringArray" => 1,
    "IntArray" => 1,
    "LineStyle" =>1,
     "Hemisphere" =>1,
    "Colour" =>1,
);

my %magtype = (
	"int" => "magint",
	"long" => "magint",
	"float" => "magfloat",
	"double" => "magfloat",
    "string" => "magstring",
    "FloatArray" => "magfloatarray",
    "StringArray" => "magstringarray",
    "IntArray" => "magintarray",
);

my %quote = (

    "string" => 1,
    "magstring" => 1,
    
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

sub process
{
    my $current = shift;
    
    print " Found Class $current->{attributes}->{name}\n";
    
    foreach my $param (@{$current->{parameter_list}}) 
    {   
        my $to = $current->{parameter}->{$param}->{attributes}->{to};
        print "    found param --->$param (To---> $to)\n";
        print " $current->{parameter}->{$param}->{available}->{data}\n";
        if  ( !$basetype{$to} )  {
            print "    find subclasses! \n";
            foreach my $available (@{$current->{parameter}->{$param}->{available_list}}) {
                    print "      found available --->$available\n";
                    process($info->{magics}->{class}->{$available});
            }
           
        }
    }
    
    my $objects = $current->{attributes}->{embedded_objects};
    foreach my $subobject (split('/', $objects))
    {   
        print "    found subobject --->$subobject\n";
        process($info->{magics}->{class}->{$subobject});
        
    
    }
    
}

sub element
{
    my $current = shift;
    
    
    print " <!ELEMENT  $current->{attributes}->{name}";
    my $sep = "(";
    my $last = "";
    
    
    {   
        my $occurence = $current->{child}->{$child}->{attributes}->{occurence};
        print "$sep$child$occurence";
	$sep = ", ";
	$last = ")";
        
    }
    print "$last>\n";
    
}

my $xml= new XML::Parser(Style=>"Tree");

parse ($info, $xml->parsefile($file));
print "<!DOCTYPE magics [\n";
#foreach my $object (keys %{$info->{magics}->{xml_element}}) 
foreach my $object (@{$info->{magics}->{xml_element_list}}) 
{
  
   element($info->{magics}->{xml_element}->{$object});
 
}
print "]\n"; 

foreach my $object (keys %{$info->{magics}->{class}}) 
{
   
    process($info->{magics}->{class}->{$object});
}
        
   



