#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# use common::mpp_utils;

use XML::Parser;
use Data::Dumper;


my $Author="Sylvie Lamy-Thepaut, Iain Russell";
my $info = {};
my $element = {};

my @params_done;  # this will be a variable in which we maintain a list
                  # of all parameters dealt with so far...


# translate_default
# performs a translation on certain default parameters - for example, sometimes a
# default may be 'floatarray()' - in this case, we want to display something more meaningful.

sub translate_default
{
    my $string = $_[0];

    my %map = ();
    $map{"floatarray()"}  = "list of floats, e.g. /1,2,3.5/";
    $map{"stringarray()"} = "list of strings, e.g. /'a','b','c'/";
    $map{"intarray()"}    = "list of integers, e.g. /1,2,3/";

    my $translated = $map{$string};
    
    if ($translated eq "")
    {
        return $string;
    }
    else
    {
        return $translated;
    }
}



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




#sub generate_tables_for_class {
#
#	my ($default) = @_;
#
#
#    # start the table off, creating its title and header section
#
#    my $table_name  = "table_" . $current->{attributes}->{name} . "_parameters";
#    my $table_title = $current->{attributes}->{name} . " Parameters";
#
#
#
#    $default =
#"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
#  <table border=\"1\" id=\"$table_name\">
#    <title>$table_title</title>
#      <tgroup cols=\"4\">
#        <colspec colwidth=\"140\" />
#        <colspec colwidth=\"70\"  />
#        <colspec colwidth=\"150\" />
#        <colspec colwidth=\"80\"  />
#        <thead>
#          <row>
#            <entry align=\"center\"><literallayout>Parameter
#<emphasis>XML</emphasis></literallayout></entry>
#            <entry align=\"center\">Type</entry>
#            <entry align=\"center\">Function</entry>
#            <entry align=\"center\">Default</entry>
#          </row>
#        </thead>
#        <tbody>
#";
#
#
#
#
#    # for each parameter...
#
#    foreach my $key (@{$current->{parameter_list}}) 
#    {
#    
#        # For the current parameter, create its description. This will
#        # always start with the documentation string from the XML, but may
#        # also include a reference to the object that is created when 
#        # the parameter is activated.
#
#
#        # if this parameter has not been implemented, then do not document it    
#
#        next if $current->{parameter}->{$key}->{attributes}->{implemented} eq 'no';
#    
#
#        $default = $default .
#"
#          <row>
#            <entry><literallayout>$current->{parameter}->{$key}->{attributes}->{name}
#<emphasis>$current->{parameter}->{$key}->{attributes}->{xml}</emphasis></literallayout></entry>
#            <entry>$current->{parameter}->{$key}->{attributes}->{from}</entry>
#            <entry>$current->{parameter}->{$key}->{documentation}->{data}</entry>
#            <entry>$current->{parameter}->{$key}->{attributes}->{default}</entry>
#          </row>
#";   
#    
#     }
#
#
#    $default = $default .
#"
#        </tbody>
#      </tgroup>
#  </table>";
#                
#    return $default;
#
#}
#



#sub generate_list_of_tables_for_action_routine
#{
#
#    # start recursively from the root object
#    
##   print ("  root: $actionroutine->{attributes}->{root_object}\n");
#    
#    generate_list_of_tables_recursive ($actionroutine->{attributes}->{root_object});
#}


sub generate_tables_for_action_routine
{
    my ($dorecursive)   = $_[0];
    my ($actionroutine) = $_[1];
	my ($default) = @_;


    # First: a paragraph describing these parameters
    
    my ($action_desc) = ($actionroutine eq "") ? "" : "Action routine: '$actionroutine', ";
#    my ($intro_desc)  = "${action_desc}'$current->{attributes}->{name}'";
    my ($into_title)  = "Description"; # $intro_desc; # "Notes";
    my ($intro)      = (($current->{documentation}->{attributes}->{for_docs} eq "no") ||
                             ($current->{documentation}->{data} eq ""))
                        ?  "" # do not add a section describing this class
                        : "<note><title>$into_title</title>" . $current->{documentation}->{data}  . "</note>";


    # start the table off, creating its title and header section

    my $table_name  = "table_" . $current->{attributes}->{name} . "_parameters";
    my $table_title = $actionroutine . " Parameters";



    $default = $intro .
"  <table border=\"1\" id=\"$table_name\">
    <title>$table_title</title>
      <tgroup cols=\"4\">
        <colspec colwidth=\"140px\" />
        <colspec colwidth=\"70px\"  />
        <colspec colwidth=\"150px\" />
        <colspec colwidth=\"80px\"  />
        <thead>
          <row>
            <entry align=\"center\">Parameter</entry>
            <entry align=\"center\">Type</entry>
            <entry align=\"center\">Function</entry>
            <entry align=\"center\">Default</entry>
          </row>
        </thead>
        <tbody>
";


#    $default =
#"  <table border=\"1\" id=\"$table_name\">
#    <title>$table_title</title>
#      <tgroup cols=\"4\">
#        <colspec colwidth=\"140\" />
#        <colspec colwidth=\"70\"  />
#        <colspec colwidth=\"150\" />
#        <colspec colwidth=\"80\"  />
#        <thead>
#          <row>
#            <entry align=\"center\"><literallayout>Parameter
#<emphasis>MagML</emphasis></literallayout></entry>
#            <entry align=\"center\">Type</entry>
#            <entry align=\"center\">Function</entry>
#            <entry align=\"center\">Default</entry>
#          </row>
#        </thead>
#        <tbody>
#";



    # start recursively from the root object

    $default  = $default . object_parameters_recursive ($current, $current{attributes}->{name}, "", $dorecursive);


#    # write the table footer

    $default = $default .
"
        </tbody>
      </tgroup>
  </table>";

}





# --------------------------------------------
# object_parameters_recursive
# Recursively processes an obect's parameters
# --------------------------------------------

sub object_parameters_recursive {
    my ($root)        = $_[0];
    my ($object_name) = $_[1];
    my ($baseclass)   = $_[2];
    my ($dorecursive) = $_[3];
    my ($result) = "";


    if ($dorecursive eq "do-recursive")
    {

        # construct a list of all classes from which we inherit attributes
        # - starting with the base class

        my @base_classes;

        if ( ($baseclass ne $root->{attributes}->{name}) && ($baseclass ne ""))
        {
            @base_classes = ($baseclass);
        }

        if ($root->{attributes}->{inherits} ne "" || $root->{attributes}->{doc_inherits} ne "")
        {
            my @base_list  = split(/\//, $root->{attributes}->{inherits});
            @base_list = (@base_list, split(/\//, $root->{attributes}->{doc_inherits}));

            foreach my $base (@base_list)
            { 
                @base_classes = (@base_classes, $base);
            }
        }


        # for each of the base classes from which we inherit attributes, process them in
        # order to print their attributes

        foreach my $base (@base_classes)
        {
            my $base_result = object_parameters_recursive ($info->{magics}->{class}->{$base},
                                                           $object_name,
                                                           $info->{magics}->{class}->{$base}->{attributes}->{name},
                                                           $dorecursive);
            $result = $result . $base_result;
        }
    }




    foreach my $key (@{$root->{parameter_list}}) 
    {

        # only document those parameters that have been implemented

        my $implemented = $root->{parameter}->{$key}->{attributes}->{implemented};

        if ($implemented ne "no")
        {

            # only document a parameter if it has not yet been documented

#            my @param_match = grep (/contour_max_level\b/i, @params_done);
            my @param_match = grep (/$root->{parameter}->{$key}->{attributes}->{name}\b/i, @params_done);
            my $num_matches = @param_match;

            if ($num_matches eq 0 && $root->{parameter}->{$key}->{documentation}->{attributes}->{for_docs} ne "no")
            {
                # For the current parameter, create its description. This will
                # always start with the documentation string from the XML, but may
                # also include a reference to the object that is created when 
                # the parameter is activated.

                my $description = $root->{parameter}->{$key}->{documentation}->{data};



                # if this parameter has a number of fixed possible values, then list them with links
                # to the objects that are created as a result

                my $num_options_so_far = 0;

                foreach my $option (@{$root->{parameter}->{$key}->{option_list}})
                {

                    my $fortran_name    = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{fortran};
                    my $option_object   = $option;


                    # have a look at the object created by selecting this option.
                    # does it have any associated parameters?
                    # if so, create a link to the page associated with it

                    foreach my $mag_object (keys %{$info->{magics}->{class}})
                    {
                        if ($option_object  eq $mag_object)
                        {
                            $values = $values . "$fortran_name";            
                        }
                    }

                    $num_options_so_far = $num_options_so_far + 1;
                }


                my $xml_name    = $root->{parameter}->{$key}->{attributes}->{name};
                my $xml_from    = $root->{parameter}->{$key}->{attributes}->{from};
                my $xml_doc     = $root->{parameter}->{$key}->{documentation}->{data};
                my $xml_default = translate_default($root->{parameter}->{$key}->{attributes}->{default});
                
                $xml_name = lc $xml_name;   # ensure all parameter names are lower-case

                if ($xml_default eq "")
                {
                    $xml_default = "No default";
                }


                $result = $result .
                "
                          <row>
                            <entry>$xml_name</entry>
                            <entry>$xml_from</entry>
                            <entry>$xml_doc</entry>
                            <entry>$xml_default</entry>
                          </row>
                ";   


#                $result = $result .
#                "
#                          <row>
#                            <entry><literallayout>$root->{parameter}->{$key}->{attributes}->{name}
#                <emphasis>$root->{parameter}->{$key}->{attributes}->{xml}</emphasis></literallayout></entry>
#                            <entry>$root->{parameter}->{$key}->{attributes}->{from}</entry>
#                            <entry>$root->{parameter}->{$key}->{documentation}->{data}</entry>
#                            <entry>$root->{parameter}->{$key}->{attributes}->{default}</entry>
#                          </row>
#                ";   
#


                # dip recursively into the parameter's base class ... ?

                foreach my $option (@{$root->{parameter}->{$key}->{option_list}})
                {

                    my $fortran_name    = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{fortran};
                    my $base            = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{name};
                    my $docdive         = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{docdive};
                    my $option_object   = $option;

                    if ($docdive ne "no")  # sometimes we don't want to recurse
                    {
                        $result = $result . object_parameters_recursive ($info->{magics}->{class}->{$base},
                                                               $object_name,
                                                               $info->{magics}->{class}->{$base}->{attributes}->{name},
                                                               $dorecursive);
                    }
                }




                # Note: it might be nice to reintroduce the release version for various parameters once
                # Magics++ has been 'live' for a while. We should either strip the word 'Magics++' from
                # the release version info in the XML files or else do it here so we can check the
                # version as a number.


    #            if ($current->{parameter}->{$key}->{release_info}->{data} ne "")
    #            {
    #                $default = $default . "<br> $current->{parameter}->{$key}->{release_info}->{data}\n";
    #            }


                #          $default = $default . "\t\t\t<td bgcolor=\"#ffffff\">  $current->{parameter}->{$key}->{attributes}->{to} </td> \n";
                #          $default = $default . "\t\t\t<td bgcolor=\"#ffffff\">  $current->{parameter}->{$key}->{migration}->{data} </td> \n";

                @params_done = (@params_done, $root->{parameter}->{$key}->{attributes}->{name});
            }
        }
     }


    # we have an attribute called 'doc_include' which means that we just include the
    # parameters from the included object
    
#    if ($root->{attributes}->{doc_includes} ne "")
#    {
#        my @include_list  = split(/\//, $root->{attributes}->{doc_includes});
#
#        foreach my $include (@include_list)
#        { 
#            $result = $result . object_parameters_recursive ($info->{magics}->{class}->{$include},
#                                                   "Dummy",
#                                                   $info->{magics}->{class}->{$include}->{attributes}->{name},
#                                                   $dorecursive);
#        }
#    }



	return $result;
}






















sub expand_base_class
{
    local($base)  = @_[0];


    # go through the list of MAGICS objects and see which ones are derived from this one

    foreach my $mag_class (keys %{$info->{magics}->{static}})
    {
        if ($info->{magics}->{static}->{$mag_class}->{attributes}->{base} eq $base)
        {
#           print ("  child 1");
            generate_list_of_tables_recursive ($info->{magics}->{static}->{$mag_class}->{attributes}->{class});
        }
    }
}



sub generate_list_of_tables_recursive
{
    local($root)  = @_[0];

    $depth = $depth + 1;


    # first deal with this object

#   print ("    $depth sub: $root\n");
    local ($current) = $info->{magics}->{class}->{$root};
    
    if ($info->{magics}->{class}->{$root} ne "")
    {
        write_table_reference ($root);
    }


    # if it's a base class, then expand its descendants
    
    # expand_base_class ($root);


    # any children?

	if ($current->{attributes}->{embedded_objects} ne "")
	{
			my @ObjectList = split(/\//, $current->{attributes}->{embedded_objects});

    		foreach my $subobject (@ObjectList) 
    		{
#               print ("  child 2");
                generate_list_of_tables_recursive ($subobject);
   			}
	}


    $depth = $depth - 1;
            
    return $default;
}


sub write_table_reference
{
    local ($object_name) = @_[0];
    my $table_ref;

    my $table_id   = "table_" . $object_name . "_parameters";
    my $table_file = "$object_name.xml";

    $table_ref = 
"
    <xi:include href=\"$table_file\"
                xmlns:xi=\"http://www.w3.org/2001/XInclude\" />
";

    print HTML  $table_ref;
}




# -----------------------------------------------------------------------------
# Code entry point. This is where we actually start to generate the XML tables.
# We parse the magics.xml file and split it into its classes.
# -----------------------------------------------------------------------------

my $xml= new XML::Parser(Style=>"Tree");

system ('perl -s ../../tools/mergexml.pl -XMLDir=\'../../src/xml\'');

parse ($info, $xml->parsefile('../../src/xml/magics.xml'));



# generate a table for each action routine
# ----------------------------------------

foreach my $action (sort keys %{$info->{magics}->{actionroutine}}) 
{
    $actionroutine  = $info->{magics}->{actionroutine}->{$action};
    $actionname     = $actionroutine->{attributes}->{name};
    my $actiontitle = uc ($actionname);
    my $root_object = $actionroutine->{attributes}->{root_object};
    my $file_header = 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<section id=\"${actionname}_tables\">
  <title>$actiontitle Parameters</title>
";
    my $file_footer = "</section>";

    $current = $info->{magics}->{class}->{$root_object};
    @params_done = ();


    $/ = undef;
    my $text = generate_tables_for_action_routine("do-recursive", $actionname);

    open  HTML, ">paramtables/${actionname}_tables.xml";
    print HTML  $file_header;
    print HTML  $text;
    print HTML  $file_footer;
    close HTML;

}



# generate a table for each driver
# ----------------------------------------

foreach my $action (sort keys %{$info->{magics}->{driver}}) 
{
    my $driver         = $info->{magics}->{driver}->{$action};
    my $drivername     = $driver->{attributes}->{name};
    my $drivertitle    = $drivername;
    my $root_object    = $driver->{attributes}->{root_object};
    my $recursive_flag = $driver->{attributes}->{recursive_docs};
    my $file_header = 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<section id=\"${drivername}_tables\">
  <title>$drivertitle Parameters</title>
";
    my $file_footer = "</section>";

    $current = $info->{magics}->{class}->{$root_object};
    @params_done = ();


    $/ = undef;
    my $text = generate_tables_for_action_routine($recursive_flag, $drivername);

    open  HTML, ">paramtables/${drivername}_tables.xml";
    print HTML  $file_header;
    print HTML  $text;
    print HTML  $file_footer;
    close HTML;
}



#foreach my $magclass (keys %{$info->{magics}->{class}}) 
#{
#    $current = $info->{magics}->{class}->{$magclass};
#    
##   print ($current->{attributes}->{name}, "\n");
#
#    $/ = undef;
#    my $text = generate_tables_for_class("");
#
#    open  HTML, ">paramtables/$magclass.xml";
#    print HTML  $text;
#    close HTML;
#
#}
#

## generate a list of tables for each action routine
## -------------------------------------------------
#
#foreach my $action (sort keys %{$info->{magics}->{actionroutine}}) 
#{
#
#    $actionroutine  = $info->{magics}->{actionroutine}->{$action};
#    $actionname     = $actionroutine->{attributes}->{name};
#    my $root_object = $actionroutine->{attributes}->{root_object};
#    my $table_list_file = "paramtables/${actionname}_tables.xml";
#    my $file_header = 
#"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
#<section id=\"${actionname}_tables\">
#  <title>Parameters</title>
#";
#    my $file_footer = "</section>";
#
#
#    $depth = 0;
#    open  HTML, ">$table_list_file";
#    print HTML  $file_header;
#    generate_list_of_tables_for_action_routine ("");
#    print HTML  $file_footer;
#    close HTML;
#
#
#}


## We have a bit of a special case with the Data Input tables
## since the parameters are not necessarily associated with
## action routines. Process them separately here.
#
#my $table_list_file = "paramtables/data_input_tables.xml";
#my $file_header = 
#"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
#<section id=\"data_input_tables\">
#  <title>Parameters</title>
#";
#my $file_footer = "</section>";
#
#open  HTML, ">$table_list_file";
#print HTML  $file_header;
#
#foreach my $di_class ('GribDecoder', 'NetcdfDecoder', 'OdbDecoder', 'ArrayDecoder', 'ObsDecoder', 'SymbolInput', 'PpmDecoder')
#{
#    $depth = 0;
#    generate_list_of_tables_recursive ($di_class);
#}
#
#print HTML  $file_footer;
#close HTML;
#
