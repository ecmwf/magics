#!/usr/bin/env perl
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
my $Author="Iain Russell";
my $info = {};
my $element = {};
my $debug = 0;

my @params_done;  # this will be a variable in which we maintain a list
                  # of all parameters dealt with so far...


# a set of routines for formatting magml tags/elememts/attributes

# ---------------------------------------------------------------


sub format_tag
{
    my $tagname = $_[0];
    return "<classname>$tagname</classname>";
}

sub format_tag_default_option
{
    my $tagname = $_[0];
    return "<classname>$tagname</classname>";
}


sub format_attr_name
{
    my $name = $_[0];
    return "<parameter>$name</parameter>";
}

sub format_attr_value
{
    my $value = $_[0];
    
    return translate_default($value);
}

sub format_comment
{
    my $comment = $_[0];
    
    return "<comment>$comment</comment>";
}



sub start_tag_open
{
    my $tagname   = $_[0];
    my $isdefault = $_[1];
    my $formatted_tag = ($isdefault)  ? format_tag_default_option ($tagname) : format_tag ($tagname);
    
    return "&lt;$formatted_tag";
}

sub start_tag_close
{
    return "&gt;";
}


sub end_tag
{
    my $tagname = $_[0];
    my $isdefault = $_[1];
    my $formatted_tag = ($isdefault)  ? format_tag_default_option ($tagname) : format_tag ($tagname);
    
    return "&lt;/$formatted_tag&gt;\n";
}


sub start_tag_close_sudden  # eg <thing />
{
    return " /&gt;";
}


sub attribute
{
    my $name   = $_[0];
    my $value  = $_[1];
    my $indent = $_[2];
    my $add_newline_to_start = $_[3];
    
    my $newline = ($add_newline_to_start) ? "\n" : "";
    my $formatted_name  = format_attr_name  ($name);
    my $formatted_value = format_attr_value ($value);
        
    return "$newline$indent$formatted_name = $formatted_value";
}


sub option_or
{
    my $or = format_comment ("&lt;!--OR--&gt;");
    
    return "$or";
}


sub option_default
{
    my $or = format_comment ("&lt;!--Default:--&gt;");
    
    return "$or";
}


sub html_comment
{
    my $comment = $_[0];
    
    return format_comment ("&lt;!--$comment--&gt;");
}


sub hlink
{
    my $name = $_[0];
    my $book = $_[1];
    my $link = $_[2];
    
    if ($book eq "")
    {
        return ("$name");
    }
    else
    {
        return ("<olink type=\"noref\" targetdoc=\"Magics_$book\" targetptr=\"$link\">$name</olink>");
    }
}



# remove_prefix
# if $prefix is at the start of $string, then remove it and return the result.
# otherwise return $string

sub remove_prefix
{
    my $string = $_[0];
    my $prefix = $_[1];
    my $prefix_length = length($prefix);

    # yes, perhaps Perl has a fancy way of doing this, but let's make it
    # readable...
    
    # print ("  Removing prefix -$prefix- from -$string-\n");
    
    if ($prefix_length > 0 and (substr ($string, 0, $prefix_length) eq $prefix))
    {
        return substr ($string, $prefix_length + 1);  # also remove the '_' after the prefix (+1)
    }
    
    else
    {
        return $string;
    }    
}


#sub remove_prefix
#{
#    my $string       = $_[0];
#    my $prefixes     = @_[1];
#    my @prefix_list  = split(/\//, $prefixes);  # $prefix is a list of '/'-separated strings
#    my $num_prefixes = @prefix_list;
#
#    for ($i = $num_prefixes - 1; $i >= 0; $i--)  #  start with the longest prefix first
#    {
#        my $prefix        = @prefix_list [$i];
#        my $prefix_length = length ($prefix);
#
#        # yes, perhaps Perl has a fancy way of doing this, but let's make it
#        # readable...
#
#        print ("  Removing prefix -$prefix- ($i) from -$string-   [@prefix_list[0], @prefix_list[1], @prefix_list[2]]\n");
#
#        if ($prefix_length > 0 and (substr ($string, 0, $prefix_length) eq $prefix))
#        {
#            print ("YES\n");
#            return substr ($string, $prefix_length + 1);  # also remove the '_' after the prefix (+1)
#        }
#    }
#    
#    return $string;
#}


# translate_default
# performs a translation on certain default parameters - for example, sometimes a
# default may be 'floatarray()' - in this case, we want to display something more meaningful.

sub translate_default
{
    my $string = $_[0];

    my %map = ();
    $map{"floatarray()"}  = "''  " . html_comment("list of floats, e.g. '1/2/3.5'");
    $map{"stringarray()"} = "''  " . html_comment("list of strings, e.g. 'a/b/c'");
    $map{"intarray()"}    = "''  " . html_comment("list of integers, e.g. '1/2/3'");

    my $translated = $map{$string};
    
    if ($translated eq "")
    {
        return "'$string'";
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




sub xml_action_routine {
    my ($xmlaction)    = $_[0];
    my ($xmlprintname) = $_[1];
    my ($max_depth)    = $_[2];     #  how deep do we recurse into the structure?
    my ($filename_prefix) = $_[3];  #  anything extra to add to the filename?
    my ($outlinebak)   = $outline;
    my $flat = ($max_depth == 0);

    $outline = 0;

    $root_object = $xmlaction;
    $action_xml  = $root_object;
    $xmlname     = $xmlprintname;
    
    $current = $info->{magics}->{class}->{$root_object};
    

    if ($flat)
    {
        @params_done = ();
    }

    # object_parameters_recursive will be called from within generate_tables()
    
    $text = generate_tables ($max_depth, $filename_prefix);

    open  HTML, ">paramtables/${root_object}_${xmlname}_magml_${filename_prefix}tables.xml";
    print HTML  $text;
    close HTML;

    $outline = $outlinebak;
}



sub outline_recursive {

    my ($root)          = $_[0];
    my ($type)          = $_[1];
    my ($indent)        = $_[2];
    my ($max_depth)       = $_[3];  #  how deep do we recurse into the structure?
    my ($filename_prefix) = $_[4];  #  anything extra to add to the filename?
    my ($indent_incr)   = 4;  # how much do we increase the indent by for each subobject?
    my ($indent_string) = ' ' x $indent;
    my ($result)        = "";
    my ($comment_col)   = 28;
    my ($extra_indent);

    # have a quick look at the number of sub-elements and choices this element has

    my $num_subs = @{$root->{element_list}} + @{$root->{choice_list}};
        

    # for this element ...

    my ($element_name)   = $root->{attributes}->{name};
    my ($object_name)    = $root->{attributes}->{object};
    my ($comment_indent) = $comment_col - length ($element_name) - $indent;
    my ($comment_indent_string) = ' ' x $comment_indent;
    my ($book)           = $root->{attributes}->{chapter};
    my ($forcelink)      = $root->{attributes}->{linkid};
    my ($link)           = "";


    if ($type eq "element")
    {
        # print its start tag, and, if there are no sub-elements or choices, then
        # make it a short tag; otherwise we'll have to do it in 2 parts...
        # eg. it's either <tag> </tag>  or  <tag />.

        # also, the id that it links to will be calculated automatically unless linkid
        # is set - this overrides.

        $link = ($forcelink eq "") 
                ? "${object_name}_${element_name}_magml_tables"
                : $forcelink;


        # print ("linking to ${object_name}_${element_name}_magml_tables\n");

        my $tag_end      = ($num_subs != 0) ? start_tag_close () : start_tag_close_sudden ();
           $extra_indent = ($num_subs != 0) ? "  "               : "";
        my $object_tag = ($object_name eq "")
                         ? start_tag_open  ($element_name, 0)
                         : start_tag_open  (hlink($element_name, $book, $link), 0);

        $result = $result . $indent_string . $object_tag . $tag_end;
        
        xml_action_routine ($object_name, $element_name, $max_depth, $filename_prefix);
    }
    elsif ($type eq "choice")
    {
        $result = $result . $indent_string . html_comment ("choice");
    }


        
    # if it's '0+', then mention that it's optional
    
    if ($root->{attributes}->{occurrence} eq '0+')
    {
        $result = $result . $comment_indent_string . $extra_indent . html_comment ("(0 or more allowed)");
    }
        

    # if it's '1+', then mention that it can be repeated

    if ($root->{attributes}->{occurrence} eq '1+')
    {
        $result = $result . $comment_indent_string . $extra_indent . html_comment ("(1 or more allowed)");
    }
        
        
    # if it's '1', then mention that there can be only one

    if ($root->{attributes}->{occurrence} eq '1')
    {
        $result = $result . $comment_indent_string . $extra_indent . html_comment ("(exactly 1 allowed)");
    }


    # if it's '01', then mention that we can have 0 or 1

    if ($root->{attributes}->{occurrence} eq '01')
    {
        $result = $result . $comment_indent_string . $extra_indent . html_comment ("(0 or 1 allowed)");
    }


    # finish this element off with a newline

    $result = $result . "\n";



    # for each of its sub-elements
    
    foreach my $element (@{$root->{element_list}})
    {
               
        # process them      $info->{magics}->{magml}->{element}->{magics}
            
        $result = $result . outline_recursive ($root->{element}->{$element},
                                               "element",
                                               $indent + $indent_incr,
                                               $max_depth,
                                               $filename_prefix);
    }


    # for each choice ...

    foreach my $choice (@{$root->{choice_list}})
    {
        # if not the first choice, then print 'OR'

        # process the choice
            
        $result = $result . outline_recursive ($root->{choice}->{$choice},
                                               "choice",
                                               $indent + $indent_incr,
                                               $max_depth,
                                               $filename_prefix);
    }
 


    if ($num_subs != 0 && $type eq "element")
    {
        my $object_tag = ($object_name eq "")
                         ? end_tag  ($element_name, 0)
                         : end_tag  (hlink($element_name, $book, "${object_name}_${element_name}_magml_tables"), 0);

        $result = $result .  $indent_string . $object_tag;
    }


    return $result;
}




# --------------------------------------------
# object_parameters_recursive
# Recursively processes an obect's parameters
# --------------------------------------------

sub object_parameters_recursive {

    my ($root)        = $_[0];
    my ($object_name) = $_[1];
    my ($print_name)  = $_[2];
    my ($baseclass)   = $_[3];
    my ($indent)      = $_[4];
    my ($attr_indent) = $_[5];
    my ($isdefault)   = $_[6];
    my ($dotags)      = $_[7];
    my ($done_first_attribute) = $_[8];
    my ($depth)       = $_[9];   # 0 if first call, 1 if called recursively once, 2 if 2nd recursive call, ...
    my ($max_depth)   = $_[10];  # how many levels down we will recurse to in our object description
    my ($indent_incr) = 4;  # how much do we increase the indent by for each subobject?
    my ($result) = "";
    my $tagname = ($print_name eq "") ? $object_name : $print_name;
    my $xmltag = $root->{attributes}->{xmltag};  # we only check this for consistency, we don't use it.
    my ($indent_string) = ' ' x $indent;
    my ($attributes_indent_string) = ' ' x ($indent + length($tagname) + 2);
    my ($indent_to_use);
    my ($has_options) = 0;
    my $num_parameters = @{$root->{parameter_list}};
    my $num_subobjects = 0;
    my $attributes_only = ($depth >= $max_depth) ? 1 : 0;  # if deep enough, then only produce attributes, not new tags
    my $dosubtags;
    my $subindent_incr;
    my $attr_indent_to_pass = $attr_indent;  # attribute indent to pass to the next level of recursion
    my $prefix = $root->{attributes}->{prefix};  # "map/map_grid"
    my @prefix_list = split(/\//, $prefix);  # $prefix is a list of '/'-separated strings
    my $prefix_to_remove = ($max_depth == 0) ? "" : @prefix_list[$depth]; # print ("DEPTH: $depth");
    my $flat = ($max_depth == 0);
    

    # print ("Object: $object_name, Root: $root->{attributes}->{name}, depth: $depth, prefix: $prefix, prefix_to_remove: $prefix_to_remove\n");

    if ($debug) { print ($indent_string . "object_name: $object_name, root: " . $root->{attributes}->{name} .", baseclass: $baseclass, attributes_only: $attributes_only, depth: $depth\n"); }

    if ($xmltag ne "" && $xmltag ne $object_name)
    {
        # print ("Inconsistency: option name: $object_name, xmltag: $root->{attributes}->{xmltag} (object $root->{attributes}->{name})\n");
    }

    if (!$flat)
    {
        @params_done = ();
    }


    # does this object actually exist?
    
    if (length($object_name) == 0)
    {
        # NOTE: this print could actually be quite useful, so don't forget about it!
        # print ("No object for $root->{attributes}->{name}\n");
        return "";
    }
    
    # print ("$indent_string Object $root->{attributes}->{name}\n");
    
    if ($dotags)
    {
        $result = $result . $indent_string . start_tag_open  ($tagname, $isdefault);
    }



    # construct a list of all classes from which we inherit attributes
    # - starting with the base class
    
    my @base_classes;

    if ($root->{attributes}->{doc_inherits} ne "no" && $baseclass ne $root->{attributes}->{name})
    {
        @base_classes = ($baseclass);
    }


    # add the other base classes if they are mentioned
    
    if ($root->{attributes}->{doc_inherits} ne "no" &&
        ($root->{attributes}->{inherits} ne "" || $root->{attributes}->{doc_inherits} ne "" || $root->{attributes}->{xml_doc_inherits} ne ""))
    {
        my @base_list  = split(/\//, $root->{attributes}->{inherits});
        @base_list = (@base_list, split(/\//, $root->{attributes}->{doc_inherits}));
        @base_list = (@base_list, split(/\//, $root->{attributes}->{xml_doc_inherits}));

        foreach my $base (@base_list)
        { 
            #print ("+$base,");
            @base_classes = (@base_classes, $base);
        }
    }

    if ($root->{attributes}->{base} ne "")
    {
        my @base_list = split(/\//, $root->{attributes}->{base});

        foreach my $base (@base_list)
        { 
            #print ("-$base,");
            @base_classes = (@base_classes, $base);
        }
    }

    # remove duplicate elements
    undef %saw;
    @base_classes = grep(!$saw{$_}++, @base_classes);



#print ("\n");
    # for each of the base classes from which we inherit attributes, process them in
    # order to print their attributes

    foreach my $base (@base_classes)
    { 
#        print ("1: ");
        #print ("1\n");
        my $base_result = object_parameters_recursive ($info->{magics}->{class}->{$base},  # root
                                                       $object_name,                       # object_name
                                                       $tagname,                           # print_name
                                                       $info->{magics}->{class}->{$base}->{attributes}->{name}, # base_class
                                                       $indent,                            # indent
                                                       $attr_indent_to_pass,          # attr_indent
                                                       $isdef,                             # isdefault
                                                       0,                                  # dotags
                                                       $done_first_attribute,              # done_first_attribute
                                                       $depth,                             # depth
                                                       $max_depth);                        # max_depth
        $result = $result . $base_result;


        # ensure that if the base class gave us any attributes, then we remember that
        # we've now actually done the first one for this class

        if ($base_result ne "")
        {
            $done_first_attribute = 1;
        }
    }




    # if we're just generating attributes, not embedded tags, then use the attribute
    # indentation that has been passed through from the previous level

    if ($attr_indent ne "")
    {
        $attributes_indent_string = $attr_indent;
    }
    
    if ($attributes_only)
    {
        $attr_indent_to_pass = $attributes_indent_string;
    }



    # For each parameter, check to see whether it has 'option' tags.
    # If it does not, then it's just an attribute and we add it.

    foreach my $key (@{$root->{parameter_list}}) 
    {
        my $num_options = @{$root->{parameter}->{$key}->{option_list}};
        
        if ($num_options == 0 || $root->{parameter}->{$key}->{attributes}->{ignore_indirection} eq "yes")  # no options, so it's an attribute
        {
            my $attr_name = $root->{parameter}->{$key}->{attributes}->{xml};
            
            if ($flat || $attr_name eq "")  # if flat view or no 'xml' tag, then use 'name'
            {
                $attr_name = $root->{parameter}->{$key}->{attributes}->{name};
            }

            if ($attr_name eq "")  # some attributes have no xml member - we cannot document these (they cannot be used)
            {
                print ("Parameter '$root->{parameter}->{$key}->{attributes}->{name}' of object '$object_name' does not have an xml member.\n");
                next;
            }

            # some parameters are not to be documented...
            if ($root->{parameter}->{$key}->{documentation}->{attributes}->{for_docs} eq "no")
            {
                print ("Parameter '$root->{parameter}->{$key}->{attributes}->{name}' of object '$object_name' is hidden from docs.\n");
                next;
            }

            my $implemented  = $root->{parameter}->{$key}->{attributes}->{implemented};
            
            if ($implemented ne "no")
            {
                # only document a parameter if it has not yet been documented

                my @param_match = grep (/$root->{parameter}->{$key}->{attributes}->{name}\b/i, @params_done);
                my $num_matches = @param_match;

                if ($num_matches eq 0)
                {
                    $indent_to_use = ($done_first_attribute)      # have we done the first attribute?
                                     ? $attributes_indent_string  # yes - use the full indent
                                     : ' ';                       # no  - just a single space

                    my $attr_default = $root->{parameter}->{$key}->{attributes}->{default};

                    $result = $result . attribute (remove_prefix ($attr_name, $prefix_to_remove), $attr_default, $indent_to_use, $done_first_attribute);
                    # print ("$indent_to_use, $attr_name, $attr_default, $done_first_attribute\n");

                    $done_first_attribute = 1;
                    #print ("----attr: $key\n");

                    @params_done = (@params_done, $root->{parameter}->{$key}->{attributes}->{name});
                }
            }
        }
        
        else
        {
            $num_subobjects = $num_subobjects + 1;
        }
    }

    
    # we have an attribute called 'doc_include' which means that we just include the
    # flat parameters from the included object
    
#    if ($root->{attributes}->{doc_includes} ne "")
#    {
#        my @include_list  = split(/\//, $root->{attributes}->{doc_includes});
#
#        foreach my $include (@include_list)
#        { 
#            $result = $result . object_parameters_recursive ($info->{magics}->{class}->{$include},   # root
#                                                             "Dummy",                                # object_name
#                                                             "",                                     # print_name
#                                                             $base,                                  # base_class
#                                                             $indent,                                # indent
#                                                             $attributes_indent_string,              # attr_indent
#                                                             $isdef,                                 # isdefault
#                                                             $dosubtags,                             # dotags
#                                                             1,                                      # done_first_attribute
#                                                             $depth,                                 # depth
#                                                             $max_depth);                            # max_depth
#        }
#    }
#







    if ($dotags && ($num_subobjects != 0) && !$attributes_only)
    {
        $result = $result . start_tag_close () . "\n";
    }



    
    if (($num_subobjects == 0) && $dotags)
    {
        $result = $result . start_tag_close_sudden () . "\n";
        return $result;
    }




    # For each parameter, check to see whether it has 'option' tags.
    # If it has, then recursively call this function with each option's object.

    if ($attributes_only)
    {
        $dosubtags      = 0;
        $subindent_incr = 0;
    }
    else
    {
        $dosubtags      = 1;
        $subindent_incr = $indent_incr;
    }


    foreach my $key (@{$root->{parameter_list}}) 
    {
        my $num_options = @{$root->{parameter}->{$key}->{option_list}};

        if ($num_options != 0 && $root->{parameter}->{$key}->{attributes}->{ignore_indirection} ne "yes")   # do we have any options to process?
        {
            if ($attributes_only)
            {
                my $attr_name = $root->{parameter}->{$key}->{attributes}->{xml};

                if ($flat || $attr_name eq "")  # if flat view or no 'xml' tag, then use 'name'
                {
                    $attr_name = $root->{parameter}->{$key}->{attributes}->{name};
                }

                if ($attr_name eq "")  # some attributes have no xml member - we cannot document these (they cannot be used)
                {
                    print ("Parameter '$root->{parameter}->{$key}->{attributes}->{name}' of object '$object_name' does not have an xml member.\n");
                    next;
                }


                my $implemented  = $root->{parameter}->{$key}->{attributes}->{implemented};

                if ($implemented ne "no")
                {
                    $indent_to_use = ($done_first_attribute)      # have we done the first attribute?
                                     ? $attributes_indent_string  # yes - use the full indent
                                     : ' ';                       # no  - just a single space

                    my $attr_default = $root->{parameter}->{$key}->{attributes}->{default};

                    $result = $result . attribute (remove_prefix ($attr_name, $prefix_to_remove), $attr_default, $indent_to_use, $done_first_attribute);

                    $done_first_attribute = 1;
                }


                my $base       = $root->{parameter}->{$key}->{attributes}->{to};
                my $done_first_option = 0;
                my $next_indent = ' ' x ($indent + $indent_incr + 2);
                $has_options = 1;

                foreach my $option (@{$root->{parameter}->{$key}->{option_list}}) 
                {
                    my $option_xml     = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{xml};
                    my $option_fortran = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{fortran};
                    my $isdef          = ($option_fortran eq $defaultval)
                                         ? 1  # yes, this is the default option
                                         : 0; # no, this is not the default option
                    my $base_to_inherit = ($root->{parameter}->{$key}->{option}->{$option}->{attributes}->{doc_inherits} eq "no")
                                          ? ""     # we do not want to inherit the base class
                                          : $base; # we do want to inherit the base class by default

                    #print ("2\n");
                    $result = $result . object_parameters_recursive ($info->{magics}->{class}->{$option},    # root
                                                                     $option_xml,                            # object_name
                                                                     "",                                     # print_name
                                                                     $base_to_inherit,                       # base_class
                                                                     $indent + $subindent_incr,              # indent
                                                                     $attr_indent_to_pass,                   # attr_indent
                                                                     $isdef,                                 # isdefault
                                                                     $dosubtags,                             # dotags
                                                                     $attributes_only,                       # done_first_attribute
                                                                     $depth,                                 # depth
                                                                     $max_depth);                            # max_depth
                    $done_first_option = 1;
                }
            }
            
            else  # ie, if (not($attributes_only))
            {
                my $defaultval = $root->{parameter}->{$key}->{attributes}->{default};
                my $base       = $root->{parameter}->{$key}->{attributes}->{to};
                my $done_first_option = 0;
                my $next_indent = ' ' x ($indent + $indent_incr + 2);
                $has_options = 1;

                if (!$attributes_only) {$result = $result . "\n";}

                foreach my $option (@{$root->{parameter}->{$key}->{option_list}}) 
                {
                    my $option_xml     = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{xml};
                    my $option_fortran = $root->{parameter}->{$key}->{option}->{$option}->{attributes}->{fortran};
                    my $isdef          = ($option_fortran eq $defaultval)
                                         ? 1  # yes, this is the default option
                                         : 0; # no, this is not the default option
                    my $base_to_inherit = ($root->{parameter}->{$key}->{option}->{$option}->{attributes}->{doc_inherits} eq "no")
                                          ? ""     # we do not want to inherit the base class
                                          : $base; # we do want to inherit the base class by default


                    if ($done_first_option)
                    {
                        $result = $result . $next_indent . option_or() . "\n";
                    }


                    if ($isdef)
                    {
                        $result = $result . $next_indent . option_default() . "\n";
                    }

                    #print ("3\n");
                    $result = $result . object_parameters_recursive ($info->{magics}->{class}->{$option},    # root
                                                                     $option_xml,                            # object_name
                                                                     "",                                     # print_name
                                                                     $base_to_inherit,                       # base_class
                                                                     $indent + $subindent_incr,              # indent
                                                                     $attr_indent_to_pass,                   # attr_indent
                                                                     $isdef,                                 # isdefault
                                                                     $dosubtags,                             # dotags
                                                                     $attributes_only,                       # done_first_attribute
                                                                     $depth + 1,                             # depth
                                                                     $max_depth);                            # max_depth
                    $done_first_option = 1;
                }
            }
        }
    }
    



    if ($dotags)
    {
        if ($attributes_only)
        {
            $result = $result . start_tag_close_sudden () . "\n";
        }
        
        else
        {
            my $newline_before_endtag = ($has_options)
                                        ? "\n"
                                        : "";
            $result = $result . $newline_before_endtag . $indent_string . end_tag ($tagname, $isdefault);
        }
    }

    return $result;
}




sub generate_tables {
	my ($default);
    my ($max_depth)       = $_[0];  #  how deep do we recurse into the structure?
    my ($filename_prefix) = $_[1];  #  anything extra to add to the filename?
    my ($object_name) = $current->{attributes}->{name};
    my ($title) = ($max_depth == 0)
                  ? "MagML Flat Outline Template for $xmlname"
                  : "MagML Outline Template for $xmlname";


    my $file_header = 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<section id=\"${object_name}_${xmlname}_magml_${filename_prefix}tables\">
  <title>$title</title>
";

    my $file_footer = "
  </programlisting>
</section>";


    # are we producing the MagML outline document?

    if ($outline)
    {
        $default = $file_header . "<programlisting>" . outline_recursive ($current, 1, -4, $max_depth, $filename_prefix);
    }


    # or one document per top-level tag?

    else
    {
        my ($into_title);
        my ($intro);
        # First: a paragraph describing these parameters
    
        if ($max_depth == 0)
        {
            $intro = "";
        }
        else
        {
            ($into_title) = "Description"; # "Element '$xmlname'";
            ($intro)      = (($current->{documentation}->{attributes}->{for_docs} eq "no") ||
                             ($current->{documentation}->{data} eq ""))
                                ?  "" # do not add a section describing this class
                                : "<note><title>$into_title</title>" . $current->{documentation}->{data}  . "</note>";

        }


        $default = $file_header . $intro . "<programlisting>\n" .
                   object_parameters_recursive ($current,                    # root
                                                $action_xml,                 # object_name
                                                $xmlname,                    # print_name
                                                "",                          # base_class
                                                0,                           # indent
                                                "",                          # attr_indent
                                                1,                           # isdefault
                                                1,                           # dotags
                                                0,                           # done_first_attribute
                                                0,                           # depth
                                                $max_depth);                 # max_depth
    }


    # finish off the table

	$default = $default . $file_footer;

                
	return $default;
}



# -----------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------
#
#                                      Main Code
#
# -----------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------



my $xml= new XML::Parser(Style=>"Tree");

system ('perl -s ../../tools/mergexml.pl -XMLDir=\'../../src/xml\'');

parse ($info, $xml->parsefile('../../src/xml/magics.xml'));

$outline = 1;


# construct the main page that gives the general outline of a 'MagML program'

# $current = $info->{magics}->{magml}->{element}->{magics};
# $current = $info->{magics}->{justcontour};

print ("Generating deep MagML tables...\n");
$current = $info->{magics}->{magml2};
$text = generate_tables (10, "");

print ("Generating flat MagML tables...\n");
$current = $info->{magics}->{magml2};
$text = generate_tables ( 0, "flat_");

open  HTML, ">paramtables/magml_outline.xml";
print HTML  $text;
close HTML;



