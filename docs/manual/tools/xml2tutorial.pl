#!/usr/bin/perl

use XML::Parser;
use Time::localtime;

my $file = shift;
my $dir = shift;
my $Author="MagicsTeam";
my $ecmwf="http://wms.ecmwf.int";
my $info = {};
my $element = {};

my %links = (
    'psetr'=>  [ 'Magics_using', 'Single_Parameter_Setting'],
    'pset1r'=> [ 'Magics_using', 'Array_Parameter_Setting'],
    'pset1c'=> [ 'Magics_using', 'Array_Parameter_Setting'],
    'pset1i'=> [ 'Magics_using', 'Array_Parameter_Setting'],
    'psetc'=>  [ 'Magics_using', 'Single_Parameter_Setting'],
    'pseti'=>  [ 'Magics_using', 'Single_Parameter_Setting'],
    'preset'=> [ 'Magics_using', 'table_single_param_reset_routines'],
    'pcoast'=> [ 'Magics_layout_mapping_coastlines', 'pcoast_tables'],
    'pcont'=>  [ 'Magics_contour', 'pcont_tables'],
    'ptext'=>  [ 'Magics', 'Text_Plotting_Parameters'],
    'pwind'=>  [ 'Magics', 'Wind_Plotting_Parameters'],
    'pgrib'=>  [ 'Magics_data', 'pgrib_tables'],
    'pnew("PAGE")'=> [ 'Magics_layout_mapping_coastlines', 'Pseudo_Action_Routine_PNEW'],
);

my %ulinks = (
    'pobs'=>  [ 'Magics', 'Observation_Plotting_Parameters_1'],
);

my %magmls = (
    
);

my $linkmagml;

sub link_function
{
    my $action = shift;
    if ( ${$links{$action}}[0] ne "") {
        return  "<function><olink targetdoc=\"${$links{$action}}[0]\" targetptr=\"${$links{$action}}[1]\">$action</olink></function>";
    }
    
    if ( ${$ulinks{$action}}[0] ne "") {
        return  "<function><ulink url=\"http://www.ecmwf.int/publications/manuals/magics/manuals/${$ulinks{$action}}[1].html\">$action</ulink></function>";
    }
        
    return "<function>$action</function>";
    
}

sub magml_function
{
#    my $magml = shift;
#    if ( ${$magmls{$magml}}[0] ne "") {
#        return  "<olink targetdoc=\"${$magmls{$magml}}[0]\" targetptr=\"${$magmls{$magml}}[1]\">$magml</olink>";
#    }
#    else {
        return "<olink targetdoc=\"Magics_using\" targetptr=\"MagML_Interface\" >MagML</olink>";
#    }
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





my $xml= new XML::Parser(Style=>"Tree");
  
parse ($info, $xml->parsefile($file));

foreach my $object (keys %{$info->{magics}->{tutorial}}) 
{
   my $current=$info->{magics}->{tutorial}->{$object};
   
   my $section=$current->{attributes}->{name};
   my $title=$current->{attributes}->{title};
   my $comment=$current->{attributes}->{comment};
   my $magml=$current->{attributes}->{magml};
   my $magplus=$current->{attributes}->{magplus};
   
   print << "EOF";   
    <section id='section_$section'>
        <title>$title </title>
        <para> $comment </para>
        <figure id="figure_$section">
           <title>The expected result</title>
	       <mediaobject id="output_$section">
	            <imageobject role="fo">
	              <imagedata fileref="../examples/tutorial/fortran/png/$section\_m69_CYLINDRICAL_01.png" format="PNG"
	                         align="center"/>
	            </imageobject>
	
	            <imageobject role="html">
	              <imagedata fileref="images/fortran/$section\_m69_CYLINDRICAL_01_thumb.png" format="PNG"
	                         align="center"/>
	            </imageobject>
	        </mediaobject>
       </figure>
         <section id='$section\_requirements'>
	        <title> Requirements </title>
	        <para> Here are our requirements </para>
	        <itemizedlist>     	
	     
EOF

 foreach my $req (sort keys %{$current->{requirement}} )     
   {      
        my $id=$current->{requirement}->{$req}->{attributes}->{name}; 
        my $subject=$current->{requirement}->{$req}->{attributes}->{subject};      
        my $text=$current->{requirement}->{$req}->{attributes}->{text};
        print << "EOF";
        <listitem><para> <olink targetdoc="Magics_tutorial" targetptr="$section\_clues_$id"> $subject </olink>: 
	            <emphasis>$text</emphasis></para></listitem>
EOF
   }
   print << "EOF";
   </itemizedlist>  
   </section>  
   <section  id='$section\_clues'>
     <title>Clues</title>
EOF

    foreach my $req (sort keys %{$current->{requirement}} )     
   {      
        my $id=$current->{requirement}->{$req}->{attributes}->{name}; 
        my $subject=$current->{requirement}->{$req}->{attributes}->{subject};      
        my $text=$current->{requirement}->{$req}->{attributes}->{text};
        my $targetdoc=$current->{requirement}->{$req}->{attributes}->{link_doc};
        my $targetptr=$current->{requirement}->{$req}->{attributes}->{link_ptr};
        my $ulink=$current->{requirement}->{$req}->{attributes}->{ulink};


        my $num_param = keys %{$current->{requirement}->{$req}->{parameter}}; 
        my $num_action = keys %{$current->{requirement}->{$req}->{action}};
       
        print <<"EOF";
           <section id='$section\_clues_$id'>
              <title>$subject</title>  
EOF
 if ( $num_param ne 0 ) {
                 print <<"EOF";          
                <table id="$section\_table_$id\_clues_params">
                  <title>Parameters for $subject</title>
                  <tgroup cols="3">
                  <colspec colwidth="200"/>
                  <colspec colwidth="80"/>
                  <colspec colwidth="100"/>
                  <thead>
                    <row>
                      <entry> Parameters </entry>  
                      <entry> Values </entry>  
                      <entry> Functions </entry>  
                    </row>
                  </thead>            
                  <tbody>
EOF
             foreach my $param (keys %{$current->{requirement}->{$req}->{parameter}} )   
              {
                    my $value = $current->{requirement}->{$req}->{parameter}->{$param}->{attributes}->{value};
                    my $function = $current->{requirement}->{$req}->{parameter}->{$param}->{attributes}->{function};
                    my $link = link_function($function);
                    
                    print <<"EOF";
                    <row>
                      <entry><parameter>$param</parameter></entry>
                      <entry>$value</entry>
                      <entry>$link</entry>
                    </row>
                    

EOF
                    
              }
print <<"EOF";
                  </tbody>
                </tgroup>
              </table>
EOF
}
           if   ( $num_action ne 0 ) {  
      
        print <<"EOF";
              <table id="$section\_table_$id\_clues_actions">
                <title>Action Routines for $subject</title>
                <tgroup cols="1">
                  <colspec colwidth="160"/>
                  <thead>
                    <row>
                      <entry> Action Routines </entry>  
                    </row>
                  </thead>
                  <tbody>
EOF

              foreach my $function (keys %{$current->{requirement}->{$req}->{action}} )   
              {
                    my $link = link_function($function);
                    print "          <row><entry>$link</entry></row>\n";
              }

        print <<"EOF";
                  </tbody>
                </tgroup>
              </table>
EOF
}

    if ( $targetdoc ne "" ) {
    
    print <<"EOF";
              <para> More information on <olink targetdoc="Magics_$targetdoc" targetptr="$targetptr" >$subject</olink> </para>
          
EOF
    }
    if ( $ulink ne "" ) {
    
    print <<"EOF";
              <para> More information on <ulink url=\"http://www.ecmwf.int/publications/manuals/magics/manuals/${ulink}.html\">$subject</ulink> </para>
           
EOF
    }
     print <<"EOF";
              </section>
           
EOF
    }
    print << "EOF";
        </section>
        <section  id='$section\_solution'>
          <title>Solution</title>                    
          <para> Here is the complete solution  
          <filename>$section\.f</filename></para>
          <programlisting>
           <xi:include href="../examples/tutorial/fortran/source/$section\.f_cc" parse="xml"
                       xmlns:xi="http://www.w3.org/2001/XInclude" />
            </programlisting>           
         </section>    
EOF

if ( $magplus ne 'off' ) {       
      print << "EOF";
          <section  id='$section\_diff'>
            <title> Spot the differences... </title>     
            <figure id="figure_output_mpp_$section">
              <title>Output for <productname>Magics++</productname></title>
              <mediaobject id="output_mpp_$section">

                <imageobject role="fo">
                  <imagedata fileref="../examples/tutorial/fortran/png/$section\_01.png" format="PNG"
                             align="center"/>
                </imageobject>

                <imageobject role="html">
                  <imagedata fileref="images/fortran/$section\_01_thumb.png" format="PNG"
                             align="center"/>
                </imageobject>
              </mediaobject>
            </figure>

            <figure id="figure_output_m69_$section">
              <title>Output for <productname>Magics 6.9</productname></title>
              <mediaobject id="output_m69_$section">
                <imageobject role="fo">
                  <imagedata fileref="../examples/tutorial/fortran/png/$section\_m69_CYLINDRICAL_01.png" format="PNG"
                             align="center"/>
                </imageobject>

                <imageobject role="html">
                  <imagedata fileref="images/fortran/$section\_m69_CYLINDRICAL_01_thumb.png" format="PNG"
                             align="center"/>
                </imageobject>
              </mediaobject>
          </figure>

         <important> 
            <title>Main differences</title>
            <itemizedlist>
EOF

                   
        foreach my $spot (keys %{$current->{spot}} )     
        {      
            my $text=$current->{spot}->{$spot}->{attributes}->{text}; 
            print << "EOF";
                <listitem><para>$text</para></listitem>
EOF

        }
        
        $linkmagml = magml_function($magml);
           print << "EOF";
            </itemizedlist>    
        </important>
        
      
     
       </section >

EOF

}

if ( $magml ne 'off' ) {
          print << "EOF";       
			<section  id='$section\_magml'>
				<title> Using MagML </title>
					<para> To get the same result, you can create the following <productname>magml</productname> description </para>
					<para><filename>$section\.magml</filename></para>
					<programlisting>
						<xi:include href="../examples/tutorial/magml/source/$section\.magml_cc" parse="xml"
                        xmlns:xi="http://www.w3.org/2001/XInclude" />
					</programlisting>

          <mediaobject id="output_magml_$section">
            <imageobject role="fo">
              <imagedata fileref="../examples/tutorial/magml/png/$section\.png" format="PNG"
                         align="center"/>
            </imageobject>

            <imageobject role="html">
              <imagedata fileref="images/magml/$section\_thumb.png" format="PNG"
                         align="center"/>
            </imageobject>
         </mediaobject>

         <para> More information on $linkmagml </para>
     
    </section >
  
  
        
EOF
}

print << "EOF";      
  </section > <!-- end $section -->  
EOF

}


