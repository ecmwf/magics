#!/usr/bin/perl


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
    "Colour" => 1,  
    "floatarray" =>1,
    "stringarray" => 1,
     "stringarray" => 1,
    "intarray" => 1, 
    "LineStyle" =>1,
    "Hemisphere" =>1,
    "ArrowPosition" => 1,
    "ListPolicy" => 1,
    "Justification" => 1,
    "Position" => 1,
    "AxisAutomaticSetting" => 1,
    "OpenGLDriverObserverPtr" => 1,
    "Widget" =>1,
    "QWidget*" =>1, 
    "cairo_t*" =>1, 
    "Matrix" => 1,
    "GribHandlePtr" =>1,
    "Path" =>1
);

my %arraytype = (
	"doublearray" => "atof(data)",
	"stringarray" => "data",
	"intarray" => "atoi(data)",
);

my %pointertype = {
	"Colour" => "1"
};

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





my $xml= new XML::Parser(Style=>"Tree");
  
parse ($info, $xml->parsefile($file));





foreach my $object (keys %{$info->{magics}->{class}}) 
{
   
    $current = $info->{magics}->{class}->{$object};
    $directory = $info->{magics}->{class}->{$object}->{attributes}->{directory};
    my $include_file = $info->{magics}->{class}->{$object}->{attributes}->{include};
    my $inherit = $info->{magics}->{class}->{$object}->{attributes}->{inherit};

######################################################
#####                Include file

  

    open STDOUT, ">$dir/$object\Wrapper.h";
    
    my $string =  ctime();
    
    $include_file = "$object.h" if  $include_file eq '';
    print  <<EOF;

/*! \\file $object\Wrapper.h
    \\brief Wrapper for Metview Request
    Automatically generated on $string
    Do Not Edit!
    
    Magics Team - ECMWF 2004
   
    Created: $string
    
*/
   

#ifndef $object\Wrapper_H
#define $object\Wrapper_H

#include "magics.h"
#include "ParameterManager.h"
#include "Factory.h"

#include "$include_file"

EOF
my $includes = {};
    my $member = lc $object;
    my $impl=$current->{attributes}->{inherits};
    my $abstract=$current->{attributes}->{abstract};
    
    print "#include \"$impl\Wrapper.h\"\n" if $impl ne ''; 
    my $parent ='';
    $parent=": public $impl\Wrapper" if $impl ne ''; 
    
    
    foreach my $param (@{$current->{parameter_list}}) 
    {   
       my $todo = $current->{parameter}->{$param}->{attributes}->{implemented};
       next if $todo eq 'no';
       my $mv = $current->{parameter}->{$param}->{attributes}->{metview};
       next if $mv eq 'no';
       my $to = $current->{parameter}->{$param}->{attributes}->{to};
     
       $includes->{$to} = "find" unless $basetype{$to} || $to eq 'Colour' ;
    }
    
    foreach my $include (keys %{$includes} ) 
    {
          print "#include \"$include\Wrapper.h\"\n"; 
    }
    
    print "\n";
  
    
    my $template = $current->{attributes}->{template};
    my $inherits_template = $current->{attributes}->{inherits_template};
    my $class = "$object\Wrapper";
    my $line = "";
    $object_t = "$object";
    if ( $template ne "" ) {
        $line = "template <class $template>";
        $class = "$object\Wrapper<$template>";
        $object_t = "$object<P>";
        $parent = "$parent<P>" unless  $parent eq "";
    }
    
    print <<EOF;



namespace magics {

class MagRequest;
$line
class $object\Wrapper $parent
{
public:
//  --  constructor
    $object\Wrapper();
    $object\Wrapper($object_t*);
//  --  destructor
    virtual ~$object\Wrapper();
    
    virtual void set(const MagRequest&);
EOF

if ($impl eq '') {
print <<EOF; 
    void object($object_t* $member) 
    	{ delete $member\_; $member\_ = $member; }
    $object_t* object()   { return $member\_; }
 
protected:
    $object_t* $member\_;

EOF
}

else {
print <<EOF; 
    $object_t* me() { return $member\_; }
  
protected:
    $object_t* $member\_;
EOF
}


print <<EOF;  

//  --  method
	virtual void print(ostream&) const;
	//virtual void toxml(ostream& out, int tabs = 0) const { toxml(out, tag_, tabs); }
	//virtual void toxml(ostream&, const string&, int tabs) const;
	


private:
    string tag_;
	friend ostream& operator<<(ostream& s,const $class& p)
	{ p.print(s); return s; }
};

} // namespace magics
EOF

    if ( $template ne "" ) {
        print "#include \"$object\Wrapper.cc\" \n";
    }
    
    print "\n#endif\n";
 
    close STDOUT;
    
#####                Include file
######################################################

######################################################
#####                Source file

    open STDOUT, ">$dir/$object\Wrapper.cc";
    
 
    print  <<EOF;
/*! \\file $object\Wrapper.h
    \\brief Implemtation of $object Wrapper class.
    Automatically generated on $string
    Do Not Edit!
    
    Magics Team - ECMWF 2004
   
    Created: $string
    
*/    

#include "MagRequest.h" 
#include "$object\Wrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;

EOF
   $header = "";
   $p = "";
   $ip = "";
   
   $ip = "<P>" if $inherits_template ne "";
   if ( $template ne "" ) {
        $header = "template <class $template>";
        $p = "<P>";
    }
    my $var = lc "$object";
    $member = lc "$object\_";
   
my $creator = '0';
$creator="new $object_t()" if $abstract ne 'yes'; 
if ($impl eq '') {
	
print <<EOF;
$header\n$object\Wrapper$p\::$object\Wrapper(): $member($creator)
{
}

$header\n$object\Wrapper$p\::$object\Wrapper($object_t* $var): $member($var)
{
}

EOF
}
else {
	
print <<EOF;
$header\n$object\Wrapper$p\::$object\Wrapper(): $impl\Wrapper$ip($creator)
{
	$member = static_cast<$object$p*>(this->object());
}

$header\n$object\Wrapper$p\::$object\Wrapper($object_t* $var): $impl\Wrapper$ip($var)
{
	$member = static_cast<$object$p*>(this->object());
}
EOF
}

print <<EOF;

$header
$object\Wrapper$p\::~$object\Wrapper()
{
}

$header
void $object\Wrapper$p\::set(const MagRequest& request)
{
EOF
	my $impl=$current->{attributes}->{inherits};

	print "\t$impl\Wrapper$p\::set(request);\n" unless $impl eq "";

	$val = 0;
	my $wrapper = lc $object;

    foreach  my $param (@{$current->{parameter_list}}) 
    {
        my $todo = $current->{parameter}->{$param}->{attributes}->{implemented};
        next if $todo eq 'no';
        my $mv = $current->{parameter}->{$param}->{attributes}->{metview};
        next if $mv eq 'no';
        my $name = $current->{parameter}->{$param}->{attributes}->{name};
	my $pre = $current->{attributes}->{metview_prefix};
	$name =~ s/$pre//;
        my $default = $current->{parameter}->{$param}->{attributes}->{default};
        my $mv_default = $default;
        $mv_default = $current->{parameter}->{$param}->{attributes}->{metview_default} if $current->{parameter}->{$param}->{attributes}->{metview_default} ne "";
        my $member = $current->{parameter}->{$param}->{attributes}->{member};
        my $from = $current->{parameter}->{$param}->{attributes}->{from};
        my $to = $current->{parameter}->{$param}->{attributes}->{to};
        my $tpl = $current->{parameter}->{$param}->{attributes}->{template};
        my $xml = $current->{parameter}->{$param}->{attributes}->{xml};
        my $wrapper_t = "$to\Wrapper";

        $wrapper_t = "$to\Wrapper<$tpl> " unless $tpl eq "";

        $attr_t = "<$tpl>" unless $tpl eq "";;
        $to = "$to<$tpl> " unless $tpl eq "";


        my $magto = $to;
        my $method = ucfirst $member;
        $magto = $magtype{$to} if $magtype{$to};
        $magto = "double" if $magto eq "float";
        $from = $magtype{$from} if $magtype{$from};
	
	
        $name = uc $name;
	
        my %simpletype = (
	        "Path" =>1,
	        "LineStyle" => 1,
    		"AxisAutomaticSetting" => 1,
            "ArrowPosition" =>1, 
	        "Justification" => 1,
		"Position" => 1,
            "ListPolicy" => 1,
            "stringarray" =>1,
            "bool" =>1,
            "Hemisphere"=>1,
	    );

	    my %arraytype = (
	    	"floatarray" => "(double)",
	    	"stringarray" => "(string)",
	    	"intarray" => "(int)",
    	);
	    my %pointertype = (
	    	"Colour" => "1",
	    	"Path" => "1",
		);
	    if ( $arraytype{$to} ) 
	    {
	    	print "\t\t$to $member;\n";
	    	print "\t\tfor (int i = 0; i < request.countValues(\"$name\"); i++)\n";	
	    	print "\t\t\t$member.push_back($arraytype{$to}request(\"$name\", i));\n";
	    	print "\t\tif ( !$member.empty() ) \n";
	    	print "\t\t\t$wrapper\_->$member\_ = $member;\n";
	    }
        elsif ( $basetype{$to} ) 
        {
            if ( $default eq $mv_default ) 
            {             
                print "\t\t if  (request.countValues(\"$name\") ) {\n";
            if ( $simpletype{$magto} ) 
            {
                print "\t\t\tstring $member\_s = request(\"$name\");\n";
                print "\t\t\t$wrapper\_->$member\_ = MagTranslator<string, $magto>()($member\_s);\n";
            }
            elsif ( $pointertype{$magto} ) 
            {
				
                print "\t\t\tstring $member\_s = request(\"$name\");\n";
                print "\t\t\t$wrapper\_->$member\_ = auto_ptr<$magto>(MagTranslator<string, $magto>()($member\_s));\n";
            }
            else
            {
        		if ( $quote{$magto} ) {
        			print "\t\t\t$magto $member(request(\"$name\"));\n";
        		}
        		else {
        			print "\t\t\t$magto $member = request(\"$name\");\n"; 
        		}
                print "\t\t\t$wrapper\_->$member\_ = $member;\n";
            }
            print "\t\t}\n\n";
            }
            else {
                 if ( $simpletype{$magto} ) 
            {
                 $arg = "(request.countValues(\"$name\") ? (string) request(\"$name\") : \"$mv_default\")";
                
                print "\t\tstring $member\_s = $arg;\n";
                print "\t\t$wrapper\_->$member\_ = (MagTranslator<string, $magto>()($member\_s));\n\n";
            }
                 elsif ( $pointertype{$magto} ) 
            {
                 $arg = "(request.countValues(\"$name\") ? (string) request(\"$name\") : \"$mv_default\")";
                
                print "\t\tstring $member\_s = $arg;\n";
                print "\t\t$wrapper\_->$member\_ = auto_ptr<$magto>(MagTranslator<string, $magto>()($member\_s));\n\n";
			}
            else
            {
        		if ( $quote{$magto} ) {
                 $arg = "(request.countValues(\"$name\") ? (string) request(\"$name\") : \"$mv_default\")";
        	  		print "\t\t$magto $member($arg);\n";
        		}
        		else {
                 $arg = "(request.countValues(\"$name\") ? request(\"$name\") : $mv_default)";
        			print "\t\t$magto $member = $arg;\n"; 
        		}
                print "\t\t$wrapper\_->$member\_ = $member;\n\n";
            }
            }
           
            
         }
         else {
         	    print "\t\tstring $member\_s = request.countValues(\"$name\") ?  (string) request(\"$name\") : \"$mv_default\";\n";
         	    print "\t\tMagLog::debug() << \" $name set to \" << $member\_s << endl;\n";
         	   	print "\t\t$wrapper_t* $member\_w = 0;\n";
                print "#ifdef MAGICS_EXCEPTION\n";
                print "\t\ttry\n";
                print "#endif\n";
                print "\t\t{\n";
                print "\t\t\t$member\_w = SimpleFactory<$wrapper_t>::create($member\_s);\n";
                print "\t\t}\n";
                print "#ifdef MAGICS_EXCEPTION\n";
                print "\t\t catch (NoFactoryException) {\n"; 
                print "#else\n";
                print "\t\t if (!$member\_w) {\n"; 
                print "#endif\n";
                print "\t\t\tMagLog::warning() << \"[\" << $member\_s << \"] is not a valid value for $name: reset to default -> [$mv_default]\" << endl;\n";
                print "\t\t\t$member\_w = SimpleFactory<$wrapper_t>::create(\"$mv_default\");\n";
			    print "\t\t}\n";
                print "\t\t$member\_w->set(request);\n";
                print "\t\t$wrapper\_->$member\_ =  auto_ptr<$magto>($member\_w->object());\n";
                print "\t\tdelete $member\_w;\n\n";
         }
    }
    
print <<EOF;
}

$header
void $object\Wrapper$p\::print(ostream& out)  const
{
	out << "$object\Wrapper[]";
}

EOF

    foreach  my $param (@{$current->{parameter_list}}) 
    {
        my $todo = $current->{parameter}->{$param}->{attributes}->{implemented};
        next if $todo eq 'no';
        foreach  my $option (@{$current->{parameter}->{$param}->{option_list}}) { 
        	my $class = $current->{parameter}->{$param}->{option}->{$option}->{attributes}->{name};
        	my $include = $current->{parameter}->{$param}->{option}->{$option}->{attributes}->{include};
        	my $fortran = $current->{parameter}->{$param}->{option}->{$option}->{attributes}->{fortran};
        	my $xml = $current->{parameter}->{$param}->{option}->{$option}->{attributes}->{xml};
        	my $base = $current->{parameter}->{$param}->{attributes}->{to};
            my @fortrans = split("/", $fortran);
        	my $template = $current->{parameter}->{$param}->{option}->{$option}->{attributes}->{template};
        	if ($template eq "") {
              if ( $class eq  $base) {
                for my $o (@fortrans) {
                    print "\nstatic SimpleObjectMaker<$base\Wrapper> $object\_$o\_$class\_w(\"$o\");\n" if $o ne "";
        	    }  
              }
              else {
                print "\n#include \"$class\Wrapper.h\"\n";  
                for my $o (@fortrans) { 	
        	        print "static SimpleObjectMaker<$class\Wrapper, $base\Wrapper> $object\_$o\_$class\_w(\"$o\");\n" if $o ne "";
                }
                if ($xml ne "") {
                    if ($xml ne $fortran) {
                        print "static SimpleObjectMaker<$class\Wrapper, $base\Wrapper> $object\_$xml\_$class\_w(\"$xml\");\n";
                    }
                }
        	  }
        	}
        	else {
        		my @templates = split("/", $template);	
        		print "\n#include \"$class\Wrapper.h\"\n";

        		for my $t (@templates) {
        		  print "\n#include \"$t.h\"\n";  
        		  if ( $class eq  $base) {
                	print "static SimpleObjectMaker<$base\Wrapper<$t> > $object\_$fortran\_$class\_$t\_w(\"$fortran\");\n" if $fortran ne "";
        	      }
                  else {
        	        print "static SimpleObjectMaker<$class\Wrapper<$t>, $base\Wrapper<$t> > $object\_$fortran\_$class\_$t\_w(\"$fortran\");\n" if $fortran ne "";
        	      }
        		}
        	}
        }
    }
}
