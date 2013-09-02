#!/usr/bin/perl
use strict;
use Date::Calc;

my $class = @ARGV[0];
shift @ARGV;


my @parents = @ARGV;

(my $year,my $month,my $day) = Date::Calc::Today();
my $string = Date::Calc::Date_to_Text($year, $month, $day);

open STDOUT, ">src/$class.h";
print <<"EOF";
/*! \\file $class.h
    \\brief Definition of the Template class $class.
    
    Magics Team - ECMWF $year
    
    Started: $string
    
    Changes:
    
*/

#ifndef $class\_H
#define $class\_H

#include "magics.h"
#include "Translator.h"
#include "Factory.h"


EOF

foreach my $parent (@parents) {
	print "#include \"$parent\.h\"\n";
}
print "\n";
print "namespace magics {\n\n";

my $sep = ":";
print "class $class";
foreach my $parent (@parents) {
	print "$sep public $parent";
	$sep = ",";
}
print " {\n";



print <<"EOF";

public:
	$class();
	virtual ~$class();
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "$class::set(const XmlNode&)---> to be checked!...\\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "$class::set(const map<string, string&)---> to be checked!...\\n";
    }
    virtual $class* clone() const {
        MagLog::dev() << "$class::set(const map<string, string&)---> to be checked!...\\n";
        return new $class();
    }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	$class(const $class&);
    //! Overloaded << operator to copy - No copy allowed
	$class& operator=(const $class&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const $class& p)
		{ p.print(s); return s; }

};

template <>
class Translator<string, $class> { 
public:
	$class* operator()(const string& val )
	{
		return SimpleObjectMaker<$class>::create(val);
	}     

	$class* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
EOF
close STDOUT;

open STDOUT, ">src/$class.cc";
print <<"EOF";
/*! \\file $class.h
    \\brief Implementation of the Template class $class.
    
    Magics Team - ECMWF $year
    
    Started: $string
    
    Changes:
    
*/



#include "$class\.h"

using namespace magics;

$class\::$class() 
{
}


$class\::~$class() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void $class\::print(ostream& out)  const
{
	out << "$class\[";
	out << "]";
}

EOF

close STDOUT;
