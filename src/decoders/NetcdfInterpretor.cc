/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfInterpretor.cc
    \brief Implementation of the Template class NetcdfInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/



#include "NetcdfInterpretor.h"
#include "NetcdfData.h"
#include "XmlReader.h"
#include <limits>

using namespace magics;

NetcdfInterpretor::NetcdfInterpretor() 
{
}

void NetcdfInterpretor::setDimensions(const stringarray& value, map<string, string>& first, map<string, string>& last)
{
    first.clear();
    last.clear();
 
    Tokenizer tokenizer("/ :");
    vector<string> tokens;
    for (stringarray::const_iterator val = value.begin(); val != value.end(); ++val) {
        tokens.clear();
        tokenizer(*val, tokens);
        
        switch (tokens.size()) 
        {
            case 2 : // param/from/from
                first[tokens[0]] = tokens[1];
                last[tokens[0]] =  tokens[1];
                break;
            case 3 : // param/from/to
                first[tokens[0]] =  tokens[1];
                last[tokens[0]] = tokens[2];
                break;
            case 1  : // param = all    
    			break;
            default : 
                throw MagicsException( "Syntax not Valid!:" + *val);
        }
          
    }

}
NetcdfInterpretor::~NetcdfInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void NetcdfInterpretor::print(ostream& out)  const
{
	out << "NetcdfInterpretor[";
    NetcdfInterpretorAttributes::print(out);
	out << "]";
}
/*
 * return the missing value
 */
double NetcdfInterpretor::missing(Netcdf& netcdf) const
{
	double missing = netcdf.getAttribute(missing_attribute_, std::numeric_limits<double>::max());
	return missing;
}

bool NetcdfInterpretor::reference_date(Netcdf& netcdf, const string& var, const string& refdate, string& basedate, vector<double>& coords, vector<double>& originals)
{
	static map<string, double> factors;
	if ( factors.empty() ) {
		factors["hours"] = 3600;
		factors["days"] = 24*3600;
	}

	double missing_value = netcdf.getMissing(var, missing_attribute_);
	string date = netcdf.getVariableAttribute(var, "reference_date", string(""));
	if ( date.empty() ) return false;
	originals.reserve(coords.size());
	for (vector<double>::iterator c = coords.begin(); c != coords.end(); ++c)
		 originals.push_back(*c);
	string units = netcdf.getVariableAttribute(var, "units", string(""));
	basedate = date;
	double diff = ( refdate.empty() ) ? 0 : DateTime(date) - DateTime(refdate) ;
	map<string, double>::const_iterator factor = factors.find(units);
	cout << "last point in days!" << coords.back() << endl;
	if ( factor != factors.end() )
		std::transform(coords.begin(), coords.end(),  coords.begin(), Multiply(factor->second, missing_value));
	std::transform(coords.begin(), coords.end(),  coords.begin(), Plus(diff, missing_value));
}

bool NetcdfInterpretor::cf_date(Netcdf& netcdf, const string& var, const string& refdate, string& basedate, vector<double>& coords, vector<double>& originals)
{
	//Step 1 : try to find a attribute long_name = time
	//Step 2 : Parse the attribute  units : days since date
	static map<string, double> factors;
	if ( factors.empty() ) {
		factors["hours"] = 3600;
		factors["days"] = 24*3600;

	}
	double missing_value = netcdf.getMissing(var, missing_attribute_);
	cout << " direct " << netcdf.getVariableAttribute(var, "long_name", string("")) << endl;
	string date = netcdf.getVariableAttribute(var, "long_name", string(""));
	cout << "var: " << var << "   date-->" << date << endl;
	if ( date.empty() ) return false;
	if ( date != "time" && date != "date and time") return false;

	string units = netcdf.getVariableAttribute(var, "units", string(""));
	if ( units.empty() ) return false;
	originals.reserve(coords.size());
	for (vector<double>::iterator c = coords.begin(); c != coords.end(); ++c)
		 originals.push_back(*c);

	// Now we parse the string !
	vector<string> tokens;
	Tokenizer tokenizer(" ");
	tokenizer(units, tokens);



	basedate = tokens[2];
	double diff;
	map<string, double>::const_iterator factor = factors.find(tokens[0]);
	if ( refdate.empty() ) {
		diff = Multiply(factor->second, missing_value)(coords.front());
		DateTime newref = DateTime(basedate) + Second(diff);
		basedate =  newref.tostring("%F %T");
		diff = -diff;
	}
	else {
		diff = DateTime(basedate) - DateTime(refdate) ;
	}


	if ( factor != factors.end() )
		std::transform(coords.begin(), coords.end(),  coords.begin(), Multiply(factor->second, missing_value));
	std::transform(coords.begin(), coords.end(),  coords.begin(), Plus(diff, missing_value));

}
string NetcdfInterpretor::getAttribute(const string& var, const string& attr, const string& def)
{
	Netcdf netcdf(path_, dimension_method_);
	if ( var.empty() )
		return netcdf.getAttribute(attr, def);
	return netcdf.getVariableAttribute(var, attr, def);
}

void NetcdfInterpretor::visit(TextVisitor& title)
{
	vector<string> titles;

	title.titles(titles);

	NetcdfTag tag(*this, title);
	for ( vector<string>::const_iterator t = titles.begin(); t != titles.end(); ++t ) {
		tag.decode(*t);
	}
	Netcdf netcdf(path_, dimension_method_);
	title.addAutomaticTitle(netcdf.getAttribute("title", string("NO TITLE")));
}

void NetcdfInterpretor::getAttributes(Netcdf& nc,const string& varName,string& keys,string& values)
{
	try
	{	
		NetVariable var=nc.getVariable(varName);
		bool first=true;
		for(map<string, NetAttribute>::iterator it=var.attributes_.begin(); it != var.attributes_.end(); it++)
		{
			string val;
			it->second.get(val);
			
			
			
		
			if(!first)
			{	
			  	keys+="/";
				values+="/";
			}
			first=false;
		
				keys+=it->first;
				values+=val;		
			}
	}
	catch ( ... ) {}
}


void NetcdfTag::visit(const XmlNode& node)
	{
		if ( magCompare(node.name(), "netcdf_info") )
		{
			string var = node.getAttribute("variable");

			string attr = node.getAttribute("attribute");
			string def = node.getAttribute("default");


			string val = netcdf_.getAttribute(var, attr, def);


			title_.update("netcdf"+var, attr,  val);
		}


		node.visit(*this);
	}

	void  NetcdfTag::decode(const string& line)
	{
		XmlReader parser;
		XmlTree tree;

		ostringstream xml;
		xml << "<?xml version='1.0' ?> \n";
		xml << "<xml> \n";
		xml << line;
		xml << "\n</xml>";

		try {
			parser.decode(xml.str(), &tree);
			tree.visit(*this);
		}
		catch (MagicsException& e) {
			MagLog::debug() << e.what() << endl;
		}
     }

