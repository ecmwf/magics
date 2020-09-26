
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file ContinuousLegendMethodAttributes.h
    \\brief Definition of ContinuousLegendMethod Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "ContinuousLegendMethodAttributes.h"
#include "MagicsParameter.h"
#include "ParameterSettings.h"

using namespace magics;

ContinuousLegendMethodAttributes::ContinuousLegendMethodAttributes():
	label_frequency_(ParameterManager::getInt("legend_label_frequency"))
	
	 
{
} 


ContinuousLegendMethodAttributes::~ContinuousLegendMethodAttributes()
{
	
}

    
void ContinuousLegendMethodAttributes::set(const std::map<string, string>& params)
{
	vector<string> prefix(1);
	int i = 0;
	prefix[i++] = "";
	
	setAttribute(prefix, "legend_label_frequency", label_frequency_, params);
	
	
}

void ContinuousLegendMethodAttributes::copy(const ContinuousLegendMethodAttributes& other)
{
	label_frequency_ = other.label_frequency_;
	
} 


bool ContinuousLegendMethodAttributes::accept(const string& node)
{	
	
	if ( magCompare(node, "")  )
		return true;
	
	return false;
}

void ContinuousLegendMethodAttributes::set(const XmlNode& node)
{
	bool apply = false;

	if ( this->accept(node.name()) == false ) 
		return;

	if ( magCompare(node.name(), "")  )
		apply = true;
	

	if ( apply )
		set(node.attributes());
	else {
		
	}
	for (auto &elt : node.elements())
	{
		
	}
}

void ContinuousLegendMethodAttributes::print(ostream& out)  const
{
	out << "Attributes[";
	out << " label_frequency = " <<  label_frequency_;
	
	out << "]" << "\n";
}

void ContinuousLegendMethodAttributes::toxml(ostream& out)  const
{
	out <<  "\"\""; 
	out << ", \"legend_label_frequency\":";
	niceprint(out,label_frequency_);
	
}

static MagicsParameter<int> legend_label_frequency("legend_label_frequency", 1, "");