/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file XmlNode.cc
    \brief Implementation of the Template class XmlNode.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 10-Jun-2005
    
    Changes:
    
*/

#include "XmlNode.h"

using namespace magics;

XmlNode::XmlNode() 
{
}

XmlNode::XmlNode(const string& name) : name_(name) 
{
}

XmlNode::XmlNode(const string& name, const map<string, string>& def) : 
	name_(name), attributes_(def)
{
}

XmlNode::XmlNode(const XmlNode& from) :
	name_(from.name_)
{
	copy(from);
}

void XmlNode::copy(const XmlNode& from) 
{
	name_ = from.name_;
	data_ = from.data_;
// First copy the attributes...
	// From the node..
	for ( map<string, string>::const_iterator attr = from.attributes_.begin(); attr != from.attributes_.end(); ++attr) {
		attributes_.insert(make_pair(attr->first, attr->second));
	}

	// Now copy the element...
	for (vector<XmlNode*>::const_iterator elt = from.elements_.begin(); elt != from.elements_.end(); ++elt) {
		elements_.push_back(new XmlNode(**elt));	
	}
}

XmlNode::XmlNode(const XmlNode& from, const map<string, string>& def) 
{
	copy(from);
	for ( map<string, string>::const_iterator attr = def.begin(); attr != def.end(); ++attr) {
		attributes_.insert(make_pair(attr->first, attr->second));
	}
}

XmlNode::~XmlNode() 
{
}

string  XmlNode::getAttribute(const string& attr, const string& def) const
{
	
	AttributesIterator value = attributes_.find(attr);
	return ( value != attributes_.end() ) ?  value->second : def;
}

/*!
 Class information are given to the output-stream.
*/		
void XmlNode::print(ostream& out)  const
{
	static int indent = -1;
	indent++;
	ostringstream tabs;
	for (int i = 0; i < indent; i++) tabs << "  ";
	string tab = tabs.str();
	out << tab << "<" << name_;
		
	for (map<string, string>::const_iterator attr = attributes_.begin(); attr != attributes_.end(); ++attr) {
		out << " " << attr->first << "='" << attr->second <<"'";	
	}
	out << ">\n";
	for (vector<string>::const_iterator data = data_.begin(); data != data_.end(); ++data) {
		out << *data << "\n";
	}
	for (vector<XmlNode*>::const_iterator elt = elements_.begin(); elt != elements_.end(); ++elt) {
		out << **elt;			
	}
	out << tab << "</" << name_ << ">\n";
	indent--;
}

void  XmlNode::visit(XmlNodeVisitor& visitor) const
{ 
	for (vector<XmlNode*>::const_iterator elt = elements_.begin(); elt != elements_.end(); ++elt) {
		visitor.visit(**elt);		
	}
}
