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

/*! \file XmlTree.h
    \brief Implementation of the Template class XmlTree.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 10-Jun-2005
    
    Changes:
    
*/



#include "XmlTree.h"
#include "MagLog.h"

using namespace magics;

XmlTree::XmlTree() : root_("root")
{
}


XmlTree::~XmlTree() 
{
//	for (map<string, XmlNode*>::iterator id = ids_.begin(); id != ids_.end(); ++id) {
//		XmlNode* node = id->second;
//		id->second =0;
//		delete node;
//	}
}

/*!
 Class information are given to the output-stream.
*/		
void XmlTree::print(ostream& out)  const
{
	out << "XmlTree[";
	out << root_;
	out << "]";
}


XmlNode* XmlTree::getParent(const string& tag)
{
	if (tag == "magics_plot") return &root_;
	if (tag == "magics") return &root_;
	if (tag == "definition") return &definitions_;
	return 0; // Not special tag!
}

void XmlTree::endElement(const string& tag)
{
	if ( tag == "definition") {
		// we build the map of id ...
		XmlNode::ElementIterator def = definitions_.firstElement();
		while ((*def)->name() != "definition" ) def++;
		for (XmlNode::ElementIterator id = (*def)->firstElement(); id != (*def)->lastElement(); ++id ) {
				string keyword = (*id)->getAttribute("id");
				if ( keyword == "" ) 
					MagLog::warning() << "No if defined in group definition : " << (*id)->name() << "\n";
				else
					ids_.insert(make_pair(keyword, (*id)));
				
				
		} 
		
	}
}

XmlNode* XmlTree::newNode(const string& name, const map<string, string>& def) const
{
	// find in the attributes, if the keywird use_id is present
	map<string, string>::const_iterator use_id = def.find("use_id");
	if ( use_id == def.end() ) {
		return new XmlNode(name, def);
	}
	map<string, XmlNode*>::const_iterator id = ids_.find(use_id->second);
	if ( id == ids_.end() && definitions_.noElement() == false) {
		// We try to update the tree to see 
		
			XmlNode::ElementIterator elt = definitions_.firstElement();
			
			while ( (*elt)->name() != "definition" ) 
				elt++;
			for (XmlNode::ElementIterator i = (*elt)->firstElement(); i != (*elt)->lastElement(); ++i ) {
				string keyword = (*i)->getAttribute("id");
				if ( keyword == "" ) 
					MagLog::warning() << "No id defined in group definition : " << (*i)->name() << "\n";
				else
					ids_.insert(make_pair(keyword, (*i)));			
			}

		// we try again! 
		id = ids_.find(use_id->second); 
		if ( id == ids_.end() ) { 
			MagLog::warning() << "Could not find definition for " << use_id->second << "\n";
			return new XmlNode(name, def);
		}
	}
	return new XmlNode(*(id->second), def);
}

void XmlTree::definition(XmlNode* definition)
{
	definitions_.push_back(definition);
	XmlNode::ElementIterator elt = definitions_.firstElement();
				
	while ( (*elt)->name() != "definition" ) 
		elt++;
	for (XmlNode::ElementIterator i = (*elt)->firstElement(); i != (*elt)->lastElement(); ++i ) {
		string keyword = (*i)->getAttribute("id");
		if ( keyword == "" ) 
			MagLog::warning() << "No if defined in group definition : " << (*i)->name() << "\n";
		else
			ids_.insert(make_pair(keyword, (*i)));		
	}
}
