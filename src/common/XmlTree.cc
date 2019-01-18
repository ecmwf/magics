/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
		for (auto &elt : definitions_.elements())
			if (elt->name() == "definition")
			{
				for (auto &id : elt->elements())
				{
					string keyword = id->getAttribute("id");
					if ( keyword == "" )
						MagLog::warning() << "No if defined in group definition : " << id->name() << "\n";
					else
						ids_.insert(make_pair(keyword, id.get()));
				}
				break;
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
		for (auto &elt : definitions_.elements())
			if (elt->name() == "definition")
			{
				for (auto &i : elt->elements())
				{
					string keyword = i->getAttribute("id");
					if ( keyword == "" )
						MagLog::warning() << "No id defined in group definition : " << i->name() << "\n";
					else
						ids_.insert(make_pair(keyword, i.get()));
				}
				break;
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

	for (auto &elt : definitions_.elements())
		if (elt->name() == "definition")
		{
			for (auto &i : elt->elements())
			{
				string keyword = i->getAttribute("id");
				if ( keyword == "" )
					MagLog::warning() << "No if defined in group definition : " << i->name() << "\n";
				else
					ids_.insert(make_pair(keyword, i.get()));
			}
			break;
		}

}
