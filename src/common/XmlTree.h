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
    \brief Definition of the Template class XmlTree.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jun-2005

*/

#ifndef XmlTree_H
#define XmlTree_H

#include "magics.h"
#include "XmlNode.h"


namespace magics {

class XmlTree {

public:
	MAGICS_EXPORT XmlTree();
	MAGICS_EXPORT virtual ~XmlTree();
    XmlNode* getParent(const string&);
    void     endElement(const string&); 
    XmlNode* newNode(const string&, const map<string, string>& def) const;
    XmlNode::ElementIterator begin() const { return root_.elements().begin(); }
    XmlNode::ElementIterator end() const  { return root_.elements().end(); }
    void visit(XmlNodeVisitor& visitor) const { root_.visit(visitor); } 
    XmlNode* root()                           { return &root_; } 
    void definition(XmlNode* definition);    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable map<string, XmlNode*> ids_;
	 XmlNode                  root_;
	 XmlNode                  definitions_;

private:
    //! Copy constructor - No copy allowed
	XmlTree(const XmlTree&);
    //! Overloaded << operator to copy - No copy allowed
	XmlTree& operator=(const XmlTree&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XmlTree& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
