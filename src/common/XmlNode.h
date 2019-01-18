/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XmlNode.h
    \brief Definition of the Template class XmlNode.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 10-Jun-2005
    
    Changes:
    
*/

#ifndef XmlNode_H
#define XmlNode_H

#include "magics.h"
#include "VectorOfPointers.h"


namespace magics {
	
class XmlNode;
	
class XmlNodeVisitor
{
public:
	XmlNodeVisitor() {}
	virtual ~XmlNodeVisitor() {}
	virtual void visit(const XmlNode&) = 0;
	
};

class XmlNode {

public:
    typedef vector<XmlNode*>::const_iterator ElementIterator;
    typedef map<string, string>::const_iterator AttributesIterator;
    typedef vector<string>::const_iterator DataIterator;
    
	MAGICS_EXPORT XmlNode();
	MAGICS_EXPORT XmlNode(const string&);
	MAGICS_EXPORT XmlNode(const string&, const map<string, string>&);
	MAGICS_EXPORT XmlNode(const XmlNode&, const map<string, string>&);
	MAGICS_EXPORT XmlNode(const XmlNode&);
	
	MAGICS_EXPORT virtual ~XmlNode();
	void push_back(XmlNode* element) { elements_.push_back(element); }
	void setData(const string& data) { data_.push_back(data); }
	
	
	bool noElement() const { return elements_.empty(); }
	
	ElementIterator firstElement()  const      { return elements_.begin(); }
	ElementIterator lastElement() const        { return elements_.end(); }
	AttributesIterator firstAttributes() const { return attributes_.begin(); }
	AttributesIterator lastAttributes() const  { return attributes_.end(); }
	DataIterator   firstData() const           { return data_.begin(); }
	DataIterator   lastData() const            { return data_.end(); }
	const string& data() const              { static string e = ""; return data_.empty() ? e : *data_.begin(); }
	
	MAGICS_EXPORT string   getAttribute(const string& attr, const string& def = "") const;
	
	const string&   name() const                       { return name_; } 
	void   name(const string& name)                       { name_ = name; } 
	const map<string, string>&   attributes() const { return attributes_; }
	void addAttribute(const string& param, const string& value) { attributes_.insert(make_pair(param, value)); }
	
	MAGICS_EXPORT void  visit(XmlNodeVisitor&) const;
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 string                  name_;
	 vector<string>          data_;
	 VectorOfPointers<vector<XmlNode*> > elements_;
	 map<string, string>  attributes_;
	 void copy(const XmlNode&); 
private:
 
    //! Overloaded << operator to copy - No copy allowed
	XmlNode& operator=(const XmlNode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XmlNode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
