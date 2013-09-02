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
    
	XmlNode();
	XmlNode(const string&);
	XmlNode(const string&, const map<string, string>&);
	XmlNode(const XmlNode&, const map<string, string>&);
	XmlNode(const XmlNode&);
	
	virtual ~XmlNode();
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
	
	string   getAttribute(const string& attr, const string& def = "") const;
	
	const string&   name() const                       { return name_; } 
	void   name(const string& name)                       { name_ = name; } 
	const map<string, string>&   attributes() const { return attributes_; }
	void addAttribute(const string& param, const string& value) { attributes_.insert(make_pair(param, value)); }
	
	void  visit(XmlNodeVisitor&) const;
	
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
