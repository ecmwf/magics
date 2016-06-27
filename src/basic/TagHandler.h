/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TagHandler.h
    \brief Definition of the Template class TagHandler.
    
    Magics Team - ECMWF 2008
    
    Started: Mon 27-Oct-2008
    
    Changes:
    
*/

#ifndef TagHandler_H
#define TagHandler_H

#include "magics.h"
#include "XmlNode.h"
#include "Text.h"


namespace magics {

class TagHandler {

public:
	TagHandler();
	virtual ~TagHandler();
	void update(const string&, const string&, const string&);		
	string get(const string&, const string&);
    void reset() { definitions_.erase(definitions_.begin(), definitions_.end()); }
    virtual void addToTags(const string&, const string&) {}; 
    
    bool hasInfos() { return definitions_.empty() == false; }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 typedef map<string, std::set<string> > DefList;
	 map<string, DefList > definitions_;

	 
	 
	 
	 
private:
    //! Copy constructor - No copy allowed
	TagHandler(const TagHandler&);
    //! Overloaded << operator to copy - No copy allowed
	TagHandler& operator=(const TagHandler&);
	
	     
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TagHandler& p)
		{ p.print(s); return s; }

};

class TagConverter: public XmlNodeVisitor, public stack<NiceText>
{
public:
	TagConverter(TagHandler& owner);	
	~TagConverter();
	void visit(const XmlNode&);
	void ignore(const XmlNode&);
	void encoding(const string& encoding);
	void text(const XmlNode&);
	
	void grib(const XmlNode&);
	void netcdf(const XmlNode&);
	void spot(const XmlNode&);
	void pass(const XmlNode&);
	
	void font(const MagFont& font) { font_ = font; }
	void magics(const XmlNode&);
	void base_date(const XmlNode&);
	void json(const XmlNode&);
	void data(const XmlNode&);
	void font(const XmlNode&);
	void underline(const XmlNode&);
	void push();
	void bold(const XmlNode&);
	void superscript(const XmlNode&);
	void subscript(const XmlNode&);
	void check(const string&);
	void entities(ostream&);
	void decode(const string& line, Text*);
	const string& label() { return label_; }
	bool staticTag(const string&);

protected :
    TagHandler& owner_;
	typedef void (TagConverter::*ConvertFunction)(const XmlNode&);
	map<string, ConvertFunction> map_;
	MagFont       font_;
	TextElevation elevation_;
	string encoding_;
	Text* text_; 
	string label_;
	int automatic_;
	
};



} // namespace magics
#endif
