/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagConfig_H
#define MagConfig_H

#include "magics.h"



#include "json_spirit.h"
#include <limits>

namespace magics {
class MagConfig
{
public:
	MagConfig();
	~MagConfig();

	static string convert(const json_spirit::Value& value);

	virtual void callback(const string&, const json_spirit::Value&) = 0;
	virtual void callback(const json_spirit::Array&) {}

};


class MagConfigHandler

{
public:
	MagConfigHandler(const string& config, MagConfig& object);
	virtual ~MagConfigHandler();

	void dig(const json_spirit::Value&);
	void ignore(const json_spirit::Value&) {}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 


private:
    //! Copy constructor - No copy allowed
	MagConfigHandler(const MagConfigHandler&);
    //! Overloaded << operator to copy - No copy allowed
	MagConfigHandler& operator=(const MagConfigHandler&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagConfigHandler& p)
		{ p.print(s); return s; }

};

struct Style 
{
	typedef map<string, string> Definition;
	typedef void  (Style::*SetMethod)(const json_spirit::Value&);
	map<string,  SetMethod> methods_;
	
	void criteria(const json_spirit::Value&);
	void style(const json_spirit::Value&);
	void more(const json_spirit::Value&); 

	Definition criteria_;
	Definition style_;
	vector<Style> more_;
	void set(const json_spirit::Object&);
	bool find(const Definition& data, Definition& visdef);
};

class StyleLibrary : public MagConfig
{
public:
	StyleLibrary(const string& theme, const string& family): theme_(theme), family_(family) { init(); }
	StyleLibrary(const string& family): family_(family) { init(); }
	~StyleLibrary() {}

	void callback(const string& name, const json_spirit::Value& value);
	void callback(const json_spirit::Array& values);
	void init();
	
	vector<Style> library_;

	string theme_;
	string family_;

	bool find(const Style::Definition& data, Style::Definition& visdef);
    
    bool find(const string& name, Style::Definition& visdef) {
    	Style::Definition criteria;
    	criteria["name"] = name;
    	return find(criteria, visdef);
    }

	
	
};

struct Palette
{
	typedef vector<string> Definition;
	typedef void  (Palette::*SetMethod)(const json_spirit::Value&);

	map<string,  SetMethod> methods_;
	
	void values(const json_spirit::Value&);
	void tags(const json_spirit::Value&);
	

	Definition colours_;
	Definition tags_;
	string name_;
	void set(const json_spirit::Object&);

	
};


class PaletteLibrary : public MagConfig
{
public:
	PaletteLibrary() { init(); }
	PaletteLibrary(const string& family)  { init(); }
	~PaletteLibrary() {}

	void callback(const string& name, const json_spirit::Value& value);
	
	void init();
	
	map<string, Palette> library_;

    bool find(const string& name, Palette::Definition& colours) {
    	
    	map<string, Palette>::iterator palette = library_.find(name);

    	if ( palette != library_.end() ) {
    		colours = palette->second.colours_;
    		return true;
    	}
    	return false;
    }

	
	
};

class NetcdfGuess : public MagConfig
{
public:
	NetcdfGuess(const string& name): name_(name)   { init(); }
	NetcdfGuess(): name_("netcdf-convention") { init(); }
	~NetcdfGuess() {}

	void callback(const string& name, const json_spirit::Value& value);
	void init();

	string name_;

	map<string, map<string, vector<string> > > guess_;	
	const map<string, string>& get(const string& name) const;
	
};










} // namespace magics
#endif
